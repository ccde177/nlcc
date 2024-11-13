use crate::ast::*;
use crate::semantic_analysis::{Result, SemAnalysisError};

use std::collections::HashMap;
use std::iter::IntoIterator;
use std::sync::OnceLock;
use std::sync::RwLock;

// Global symbol table
pub static SYM_TABLE: GlobalSymTable = GlobalSymTable::new();
#[derive(Debug)]
pub struct GlobalSymTable {
    inner: OnceLock<RwLock<SymTable>>,
}

impl GlobalSymTable {
    const fn new() -> Self {
        Self {
            inner: OnceLock::new(),
        }
    }

    fn init(&self, table: SymTable) {
        self.inner
            .set(RwLock::new(table))
            .expect("Should only be called once");
    }

    pub fn add_local_sym(&self, name: Identifier, sym_type: Type) {
        let attrs = IdAttr::Local;
        let entry = SymTableEntry { sym_type, attrs };
        self.inner
            .get()
            .expect("Should be initialized by this point")
            .write()
            .expect("Should not be poisoned")
            .insert(name, entry);
    }

    pub fn get_keys(&self) -> Vec<String> {
        let inner = self
            .inner
            .get()
            .expect("Always initialized at this point")
            .read()
            .expect("Should not be poisoned");
        inner.keys().cloned().collect()
    }

    pub fn get_type(&self, sym: &str) -> Option<Type> {
        self.inner
            .get()
            .expect("Should be initialized")
            .read()
            .expect("Should not be poisoned")
            .get(sym)
            .map(|sym| sym.sym_type.clone())
    }

    pub fn get_symbol(&self, sym: &str) -> Option<SymTableEntry> {
        self.inner
            .get()
            .expect("Should be initialized by this point")
            .read()
            .expect("Should not be poisoned")
            .get(sym)
            .cloned()
    }

    pub fn is_sym_static(&self, sym: &str) -> bool {
        self.inner
            .get()
            .expect("Should be initialized by this point")
            .read()
            .expect("Should not be poisoned")
            .get(sym)
            .filter(|s| s.attrs.is_static())
            .is_some()
    }
}

// Current function return type is required for
// typechecking return statment
static CURRENT_RTYPE: RwLock<Type> = RwLock::new(Type::Int);
// Current Switch statement controlling expression type
// is required for casting case expressions
static CTRL_TYPE: RwLock<Type> = RwLock::new(Type::Int);

#[derive(Debug, Clone)]
pub struct SymTableEntry {
    pub sym_type: Type,
    pub attrs: IdAttr,
}

impl SymTableEntry {
    pub fn is_global(&self) -> bool {
        self.attrs.is_global()
    }
    pub fn get_init(&self) -> Option<InitValue> {
        self.attrs.get_init().filter(InitValue::is_not_noinit)
    }
}

#[derive(Debug, Copy, Clone)]
pub enum InitValue {
    Tentative,
    Initial(StaticInit),
    NoInit,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum StaticInit {
    Int(i32),
    Long(i64),
    UInt(u32),
    ULong(u64),
    Double(f64),
}

impl From<AstConst> for StaticInit {
    fn from(value: AstConst) -> Self {
        use AstConst as C;
        match value {
            C::Int(i) => Self::Int(i),
            C::UInt(u) => Self::UInt(u),
            C::Long(i) => Self::Long(i),
            C::ULong(u) => Self::ULong(u),
            C::Double(f) => Self::Double(f),
        }
    }
}

impl StaticInit {
    pub fn is_zero(&self) -> bool {
        matches!(
            self,
            Self::Int(0) | Self::Long(0) | Self::UInt(0) | Self::ULong(0)
        )
    }
    pub fn is_double(&self) -> bool {
        matches!(&self, Self::Double(_))
    }

    pub fn is_long(&self) -> bool {
        matches!(self, Self::Long(_) | Self::ULong(_))
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Self::Int(_) | Self::UInt(_))
    }
}

impl InitValue {
    #[inline]
    pub fn is_not_noinit(&self) -> bool {
        !self.is_noinit()
    }
    #[inline]
    pub fn is_noinit(&self) -> bool {
        matches!(self, Self::NoInit)
    }
    #[inline]
    pub fn is_const(&self) -> bool {
        matches!(self, Self::Initial(_))
    }
    #[inline]
    pub fn is_tentative(&self) -> bool {
        matches!(self, Self::Tentative)
    }

    pub fn get_static_init(&self) -> Option<StaticInit> {
        match self {
            Self::Initial(si) => Some(*si),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum IdAttr {
    Fun { defined: bool, global: bool },
    Static { init_val: InitValue, global: bool },
    Local,
}

impl IdAttr {
    fn is_const_init(&self) -> bool {
        match self {
            Self::Static { init_val, .. } => init_val.is_const(),
            _ => false,
        }
    }

    pub fn is_tentative_init(&self) -> bool {
        match self {
            Self::Static { init_val, .. } => init_val.is_tentative(),
            _ => false,
        }
    }

    pub fn is_static(&self) -> bool {
        matches!(self, Self::Static { .. })
    }

    fn get_init(&self) -> Option<InitValue> {
        match self {
            Self::Static { init_val, .. } => Some(*init_val),
            _ => None,
        }
    }

    pub fn is_fun_defined(&self) -> bool {
        match self {
            Self::Fun { defined, .. } => *defined,
            _ => false,
        }
    }

    pub fn is_global(&self) -> bool {
        match self {
            Self::Fun { global, .. } | Self::Static { global, .. } => *global,
            Self::Local => false,
        }
    }
}

pub type SymTable = HashMap<Identifier, SymTableEntry>;

fn typecheck_call(name: Identifier, args: Vec<Exp>, sym_table: &mut SymTable) -> Result<Exp> {
    use SemAnalysisError::ExpectedArgsCountButGot as ArgsCountErr;
    use SemAnalysisError::UndeclaredFunction as UnknownFunErr;
    use SemAnalysisError::VariableCall as VarCallErr;

    let entry = sym_table.get(&name).ok_or(UnknownFunErr(name.clone()))?;
    let len = args.len();
    let (ptypes, rtype) = match &entry.sym_type {
        Type::Fun {
            ptypes,
            return_type,
        } if ptypes.len() == len => Ok((ptypes.clone(), return_type.clone())),
        Type::Fun { ptypes, .. } => Err(ArgsCountErr(ptypes.len(), len, name.clone())),
        _ => Err(VarCallErr(name.clone())),
    }?;

    let mut converted_args = Vec::new();

    for (ptype, arg) in ptypes.into_iter().zip(args.into_iter()) {
        let typed_arg = typecheck_exp(arg.into(), sym_table)?;
        let converted = convert_to(typed_arg, ptype);
        converted_args.push(converted);
    }

    let call_exp = Exp::call(name, converted_args).set_type(*rtype);
    Ok(call_exp)
}

fn typecheck_var(name: Identifier, sym_table: &mut SymTable) -> Result<Exp> {
    use SemAnalysisError::FunctionNameAsVariable as FunAsVarErr;
    use SemAnalysisError::VariableNotDeclared as UnknownVarErr;

    let entry = sym_table.get(&name).ok_or(UnknownVarErr(name.clone()))?;
    let sym_type = entry.sym_type.clone();

    if sym_type.is_function() {
        return Err(FunAsVarErr(name.clone()));
    }

    Ok(Exp::var(name).set_type(sym_type))
}

fn typecheck_assignment(e1: Exp, e2: Exp, sym_table: &mut SymTable) -> Result<Exp> {
    let e1 = typecheck_exp(e1.into(), sym_table).map(Box::new)?;
    let e2 = typecheck_exp(e2.into(), sym_table)?;
    let left_type = e1.get_type().expect("Should always have type");
    let converted_right = Box::new(convert_to(e2, left_type.clone()));
    let assign = Exp::assignment(e1, converted_right).set_type(left_type);
    Ok(assign)
}

fn typecheck_unary(op: AstUnaryOp, exp: Exp, sym_table: &mut SymTable) -> Result<Exp> {
    let typechecked_inner = typecheck_exp(exp.into(), sym_table)?;
    let inner_type = typechecked_inner
        .get_type()
        .expect("Should have type after type checking");
    if matches!(op, AstUnaryOp::Complement) && matches!(inner_type, Type::Double) {
        return Err(SemAnalysisError::ComplementOfFloat);
    }
    let is_not = matches!(op, AstUnaryOp::LogicalNot);

    let rtype = if is_not {
        Type::Int
    } else {
        typechecked_inner
            .get_type()
            .expect("Should always have type")
    };

    let typechecked_inner = Box::new(typechecked_inner);
    let result = Exp::unary(op, typechecked_inner).set_type(rtype);
    Ok(result)
}

fn typecheck_binary(op: AstBinaryOp, e1: Exp, e2: Exp, sym_table: &mut SymTable) -> Result<Exp> {
    let e1 = typecheck_exp(e1.into(), sym_table)?;
    let e2 = typecheck_exp(e2.into(), sym_table)?;
    let t1 = e1.get_type().expect("Should have type after type checking");
    let t2 = e2.get_type().expect("Should have type after type checking");

    if (op.is_bitwise() || op.is_mod()) && (t1.is_double() || t2.is_double()) {
        return Err(SemAnalysisError::IllegalOperationOnFloat);
    }

    let common_type = Type::get_common(&t1, &t2);
    let converted_e1 = convert_to(e1, common_type.clone());
    let converted_e2 = convert_to(e2, common_type.clone());
    let binary = Exp::binary(op, Box::new(converted_e1), Box::new(converted_e2));

    let rtype = match op {
        // Equality check result is Int(bool{0, 1})
        o if o.is_eq() => Type::Int,
        _ => common_type,
    };

    Ok(binary).map(|e| e.set_type(rtype))
}

fn convert_to(e: Exp, t: Type) -> Exp {
    if e.get_type().filter(|it| it == &t).is_some() {
        return e;
    }

    let cast_exp = Exp::cast(t.clone(), Box::new(e));
    cast_exp.set_type(t)
}

fn typecheck_conditional(cond: ConditionalExp, sym_table: &mut SymTable) -> Result<Exp> {
    let condition = typecheck_exp((*cond.condition).into(), sym_table).map(Box::new)?;
    let then = typecheck_exp((*cond.then).into(), sym_table)?;
    let els = typecheck_exp((*cond.els).into(), sym_table)?;

    let then_type = then.get_type().expect("Should always have type");
    let els_type = els.get_type().expect("Should always have type");
    let common_type = Type::get_common(&then_type, &els_type);
    let converted_then = Box::new(convert_to(then, common_type.clone()));
    let converted_els = Box::new(convert_to(els, common_type.clone()));
    let cond = ConditionalExp {
        condition,
        then: converted_then,
        els: converted_els,
    };

    Ok(Exp::conditional(cond).set_type(common_type))
}

fn typecheck_constant(constant: AstConst) -> Exp {
    match constant {
        AstConst::Int(_) => Exp::constant(constant).set_type(Type::Int),
        AstConst::Long(_) => Exp::constant(constant).set_type(Type::Long),
        AstConst::UInt(_) => Exp::constant(constant).set_type(Type::UInt),
        AstConst::ULong(_) => Exp::constant(constant).set_type(Type::ULong),
        AstConst::Double(_) => Exp::constant(constant).set_type(Type::Double),
    }
}

fn typecheck_cast(t: Type, e: Exp, sym_table: &mut SymTable) -> Result<Exp> {
    let typed_inner = typecheck_exp(e.into(), sym_table).map(Box::new)?;
    let cast_exp = Exp::cast(t.clone(), typed_inner).set_type(t);
    Ok(cast_exp)
}

fn typecheck_exp(exp: UntypedExp, sym_table: &mut SymTable) -> Result<Exp> {
    use UntypedExp as UE;
    match exp {
        UE::Cast(t, e) => typecheck_cast(t, *e, sym_table),
        UE::Assignment(e1, e2) => typecheck_assignment(*e1, *e2, sym_table),
        UE::Unary(op, exp) => typecheck_unary(op, *exp, sym_table),
        UE::Binary(op, src, dst) if op.is_shift() => typecheck_shift(op, *src, *dst, sym_table),
        UE::Binary(op, src, dst) if op.is_logical() => typecheck_logical(op, *src, *dst, sym_table),
        UE::Binary(op, src, dst) => typecheck_binary(op, *src, *dst, sym_table),
        UE::Conditional(cond) => typecheck_conditional(cond, sym_table),
        UE::Call(f, args) => typecheck_call(f, args, sym_table),
        UE::Var(name) => typecheck_var(name, sym_table),
        UE::Constant(c) => Ok(typecheck_constant(c)),
    }
}

fn typecheck_logical(op: AstBinaryOp, src: Exp, dst: Exp, sym_table: &mut SymTable) -> Result<Exp> {
    let src = typecheck_exp(src.into(), sym_table).map(Box::new)?;
    let dst = typecheck_exp(dst.into(), sym_table).map(Box::new)?;
    let binary_exp = Exp::binary(op, src, dst);
    return Ok(binary_exp.set_type(Type::Int));
}

fn typecheck_shift(op: AstBinaryOp, src: Exp, dst: Exp, sym_table: &mut SymTable) -> Result<Exp> {
    let src = typecheck_exp(src.into(), sym_table).map(Box::new)?;
    let dst = typecheck_exp(dst.into(), sym_table).map(Box::new)?;
    let src_is_double = src.get_type() == Some(Type::Double);
    let dst_is_double = dst.get_type() == Some(Type::Double);
    if src_is_double || dst_is_double {
        return Err(SemAnalysisError::IllegalOperationOnFloat);
    }
    let src_type = src
        .get_type()
        .expect("Should have type after type checking");

    let binary_exp = Exp::binary(op, src, dst);
    return Ok(binary_exp.set_type(src_type));
}

fn typecheck_return(e: Exp, sym_table: &mut SymTable) -> Result<Statement> {
    let typechecked_inner = typecheck_exp(e.into(), sym_table)?;
    let current_type = CURRENT_RTYPE
        .read()
        .expect("Should not be poisoned")
        .clone();
    let converted = convert_to(typechecked_inner, current_type);
    Ok(Statement::Return(converted))
}

fn typecheck_statement(st: Statement, sym_table: &mut SymTable) -> Result<Statement> {
    use Statement as S;
    match st {
        S::While(while_st) => typecheck_while_st(while_st, sym_table),
        S::DoWhile(dowhile) => typecheck_dowhile(dowhile, sym_table),
        S::Switch(switch) => typecheck_switch(switch, sym_table),
        S::If(if_st) => typecheck_if_st(if_st, sym_table),
        S::For(for_st) => typecheck_for_st(for_st, sym_table),
        S::Cased(cased) => typecheck_cased_st(cased, sym_table),
        S::DCased(dcased) => typecheck_dcased_st(dcased, sym_table),
        S::Labeled(name, st) => typecheck_labeled_st(name, *st, sym_table),
        S::Compound(block) => typecheck_block(block, sym_table).map(S::Compound),
        S::Return(exp) => typecheck_return(exp, sym_table),
        S::Exp(exp) => typecheck_exp(exp.into(), sym_table).map(S::Exp),
        S::Goto(_) | S::Continue(_) | S::Break(_) | S::Null => Ok(st),
    }
}

fn typecheck_labeled_st(
    name: String,
    st: Statement,
    sym_table: &mut SymTable,
) -> Result<Statement> {
    typecheck_statement(st, sym_table)
        .map(Box::new)
        .map(|bst| Statement::Labeled(name, bst))
}

fn typecheck_dcased_st(mut dcased: DCasedStatement, sym_table: &mut SymTable) -> Result<Statement> {
    dcased.body = typecheck_statement(*dcased.body, sym_table).map(Box::new)?;
    Ok(Statement::DCased(dcased))
}

fn convert_case(e: Exp, t: Type) -> Result<Exp> {
    let inner_t = e.get_type().expect("Should have type after type checking");
    let exp = e.clone().into();
    if inner_t == t {
        return Ok(e);
    }
    let c = match exp {
        UntypedExp::Constant(c) => Ok(c),
        _ => Err(SemAnalysisError::NotAConstCase(e)),
    }?;

    let c = c.convert_to(&t);
    Ok(Exp::constant(c).set_type(t))
}
fn typecheck_cased_st(mut cased: CasedStatement, sym_table: &mut SymTable) -> Result<Statement> {
    cased.exp = typecheck_exp(cased.exp.into(), sym_table)?;
    if let Some(Type::Double) = cased.exp.get_type() {
        return Err(SemAnalysisError::NotAConstCase(cased.exp));
    }
    let ctrl_type = CTRL_TYPE.read().expect("Should not be poisoned").clone();
    cased.exp = convert_case(cased.exp, ctrl_type)?;
    cased.body = typecheck_statement(*cased.body, sym_table).map(Box::new)?;
    Ok(Statement::Cased(cased))
}

fn typecheck_for_st(mut for_st: For, sym_table: &mut SymTable) -> Result<Statement> {
    for_st.init = typecheck_forinit(for_st.init, sym_table)?;
    for_st.condition = for_st
        .condition
        .map(|exp| typecheck_exp(exp.into(), sym_table))
        .transpose()?;
    for_st.post = for_st
        .post
        .map(|exp| typecheck_exp(exp.into(), sym_table))
        .transpose()?;
    for_st.body = typecheck_statement(*for_st.body, sym_table).map(Box::new)?;

    Ok(Statement::For(for_st))
}

fn typecheck_forinit(init: AstForInit, sym_table: &mut SymTable) -> Result<AstForInit> {
    use AstForInit as FI;
    use SemAnalysisError::StorageIdInForInit as FIWrongSClassErr;

    match init {
        FI::InitDecl(vardec) if !vardec.storage_class.is_auto() => {
            Err(FIWrongSClassErr(vardec.name))
        }
        FI::InitDecl(vardec) => typecheck_vardec(vardec, sym_table).map(FI::InitDecl),
        FI::InitExp(exp) => exp
            .map(|exp| typecheck_exp(exp.into(), sym_table))
            .transpose()
            .map(FI::InitExp),
    }
}

fn typecheck_if_st(mut if_st: If, sym_table: &mut SymTable) -> Result<Statement> {
    if_st.condition = typecheck_exp(if_st.condition.into(), sym_table)?;
    if_st.then = typecheck_statement(*if_st.then, sym_table).map(Box::new)?;
    if_st.els = if_st
        .els
        .map(|bst| typecheck_statement(*bst, sym_table))
        .transpose()?
        .map(Box::new);
    Ok(Statement::If(if_st))
}

fn typecheck_switch(mut switch: Switch, sym_table: &mut SymTable) -> Result<Statement> {
    switch.ctrl_exp = typecheck_exp(switch.ctrl_exp.into(), sym_table)?;
    // Save old type for cases when switch is nested
    let old_type = CTRL_TYPE.read().expect("Should not be poisoned").clone();
    let ctrl_type = switch
        .ctrl_exp
        .get_type()
        .expect("Should have type after typecheck");
    // Set new controlling type for cases
    *CTRL_TYPE.write().unwrap() = ctrl_type;
    switch.body = typecheck_statement(*switch.body, sym_table).map(Box::new)?;
    // Restore old type
    *CTRL_TYPE.write().unwrap() = old_type;
    Ok(Statement::Switch(switch))
}

fn typecheck_dowhile(mut dowhile: DoWhile, sym_table: &mut SymTable) -> Result<Statement> {
    dowhile.condition = typecheck_exp(dowhile.condition.into(), sym_table)?;
    dowhile.body = typecheck_statement(*dowhile.body, sym_table).map(Box::new)?;
    Ok(Statement::DoWhile(dowhile))
}

fn typecheck_while_st(mut while_st: While, sym_table: &mut SymTable) -> Result<Statement> {
    while_st.condition = typecheck_exp(while_st.condition.into(), sym_table)?;
    while_st.body = typecheck_statement(*while_st.body, sym_table).map(Box::new)?;
    Ok(Statement::While(while_st))
}

fn check_fundec_compatible(fundec: &FunDec, old_dec: &SymTableEntry) -> Result<()> {
    let fun_type = &fundec.fun_type;
    let is_static = fundec.storage_class.is_static();
    let has_body = fundec.body.is_some();
    if &old_dec.sym_type != fun_type {
        return Err(SemAnalysisError::IncompatibleFunDec(fundec.name.clone()));
    }
    let already_defined = old_dec.attrs.is_fun_defined();
    if has_body && already_defined {
        return Err(SemAnalysisError::FunctionRedefinition(fundec.name.clone()));
    }

    if old_dec.attrs.is_global() && is_static {
        return Err(SemAnalysisError::StaticFunctionRedeclaredNonStatic(
            fundec.name.clone(),
        ));
    }

    Ok(())
}

fn typecheck_params(
    params: &[Identifier],
    fun_type: &Type,
    sym_table: &mut SymTable,
) -> Result<()> {
    let mut cn = CURRENT_RTYPE.write().expect("Should not be poisoned");
    *cn = fun_type
        .get_rtype()
        .expect("Should have return type since it is a function")
        .clone();

    let ptypes = fun_type.get_ptypes().expect("Should always be Some");
    for (param, ptype) in params.iter().zip(ptypes.iter()) {
        let attrs = IdAttr::Local;
        let entry = SymTableEntry {
            sym_type: ptype.clone(),
            attrs,
        };
        sym_table.insert(param.clone(), entry);
    }
    Ok(())
}

fn typecheck_fundec(mut fundec: FunDec, sym_table: &mut SymTable) -> Result<FunDec> {
    let has_body = fundec.body.is_some();
    let fun_type = &fundec.fun_type;
    let mut already_defined = false;
    let is_static = fundec.storage_class.is_static();
    let mut global = !is_static;

    if let Some(old_dec) = sym_table.get(&fundec.name) {
        check_fundec_compatible(&fundec, old_dec)?;
        already_defined = old_dec.attrs.is_fun_defined();
        global = old_dec.attrs.is_global();
    }

    let attrs = IdAttr::Fun {
        defined: already_defined || has_body,
        global,
    };

    let entry = SymTableEntry {
        sym_type: fun_type.clone(),
        attrs,
    };

    sym_table.insert(fundec.name.clone(), entry);

    if has_body {
        typecheck_params(&fundec.params, &fundec.fun_type, sym_table)?;
    }

    fundec.body = fundec
        .body
        .map(|block| typecheck_block(block, sym_table))
        .transpose()?;

    Ok(fundec)
}

fn typecheck_vardec_extern(vardec: VarDec, sym_table: &mut SymTable) -> Result<VarDec> {
    if vardec.init.is_some() {
        return Err(SemAnalysisError::InitOnExternVar(vardec.name.clone()));
    }

    let old_dec = sym_table
        .entry(vardec.name.clone())
        .or_insert_with(|| SymTableEntry {
            sym_type: vardec.var_type.clone(),
            attrs: IdAttr::Static {
                init_val: InitValue::NoInit,
                global: true,
            },
        });

    if old_dec.sym_type != vardec.var_type {
        return Err(SemAnalysisError::IdentifierRedeclaration(vardec.name));
    }

    Ok(vardec)
}

fn typecheck_vardec_auto(mut vardec: VarDec, sym_table: &mut SymTable) -> Result<VarDec> {
    let entry = SymTableEntry {
        sym_type: vardec.var_type.clone(),
        attrs: IdAttr::Local,
    };
    sym_table.insert(vardec.name.clone(), entry);

    if let Some(init) = vardec.init.clone() {
        vardec.init = typecheck_exp(init.into(), sym_table)
            .map(|e| convert_to(e, vardec.var_type.clone()))
            .map(Some)?;
    }

    Ok(vardec)
}

fn typecheck_vardec(vardec: VarDec, sym_table: &mut SymTable) -> Result<VarDec> {
    match vardec.storage_class {
        StorageClass::Static => typecheck_vardec_static(vardec, sym_table),
        StorageClass::Extern => typecheck_vardec_extern(vardec, sym_table),
        StorageClass::Auto => typecheck_vardec_auto(vardec, sym_table),
    }
}

fn typecheck_vardec_static(vardec: VarDec, sym_table: &mut SymTable) -> Result<VarDec> {
    let initial_value = if vardec.init.is_some() {
        let init = vardec.init.clone().unwrap();
        get_static_init(init, &vardec.var_type)?
    } else {
        InitValue::Tentative
    };
    let entry = SymTableEntry {
        sym_type: vardec.var_type.clone(),
        attrs: IdAttr::Static {
            init_val: initial_value,
            global: false,
        },
    };
    sym_table.insert(vardec.name.clone(), entry);
    Ok(vardec)
}

fn typecheck_declaration(dec: Declaration, sym_table: &mut SymTable) -> Result<Declaration> {
    use Declaration as D;
    match dec {
        D::Var(vardec) => typecheck_vardec(vardec, sym_table).map(D::Var),
        D::Fun(fundec) => typecheck_fundec(fundec, sym_table).map(D::Fun),
    }
}

fn typecheck_block_item(item: AstBlockItem, sym_table: &mut SymTable) -> Result<AstBlockItem> {
    use AstBlockItem as ABI;
    match item {
        ABI::D(dec) => typecheck_declaration(dec, sym_table).map(ABI::D),
        ABI::S(st) => typecheck_statement(st, sym_table).map(ABI::S),
    }
}

fn typecheck_block(block: AstBlock, sym_table: &mut SymTable) -> Result<AstBlock> {
    let AstBlock { items } = block;
    let items = items
        .into_iter()
        .map(|bi| typecheck_block_item(bi, sym_table))
        .collect::<Result<Vec<_>>>()?;

    Ok(AstBlock { items })
}

fn get_static_init(init: Exp, t: &Type) -> Result<InitValue> {
    if let UntypedExp::Constant(init) = init.into() {
        match init.convert_to(&t) {
            AstConst::Int(i) => Ok(StaticInit::Int(i)),
            AstConst::Long(i) => Ok(StaticInit::Long(i)),
            AstConst::UInt(u) => Ok(StaticInit::UInt(u)),
            AstConst::ULong(u) => Ok(StaticInit::ULong(u)),
            AstConst::Double(f) => Ok(StaticInit::Double(f)),
        }
        .map(InitValue::Initial)
    } else {
        Err(SemAnalysisError::NonConstantInit(String::new()))
    }
}

pub fn typecheck_toplevel_vardec(vardec: VarDec, sym_table: &mut SymTable) -> Result<VarDec> {
    use SemAnalysisError::ConflictingLinkage as LinkConfErr;
    use SemAnalysisError::FunctionRedefinition as FunRedefErr;
    use SemAnalysisError::IdentifierRedeclaration as IdRedecErr;

    let is_extern = vardec.storage_class.is_extern();
    let mut init_val = InitValue::NoInit;
    if let Some(init_exp) = vardec.init.clone() {
        init_val = get_static_init(init_exp, &vardec.var_type)?;
    } else if !is_extern {
        init_val = InitValue::Tentative;
    }

    let mut global = !vardec.storage_class.is_static();

    if let Some(old_dec) = sym_table.get(&vardec.name) {
        if old_dec.sym_type != vardec.var_type {
            return Err(FunRedefErr(vardec.name.clone()));
        }
        let is_old_global = old_dec.attrs.is_global();
        if is_extern {
            global = old_dec.attrs.is_global();
        } else if is_old_global != global {
            return Err(LinkConfErr(vardec.name.clone()));
        }

        if old_dec.attrs.is_const_init() {
            if matches!(init_val, InitValue::Initial(_)) {
                return Err(IdRedecErr(vardec.name.clone()));
            }
            init_val = old_dec.attrs.get_init().unwrap();
        } else if !init_val.is_const() && old_dec.attrs.is_tentative_init() {
            init_val = InitValue::Tentative;
        }
    }

    let attrs = IdAttr::Static { init_val, global };
    let entry = SymTableEntry {
        sym_type: vardec.var_type.clone(),
        attrs,
    };
    sym_table.insert(vardec.name.clone(), entry);
    Ok(vardec)
}

pub fn typecheck_toplevel_dec(dec: Declaration, sym_table: &mut SymTable) -> Result<Declaration> {
    use Declaration as D;
    match dec {
        D::Fun(fundec) => typecheck_fundec(fundec, sym_table).map(D::Fun),
        D::Var(vardec) => typecheck_toplevel_vardec(vardec, sym_table).map(D::Var),
    }
}

pub fn check_types(ast: Ast) -> Result<Ast> {
    let mut sym_table = SymTable::new();
    let Ast { declarations } = ast;

    let declarations = declarations
        .into_iter()
        .map(|dec| typecheck_toplevel_dec(dec, &mut sym_table))
        .collect::<Result<_>>()?;
    let ast = Ast { declarations };
    SYM_TABLE.init(sym_table);

    Ok(ast)
}
