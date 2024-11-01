use crate::ast::*;
use crate::semantic_analysis::{Result, SemAnalysisError};

use std::collections::HashMap;
use std::iter::IntoIterator;
use std::sync::OnceLock;
use std::sync::RwLock;

// Right now every function in this module is gonna return Ok(input) without changing it.
// So whole type checking phase is not gonna change the AST.
// However, later on type checking phase WILL change the AST
// and that is why I am gonna stick to this redundancy:
// to make it easier to introduce changes later.

// Global symbol table
pub static SYM_TABLE: GlobalSymTable = GlobalSymTable::new();
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
        /*
        self.inner
            .get()
            .expect("Always initialized at this point")
            .keys()
            .map(String::as_str)
            .collect()
        */
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

// Current function return type which is required for
// return statment typechecking
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
        self.attrs.get_init()
    }
}

#[derive(Debug, Copy, Clone)]
pub enum InitValue {
    Tentative,
    Initial(StaticInit),
    NoInit,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum StaticInit {
    Int(i32),
    Long(i64),
}

impl InitValue {
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

    fn is_static(&self) -> bool {
        matches!(self, Self::Static { .. })
    }

    fn get_init(&self) -> Option<InitValue> {
        match self {
            Self::Static { init_val, .. } => Some(*init_val),
            _ => None,
        }
    }

    fn is_fun_defined(&self) -> bool {
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

    //    let args = args
    //        .into_iter()
    //        .map(|exp| typecheck_exp(exp, sym_table))
    //        .collect::<Result<Vec<Exp>>>()?;
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
    let rtype = match op {
        AstUnaryOp::LogicalNot => Type::Int,
        _ => typechecked_inner
            .get_type()
            .expect("Should always have type"),
    };
    Ok(typechecked_inner)
        .map(Box::new)
        .map(|exp| Exp::unary(op, exp).set_type(rtype))
}

fn typecheck_binary(op: AstBinaryOp, e1: Exp, e2: Exp, sym_table: &mut SymTable) -> Result<Exp> {
    let e1 = typecheck_exp(e1.into(), sym_table)?; //.map(Box::new)?;
    let e2 = typecheck_exp(e2.into(), sym_table)?; //.map(Box::new)?;

    if matches!(op, AstBinaryOp::LogicalAnd | AstBinaryOp::LogicalOr) {
        let e1 = Box::new(e1);
        let e2 = Box::new(e2);
        let binary_exp = Exp::binary(op, e1, e2);
        return Ok(binary_exp.set_type(Type::Int));
    }
    let t1 = e1.get_type().expect("Should always have type");
    let t2 = e2.get_type().expect("Should always have type");
    let common_type = Type::get_common(&t1, &t2);
    let converted_e1 = convert_to(e1, common_type.clone());
    let converted_e2 = convert_to(e2, common_type.clone());
    let binary = Exp::binary(op, Box::new(converted_e1), Box::new(converted_e2));

    let rtype = match op {
        // In case of shift resulting type is a type of left, not a common type
        o if o.is_shift() => t1,
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
    let then = typecheck_exp((*cond.then).into(), sym_table)?; //.map(Box::new)?;
    let els = typecheck_exp((*cond.els).into(), sym_table)?; //.map(Box::new)?;

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

fn typecheck_constant(constant: AstConst) -> Result<Exp> {
    match constant {
        c @ AstConst::Int(_) => Ok(Exp::constant(c).set_type(Type::Int)),
        c @ AstConst::Long(_) => Ok(Exp::constant(c).set_type(Type::Long)),
    }
}

fn typecheck_cast(t: Type, e: Exp, sym_table: &mut SymTable) -> Result<Exp> {
    let typed_inner = typecheck_exp(e.into(), sym_table).map(Box::new)?;
    let cast_exp = Exp::cast(t, typed_inner);
    Ok(cast_exp)
}

fn typecheck_exp(exp: UntypedExp, sym_table: &mut SymTable) -> Result<Exp> {
    match exp {
        UntypedExp::Cast(t, e) => typecheck_cast(t, *e, sym_table),
        UntypedExp::Assignment(e1, e2) => typecheck_assignment(*e1, *e2, sym_table),
        UntypedExp::Unary(op, exp) => typecheck_unary(op, *exp, sym_table),
        UntypedExp::Binary(op, src, dst) => typecheck_binary(op, *src, *dst, sym_table),
        UntypedExp::Conditional(cond) => typecheck_conditional(cond, sym_table),
        UntypedExp::Call(f, args) => typecheck_call(f, args, sym_table),
        UntypedExp::Var(name) => typecheck_var(name, sym_table),
        UntypedExp::Constant(c) => typecheck_constant(c),
    }
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

    let c = c.convert_to(t.clone());
    Ok(Exp::constant(c).set_type(t))
}
fn typecheck_cased_st(mut cased: CasedStatement, sym_table: &mut SymTable) -> Result<Statement> {
    cased.exp = typecheck_exp(cased.exp.into(), sym_table)?;
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

fn typecheck_fundec(fundec: FunDec, sym_table: &mut SymTable) -> Result<FunDec> {
    let fun_type = fundec.fun_type;
    let has_body = fundec.body.is_some();
    let mut already_defined = false;
    let is_static = fundec.storage_class.is_static();
    let mut global = !is_static;

    if let Some(old_dec) = sym_table.get(&fundec.name) {
        if old_dec.sym_type != fun_type {
            return Err(SemAnalysisError::IncompatibleFunDec(fundec.name));
        }
        already_defined = old_dec.attrs.is_fun_defined();
        if has_body && already_defined {
            return Err(SemAnalysisError::FunctionRedefinition(fundec.name));
        }

        if old_dec.attrs.is_global() && is_static {
            return Err(SemAnalysisError::StaticFunctionRedeclaredNonStatic(
                fundec.name.clone(),
            ));
        }
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
        let mut cn = CURRENT_RTYPE.write().expect("Should not be poisoned");
        *cn = fun_type.get_rtype().unwrap().clone();
        let ptypes = fun_type.get_ptypes().expect("Should always be Some");
        for (param, ptype) in fundec.params.iter().zip(ptypes.iter()) {
            let attrs = IdAttr::Local;
            let entry = SymTableEntry {
                sym_type: ptype.clone(),
                attrs,
            };
            sym_table.insert(param.clone(), entry);
        }
    }

    let typechecked_body = fundec
        .body
        .map(|block| typecheck_block(block, sym_table))
        .transpose()?;
    let typechecked = FunDec {
        name: fundec.name,
        body: typechecked_body,
        params: fundec.params,
        storage_class: fundec.storage_class,
        fun_type,
    };

    Ok(typechecked)
}

fn typecheck_vardec(vardec: VarDec, sym_table: &mut SymTable) -> Result<VarDec> {
    let mut initial_value = InitValue::NoInit;
    if vardec.storage_class.is_extern() {
        if vardec.init.is_some() {
            return Err(SemAnalysisError::InitOnExternVar(vardec.name.clone()));
        }
        if let Some(old_dec) = sym_table.get(&vardec.name) {
            dbg!(&old_dec.sym_type, &vardec.var_type);
            if old_dec.sym_type != vardec.var_type {
                return Err(SemAnalysisError::IdentifierRedeclaration(
                    vardec.name.clone(),
                ));
            }
        } else {
            let entry = SymTableEntry {
                sym_type: vardec.var_type.clone(),
                attrs: IdAttr::Static {
                    init_val: InitValue::NoInit,
                    global: true,
                },
            };
            sym_table.insert(vardec.name.clone(), entry);
        }
    } else if vardec.storage_class.is_static() {
        if vardec.init.is_some() {
            initial_value = get_static_init(vardec.init.as_ref().cloned().unwrap())?;
        }
        let entry = SymTableEntry {
            sym_type: vardec.var_type.clone(),
            attrs: IdAttr::Static {
                init_val: initial_value,
                global: false,
            },
        };
        sym_table.insert(vardec.name.clone(), entry);
    } else {
        let entry = SymTableEntry {
            sym_type: vardec.var_type.clone(),
            attrs: IdAttr::Local,
        };
        sym_table.insert(vardec.name.clone(), entry);

        if let Some(init) = vardec.init.clone() {
            typecheck_exp(init.into(), sym_table)?;
        }
    }
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

fn get_static_init(init: Exp) -> Result<InitValue> {
    if let UntypedExp::Constant(init) = init.into() {
        match init {
            AstConst::Int(i) => Ok(i).map(StaticInit::Int),
            AstConst::Long(i) => Ok(i).map(StaticInit::Long),
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

    let mut init_val = InitValue::NoInit;
    if let Some(init_exp) = vardec.init.clone() {
        init_val = get_static_init(init_exp)?;
    } else if !vardec.storage_class.is_extern() {
        init_val = InitValue::Tentative;
    }

    let mut global = !vardec.storage_class.is_static();

    if let Some(old_dec) = sym_table.get(&vardec.name) {
        if old_dec.sym_type != vardec.var_type {
            return Err(FunRedefErr(vardec.name.clone()));
        }
        if vardec.storage_class.is_extern() {
            global = old_dec.attrs.is_global();
        } else if old_dec.attrs.is_global() != global {
            return Err(LinkConfErr(vardec.name.clone()));
        }

        if old_dec.attrs.is_const_init() {
            if matches!(init_val, InitValue::Initial(_)) {
                return Err(IdRedecErr(vardec.name.clone()));
            }
            init_val = old_dec.attrs.get_init().unwrap();
        } else if !init_val.is_const() && old_dec.attrs.get_init().unwrap().is_tentative() {
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
        .collect::<Result<Vec<_>>>()?;
    let ast = Ast { declarations };
    SYM_TABLE.init(sym_table);

    Ok(ast)
}
