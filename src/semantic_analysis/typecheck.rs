use crate::ast::*;
use crate::semantic_analysis::{Result, SemAnalysisError};

use std::collections::HashMap;
use std::iter::IntoIterator;
use std::sync::OnceLock;

// Global read-only symbol table
pub static SYM_TABLE: GlobalSymTable = GlobalSymTable::new();
pub struct GlobalSymTable {
    inner: OnceLock<SymTable>,
}

impl GlobalSymTable {
    const fn new() -> Self {
        Self {
            inner: OnceLock::new(),
        }
    }

    fn init(&self, table: SymTable) {
        self.inner.set(table).expect("Should only be called once");
    }

    pub fn get_keys(&self) -> Vec<&str> {
        self.inner
            .get()
            .expect("Always initialized at this point")
            .keys()
            .map(String::as_str)
            .collect()
    }

    pub fn get_symbol(&self, sym: &str) -> Option<&SymTableEntry> {
        self.inner.get().and_then(|st| st.get(sym))
    }

    pub fn is_sym_static(&self, sym: &str) -> bool {
        self.inner
            .get()
            .and_then(|st| st.get(sym))
            .filter(|s| s.attrs.is_static())
            .is_some()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SymTableEntry {
    pub sym_type: Type,
    pub attrs: IdAttr,
}

impl SymTableEntry {
    pub fn is_global(&self) -> bool {
        self.attrs.is_global()
    }

    pub fn get_init(&self) -> Option<i64> {
        self.attrs.get_init().and_then(|iv| iv.get_tacky_init())
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum InitValue {
    Tentative,
    Initial(i64),
    NoInit,
}

impl InitValue {
    pub fn is_const(&self) -> bool {
        matches!(self, Self::Initial(_))
    }

    pub fn get_tacky_init(&self) -> Option<i64> {
        match self {
            Self::Initial(i) => Some(*i),
            Self::Tentative => Some(0),
            Self::NoInit => None,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
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

// Right now every function in this module is gonna return Ok(input) without changing it.
// So whole type checking phase is not gonna change the AST.
// However, later on type checking phase WILL change the AST
// and that is why I am gonna stick to this redundancy:
// to make it easier to introduce changes later.

fn typecheck_call(name: Identifier, args: Vec<Exp>, sym_table: &mut SymTable) -> Result<Exp> {
    let f_type = sym_table
        .get(&name)
        .ok_or(SemAnalysisError::UndeclaredFunction(name.clone()))?;
    if let Type::Fun { nargs } = f_type.sym_type {
        if args.len() != nargs {
            return Err(SemAnalysisError::ExpectedArgsCountButGot(
                nargs,
                args.len(),
                name.clone(),
            ));
        }
    } else {
        return Err(SemAnalysisError::VariableCall(name));
    }
    let args = args
        .into_iter()
        .map(|exp| typecheck_exp(exp, sym_table))
        .collect::<Result<Vec<Exp>>>()?;

    Ok(Exp::Call(name, args))
}

fn typecheck_var(name: Identifier, sym_table: &mut SymTable) -> Result<Exp> {
    let entry = sym_table
        .get(&name)
        .ok_or(SemAnalysisError::VariableNotDeclared(name.clone()))?;
    if entry.sym_type != Type::Int {
        return Err(SemAnalysisError::FunctionNameAsVariable(name.clone()));
    }

    Ok(Exp::Var(name))
}

fn typecheck_assignment(e1: Exp, e2: Exp, sym_table: &mut SymTable) -> Result<Exp> {
    let e1 = typecheck_exp(e1, sym_table).map(Box::new)?;
    let e2 = typecheck_exp(e2, sym_table).map(Box::new)?;

    Ok(Exp::Assignment(e1, e2))
}

fn typecheck_unary(op: AstUnaryOp, exp: Exp, sym_table: &mut SymTable) -> Result<Exp> {
    typecheck_exp(exp, sym_table)
        .map(Box::new)
        .map(|exp| Exp::Unary(op, exp))
}

fn typecheck_binary(op: AstBinaryOp, e1: Exp, e2: Exp, sym_table: &mut SymTable) -> Result<Exp> {
    let e1 = typecheck_exp(e1, sym_table).map(Box::new)?;
    let e2 = typecheck_exp(e2, sym_table).map(Box::new)?;

    Ok(Exp::Binary(op, e1, e2))
}

fn typecheck_conditional(mut cond: ConditionalExp, sym_table: &mut SymTable) -> Result<Exp> {
    cond.condition = typecheck_exp(*cond.condition, sym_table).map(Box::new)?;
    cond.then = typecheck_exp(*cond.then, sym_table).map(Box::new)?;
    cond.els = typecheck_exp(*cond.els, sym_table).map(Box::new)?;
    Ok(Exp::Conditional(cond))
}

fn typecheck_exp(exp: Exp, sym_table: &mut SymTable) -> Result<Exp> {
    match exp {
        Exp::Assignment(e1, e2) => typecheck_assignment(*e1, *e2, sym_table),
        Exp::Unary(op, exp) => typecheck_unary(op, *exp, sym_table),
        Exp::Binary(op, src, dst) => typecheck_binary(op, *src, *dst, sym_table),
        Exp::Conditional(cond) => typecheck_conditional(cond, sym_table),
        Exp::Call(f, args) => typecheck_call(f, args, sym_table),
        Exp::Var(name) => typecheck_var(name, sym_table),
        Exp::Constant(_) => Ok(exp),
    }
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
        S::Compound(block) => typecheck_block(block, sym_table).map(S::Compound).map(Ok)?,
        S::Return(exp) => typecheck_exp(exp, sym_table).map(S::Return).map(Ok)?,
        S::Exp(exp) => typecheck_exp(exp, sym_table).map(S::Exp).map(Ok)?,
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

fn typecheck_cased_st(mut cased: CasedStatement, sym_table: &mut SymTable) -> Result<Statement> {
    cased.body = typecheck_statement(*cased.body, sym_table).map(Box::new)?;
    Ok(Statement::Cased(cased))
}

fn typecheck_for_st(mut for_st: For, sym_table: &mut SymTable) -> Result<Statement> {
    for_st.init = typecheck_forinit(for_st.init, sym_table)?;
    for_st.condition = for_st
        .condition
        .map(|exp| typecheck_exp(exp, sym_table))
        .transpose()?;
    for_st.post = for_st
        .post
        .map(|exp| typecheck_exp(exp, sym_table))
        .transpose()?;
    for_st.body = typecheck_statement(*for_st.body, sym_table).map(Box::new)?;

    Ok(Statement::For(for_st))
}

fn typecheck_forinit(init: AstForInit, sym_table: &mut SymTable) -> Result<AstForInit> {
    match init {
        AstForInit::InitDecl(vardec) => {
            if vardec.storage_class.is_some() {
                return Err(SemAnalysisError::StorageIdInForInit(vardec.name.clone()));
            }
            typecheck_vardec(vardec, sym_table).map(AstForInit::InitDecl)
        }
        AstForInit::InitExp(exp) => exp
            .map(|exp| typecheck_exp(exp, sym_table))
            .transpose()
            .map(AstForInit::InitExp),
    }
}

fn typecheck_if_st(mut if_st: If, sym_table: &mut SymTable) -> Result<Statement> {
    if_st.condition = typecheck_exp(if_st.condition, sym_table)?;
    if_st.then = typecheck_statement(*if_st.then, sym_table).map(Box::new)?;
    if_st.els = if_st
        .els
        .map(|bst| typecheck_statement(*bst, sym_table))
        .transpose()?
        .map(Box::new);
    Ok(Statement::If(if_st))
}

fn typecheck_switch(mut switch: Switch, sym_table: &mut SymTable) -> Result<Statement> {
    switch.ctrl_exp = typecheck_exp(switch.ctrl_exp, sym_table)?;
    switch.body = typecheck_statement(*switch.body, sym_table).map(Box::new)?;
    Ok(Statement::Switch(switch))
}

fn typecheck_dowhile(mut dowhile: DoWhile, sym_table: &mut SymTable) -> Result<Statement> {
    dowhile.condition = typecheck_exp(dowhile.condition, sym_table)?;
    dowhile.body = typecheck_statement(*dowhile.body, sym_table).map(Box::new)?;
    Ok(Statement::DoWhile(dowhile))
}

fn typecheck_while_st(mut while_st: While, sym_table: &mut SymTable) -> Result<Statement> {
    while_st.condition = typecheck_exp(while_st.condition, sym_table)?;
    while_st.body = typecheck_statement(*while_st.body, sym_table).map(Box::new)?;
    Ok(Statement::While(while_st))
}

fn typecheck_fundec(fundec: FunDec, sym_table: &mut SymTable) -> Result<FunDec> {
    let fun_type = Type::Fun {
        nargs: fundec.params.len(),
    };
    let has_body = fundec.body.is_some();
    let mut already_defined = false;
    let mut global = fundec.storage_class != Some(StorageClass::Static);

    if let Some(old_dec) = sym_table.get(&fundec.name) {
        if old_dec.sym_type != fun_type {
            return Err(SemAnalysisError::IncompatibleFunDec(fundec.name));
        }
        already_defined = old_dec.attrs.is_fun_defined();
        if has_body && already_defined {
            return Err(SemAnalysisError::FunctionRedefinition(fundec.name));
        }

        if old_dec.attrs.is_global() && fundec.storage_class == Some(StorageClass::Static) {
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
        sym_type: fun_type,
        attrs,
    };

    sym_table.insert(fundec.name.clone(), entry);

    if has_body {
        for param in &fundec.params {
            let attrs = IdAttr::Local;
            let entry = SymTableEntry {
                sym_type: Type::Int,
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
    };

    Ok(typechecked)
}

fn typecheck_vardec(vardec: VarDec, sym_table: &mut SymTable) -> Result<VarDec> {
    let mut initial_value = InitValue::Initial(0);
    if vardec.storage_class == Some(StorageClass::Extern) {
        if vardec.init.is_some() {
            return Err(SemAnalysisError::InitOnExternVar(vardec.name.clone()));
        }
        if let Some(old_dec) = sym_table.get(&vardec.name) {
            if old_dec.sym_type != Type::Int {
                return Err(SemAnalysisError::IdentifierRedeclaration(
                    vardec.name.clone(),
                ));
            }
        } else {
            let entry = SymTableEntry {
                sym_type: Type::Int,
                attrs: IdAttr::Static {
                    init_val: InitValue::NoInit,
                    global: true,
                },
            };
            sym_table.insert(vardec.name.clone(), entry);
        }
    } else if vardec.storage_class == Some(StorageClass::Static) {
        if vardec.init.is_some() {
            initial_value = get_const_init(vardec.init.as_ref().unwrap())?;
        }
        let entry = SymTableEntry {
            sym_type: Type::Int,
            attrs: IdAttr::Static {
                init_val: initial_value,
                global: false,
            },
        };
        sym_table.insert(vardec.name.clone(), entry);
    } else {
        let entry = SymTableEntry {
            sym_type: Type::Int,
            attrs: IdAttr::Local,
        };
        sym_table.insert(vardec.name.clone(), entry);

        if let Some(init) = vardec.init.clone() {
            typecheck_exp(init, sym_table)?;
        }
    }
    Ok(vardec)
}

fn typecheck_declaration(dec: Declaration, sym_table: &mut SymTable) -> Result<Declaration> {
    match dec {
        Declaration::Var(vardec) => typecheck_vardec(vardec, sym_table).map(Declaration::Var),
        Declaration::Fun(fundec) => typecheck_fundec(fundec, sym_table).map(Declaration::Fun),
    }
}

fn typecheck_block_item(item: AstBlockItem, sym_table: &mut SymTable) -> Result<AstBlockItem> {
    match item {
        AstBlockItem::D(dec) => typecheck_declaration(dec, sym_table).map(AstBlockItem::D),
        AstBlockItem::S(st) => typecheck_statement(st, sym_table).map(AstBlockItem::S),
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

fn get_const_init(init: &Exp) -> Result<InitValue> {
    if let Exp::Constant(i) = init {
        Ok(InitValue::Initial(*i as i64))
    } else {
        Err(SemAnalysisError::NonConstantInit(String::new()))
    }
}

pub fn typecheck_toplevel_vardec(vardec: VarDec, sym_table: &mut SymTable) -> Result<VarDec> {
    let mut init_val = InitValue::NoInit;
    if let Some(init_exp) = vardec.init.clone() {
        init_val = get_const_init(&init_exp)?;
    } else if vardec.storage_class != Some(StorageClass::Extern) {
        init_val = InitValue::Tentative;
    }

    let mut global = !matches!(vardec.storage_class, Some(StorageClass::Static));

    if let Some(old_dec) = sym_table.get(&vardec.name) {
        if old_dec.sym_type != Type::Int {
            return Err(SemAnalysisError::FunctionRedefinition(vardec.name.clone()));
        }
        if vardec.storage_class == Some(StorageClass::Extern) {
            global = old_dec.attrs.is_global();
        } else if old_dec.attrs.is_global() != global {
            return Err(SemAnalysisError::ConflictingLinkage(vardec.name.clone()));
        }

        if old_dec.attrs.is_const_init() {
            if matches!(init_val, InitValue::Initial(_)) {
                return Err(SemAnalysisError::IdentifierRedeclaration(
                    vardec.name.clone(),
                ));
            }
            init_val = old_dec.attrs.get_init().unwrap();
        } else if !init_val.is_const() && old_dec.attrs.get_init().unwrap() == InitValue::Tentative
        {
            init_val = InitValue::Tentative;
        }
    }
    let attrs = IdAttr::Static { init_val, global };
    let entry = SymTableEntry {
        sym_type: Type::Int,
        attrs,
    };
    sym_table.insert(vardec.name.clone(), entry);
    Ok(vardec)
}

pub fn typecheck_toplevel_dec(dec: Declaration, sym_table: &mut SymTable) -> Result<Declaration> {
    match dec {
        Declaration::Fun(fundec) => typecheck_fundec(fundec, sym_table).map(Declaration::Fun),
        Declaration::Var(vardec) => {
            typecheck_toplevel_vardec(vardec, sym_table).map(Declaration::Var)
        }
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
