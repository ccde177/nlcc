use crate::ast::*;
use crate::semantic_analysis::{Result, SemAnalysisError};
use std::sync::atomic::{AtomicUsize, Ordering};

use std::collections::HashMap;

static GLOBAL_COUNTER: AtomicUsize = AtomicUsize::new(0);
struct NameGenerator(&'static AtomicUsize);

impl NameGenerator {
    #[inline]
    fn get_instance() -> Self {
        Self(&GLOBAL_COUNTER)
    }

    #[inline]
    fn generate_name(&self, before: &Identifier) -> Identifier {
        let v = self.0.fetch_add(1, Ordering::AcqRel);
        format!("{before}..{v}")
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct MapEntry {
    pub name: Identifier,
    pub in_current_scope: bool,
    pub has_linkage: bool,
}

impl MapEntry {
    fn leave_scope(mut self) -> Self {
        self.in_current_scope = false;
        self
    }
}

struct IdentifierMap {
    inner: HashMap<Identifier, MapEntry>,
}

impl IdentifierMap {
    fn new() -> Self {
        let inner = HashMap::new();
        Self { inner }
    }

    fn add(&mut self, name: Identifier, entry: MapEntry) {
        self.inner.insert(name, entry);
    }

    fn get(&self, name: &Identifier) -> Option<&MapEntry> {
        self.inner.get(name)
    }

    fn get_uniq_name(&self, name: &Identifier) -> Option<Identifier> {
        self.inner.get(name).map(|entry| entry.name.clone())
    }

    fn is_in_current_scope(&self, name: &Identifier) -> bool {
        self.inner
            .get(name)
            .filter(|mv| mv.in_current_scope)
            .is_some()
    }

    fn add_uniq_to_scope(&mut self, name: Identifier) -> Identifier {
        let ng = NameGenerator::get_instance();
        let generated = ng.generate_name(&name);
        let entry = MapEntry {
            name: generated.clone(),
            in_current_scope: true,
            has_linkage: false,
        };
        self.inner.insert(name, entry);
        generated
    }

    fn new_scope_copy(other: &Self) -> Self {
        let im = other
            .inner
            .clone()
            .into_iter()
            .map(|(name, entry)| (name, entry.leave_scope()))
            .collect();

        Self { inner: im }
    }
}

fn resolve_forinit(init: AstForInit, im: &mut IdentifierMap) -> Result<AstForInit> {
    match init {
        AstForInit::InitDecl(dec) => resolve_vardec(dec, im).map(AstForInit::InitDecl),
        AstForInit::InitExp(exp) => resolve_optional_exp(exp, im).map(AstForInit::InitExp),
    }
}

fn resolve_optional_exp(exp: Option<Exp>, im: &mut IdentifierMap) -> Result<Option<Exp>> {
    exp.map_or(Ok(None), |exp| resolve_exp(exp, im).map(Some))
}

fn resolve_switch(mut switch: Switch, im: &mut IdentifierMap) -> Result<Statement> {
    switch.ctrl_exp = resolve_exp(switch.ctrl_exp, im)?;
    switch.body = resolve_statement(*switch.body, im).map(Box::new)?;
    Ok(Statement::Switch(switch))
}

fn resolve_case(mut cased: CasedStatement, im: &mut IdentifierMap) -> Result<Statement> {
    //Do not resolve exp since it is gonna be checked in another semantical analysis pass
    cased.body = resolve_statement(*cased.body, im).map(Box::new)?;
    Ok(Statement::Cased(cased))
}

fn resolve_dcase(mut dcased: DCasedStatement, im: &mut IdentifierMap) -> Result<Statement> {
    dcased.body = resolve_statement(*dcased.body, im).map(Box::new)?;
    Ok(Statement::DCased(dcased))
}

fn resolve_while_st(mut while_st: While, im: &mut IdentifierMap) -> Result<Statement> {
    while_st.condition = resolve_exp(while_st.condition, im)?;
    while_st.body = resolve_statement(*while_st.body, im).map(Box::new)?;
    Ok(Statement::While(while_st))
}

fn resolve_for_st(mut for_st: For, im: &mut IdentifierMap) -> Result<Statement> {
    let mut new_im = IdentifierMap::new_scope_copy(im);
    for_st.init = resolve_forinit(for_st.init, &mut new_im)?;
    for_st.condition = resolve_optional_exp(for_st.condition, &mut new_im)?;
    for_st.post = resolve_optional_exp(for_st.post, &mut new_im)?;
    for_st.body = resolve_statement(*for_st.body, &mut new_im).map(Box::new)?;

    Ok(Statement::For(for_st))
}

fn resolve_dowhile(mut dowhile: DoWhile, im: &mut IdentifierMap) -> Result<Statement> {
    dowhile.condition = resolve_exp(dowhile.condition, im)?;
    dowhile.body = resolve_statement(*dowhile.body, im).map(Box::new)?;
    Ok(Statement::DoWhile(dowhile))
}

fn resolve_if_st(mut if_st: If, im: &mut IdentifierMap) -> Result<Statement> {
    if_st.condition = resolve_exp(if_st.condition, im)?;
    if_st.then = resolve_statement(*if_st.then, im).map(Box::new)?;
    if_st.els = if_st.els.map_or(Ok(None), |bs| {
        resolve_statement(*bs, im).map(Box::new).map(Some)
    })?;
    Ok(Statement::If(if_st))
}

fn resolve_compound_st(block: AstBlock, im: &mut IdentifierMap) -> Result<Statement> {
    let mut new_im = IdentifierMap::new_scope_copy(im);
    let block = resolve_block(block, &mut new_im)?;
    Ok(Statement::Compound(block))
}

fn resolve_labeled_st(
    label: Identifier,
    st: Statement,
    im: &mut IdentifierMap,
) -> Result<Statement> {
    let statement = resolve_statement(st, im).map(Box::new)?;
    Ok(Statement::Labeled(label, statement))
}

fn resolve_statement(st: Statement, im: &mut IdentifierMap) -> Result<Statement> {
    use Statement as S;
    match st {
        S::Switch(switch) => resolve_switch(switch, im),
        S::While(while_st) => resolve_while_st(while_st, im),
        S::DoWhile(dowhile) => resolve_dowhile(dowhile, im),
        S::For(for_st) => resolve_for_st(for_st, im),
        S::If(if_st) => resolve_if_st(if_st, im),
        S::Cased(cs) => resolve_case(cs, im),
        S::DCased(dcs) => resolve_dcase(dcs, im),
        S::Compound(block) => resolve_compound_st(block, im),
        S::Labeled(label, statement) => resolve_labeled_st(label, *statement, im),
        S::Return(e) => resolve_exp(e, im).map(S::Return),
        S::Exp(e) => resolve_exp(e, im).map(S::Exp),
        _ => Ok(st),
    }
}

fn resolve_exp_call(name: Identifier, args: Vec<Exp>, im: &mut IdentifierMap) -> Result<Exp> {
    let new_name = im
        .get_uniq_name(&name)
        .ok_or(SemAnalysisError::UndeclaredFunction(name))?;
    let args = args.into_iter()
        .map(|arg| resolve_exp(arg, im))
        .collect::<Result<Vec<_>>>()?;
    
    Ok(Exp::Call(new_name, args))
}

fn resolve_exp_conditional(mut cond_exp: ConditionalExp, im: &mut IdentifierMap) -> Result<Exp> {
    cond_exp.condition = resolve_exp(*cond_exp.condition, im).map(Box::new)?;
    cond_exp.then = resolve_exp(*cond_exp.then, im).map(Box::new)?;
    cond_exp.els = resolve_exp(*cond_exp.els, im).map(Box::new)?;
    Ok(Exp::Conditional(cond_exp))
}

fn resolve_exp_assign(left: Exp, right: Exp, im: &mut IdentifierMap) -> Result<Exp> {
    if !left.is_var() {
        return Err(SemAnalysisError::WrongLvalue(left));
    }
    let left = resolve_exp(left, im).map(Box::new)?;
    let right = resolve_exp(right, im).map(Box::new)?;
    Ok(Exp::Assignment(left, right))
}

fn resolve_exp_incdec(op: AstUnaryOp, e: Exp, im: &mut IdentifierMap) -> Result<Exp> {
    let exp = resolve_exp(e, im)?;
    if exp.is_var() {
        Ok(Exp::Unary(op, Box::new(exp)))
    } else {
        Err(SemAnalysisError::WrongLvalue(exp))
    }
}

fn resolve_exp_var(name: Identifier, im: &mut IdentifierMap) -> Result<Exp> {
    im.get_uniq_name(&name)
        .ok_or(SemAnalysisError::VariableNotDeclared(name))
        .map(Exp::Var)
}

fn resolve_exp_unary(op: AstUnaryOp, exp: Exp, im: &mut IdentifierMap) -> Result<Exp> {
    let exp = resolve_exp(exp, im).map(Box::new)?;
    Ok(Exp::Unary(op, exp))
}

fn resolve_exp_binary(
    op: AstBinaryOp,
    src: Exp,
    dst: Exp,
    im: &mut IdentifierMap,
) -> Result<Exp> {
    let src = resolve_exp(src, im).map(Box::new)?;
    let dst = resolve_exp(dst, im).map(Box::new)?;
    Ok(Exp::Binary(op, src, dst))
}

fn resolve_exp(exp: Exp, im: &mut IdentifierMap) -> Result<Exp> {
    match exp {
        Exp::Unary(
            op @ (AstUnaryOp::PostfixIncrement
            | AstUnaryOp::PrefixIncrement
            | AstUnaryOp::PostfixDecrement
            | AstUnaryOp::PrefixDecrement),
            e,
        ) => resolve_exp_incdec(op, *e, im),
        Exp::Unary(op, exp) => resolve_exp_unary(op, *exp, im),
        Exp::Conditional(cond_exp) => resolve_exp_conditional(cond_exp, im),
        Exp::Assignment(left, right) => resolve_exp_assign(*left, *right, im),
        Exp::Var(name) => resolve_exp_var(name, im),
        Exp::Call(name, args) => resolve_exp_call(name, args, im),
        Exp::Binary(op, src, dst) => resolve_exp_binary(op, *src, *dst, im),
        Exp::Constant(_) => Ok(exp),
    }
}

fn resolve_fundec(dec: FunDec, im: &mut IdentifierMap) -> Result<FunDec> {
    if let Some(prev_entry) = im.get(&dec.name) {
        if prev_entry.in_current_scope && !prev_entry.has_linkage {
            let duplicate = dec.name.clone();
            return Err(SemAnalysisError::DuplicateDeclaration(duplicate));
        }
    }

    im.add(
        dec.name.clone(),
        MapEntry {
            name: dec.name.clone(),
            in_current_scope: true,
            has_linkage: true,
        },
    );

    let mut inner_map = IdentifierMap::new_scope_copy(im);

    let mut new_params = Vec::with_capacity(dec.params.len());
    for param in dec.params {
        let resolved = resolve_param(param, &mut inner_map)?;
        new_params.push(resolved);
    }

    let mut new_body = None;
    if let Some(body) = dec.body {
        let resolved = resolve_block(body, &mut inner_map)?;
        new_body = Some(resolved);
    }

    Ok(FunDec {
        name: dec.name,
        params: new_params,
        body: new_body,
    })
}

fn resolve_param(param: Identifier, im: &mut IdentifierMap) -> Result<Identifier> {
    if im.is_in_current_scope(&param) {
        return Err(SemAnalysisError::IdentifierRedeclaration(param));
    }
    let uniq_name = im.add_uniq_to_scope(param);

    Ok(uniq_name)
}

fn resolve_vardec(dec: VarDec, im: &mut IdentifierMap) -> Result<VarDec> {
    if im.is_in_current_scope(&dec.name) {
        return Err(SemAnalysisError::IdentifierRedeclaration(dec.name.clone()));
    }

    let uniq_name = im.add_uniq_to_scope(dec.name);

    let mut exp = None;
    if let Some(e) = dec.init {
        exp = Some(resolve_exp(e, im)?);
    }

    Ok(VarDec {
        name: uniq_name,
        init: exp,
    })
}

fn resolve_local_declaration(dec: Declaration, im: &mut IdentifierMap) -> Result<Declaration> {
    match dec {
        Declaration::Var(vardec) => {
            let resolved = resolve_vardec(vardec, im)?;
            Ok(Declaration::Var(resolved))
        }
        Declaration::Fun(fundec) => {
            if fundec.body.is_some() {
                return Err(SemAnalysisError::LocalFunDefinition(fundec.name));
            }
            let resolved = resolve_fundec(fundec, im)?;
            Ok(Declaration::Fun(resolved))
        }
    }
}

fn resolve_block_item(item: AstBlockItem, im: &mut IdentifierMap) -> Result<AstBlockItem> {
    match item {
        AstBlockItem::S(statement) => {
            let statement = resolve_statement(statement, im)?;
            Ok(AstBlockItem::S(statement))
        }
        AstBlockItem::D(declaration) => {
            let declaration = resolve_local_declaration(declaration, im)?;
            Ok(AstBlockItem::D(declaration))
        }
    }
}

fn resolve_block(block: AstBlock, im: &mut IdentifierMap) -> Result<AstBlock> {
    let mut result = AstBlockItems::new();
    for item in block.items {
        let resolved_item = resolve_block_item(item, im)?;
        result.push(resolved_item);
    }

    Ok(AstBlock { items: result })
}

// Even if this function seems pointless right now,
// later it will take Declaration as first parameter
fn resolve_global_declaration(dec: FunDec, im: &mut IdentifierMap) -> Result<FunDec> {
    resolve_fundec(dec, im)
}

pub fn name_resolution(ast: Ast) -> Result<Ast> {
    let Ast { functions } = ast;
    let mut resolved_decs = Vec::new();
    let mut im = IdentifierMap::new();
    for dec in functions {
        let resolved = resolve_global_declaration(dec, &mut im)?;
        resolved_decs.push(resolved);
    }

    Ok(Ast {
        functions: resolved_decs,
    })
}
