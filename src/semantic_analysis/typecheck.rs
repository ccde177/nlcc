use crate::ast::*;
use crate::semantic_analysis::{Result, SemAnalysisError};

use std::collections::HashMap;

#[derive(Debug, Clone, Eq, PartialEq)]
enum Type {
    Int,
    Fun { nargs: usize },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SymTableEntry {
    sym_type: Type,
    defined: bool,
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

    if let Some(old_dec) = sym_table.get(&fundec.name) {
        if old_dec.sym_type != fun_type {
            return Err(SemAnalysisError::IncompatibleFunDec(fundec.name));
        }
        already_defined = old_dec.defined;
        if has_body && already_defined {
            return Err(SemAnalysisError::FunctionRedefinition(fundec.name));
        }
    }

    let entry = SymTableEntry {
        sym_type: fun_type,
        defined: already_defined || has_body,
    };

    sym_table.insert(fundec.name.clone(), entry);

    if has_body {
        for param in &fundec.params {
            let entry = SymTableEntry {
                sym_type: Type::Int,
                defined: true,
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
    };

    Ok(typechecked)
}

fn typecheck_vardec(vardec: VarDec, sym_table: &mut SymTable) -> Result<VarDec> {
    let entry = SymTableEntry {
        sym_type: Type::Int,
        defined: true,
    };
    sym_table.insert(vardec.name.clone(), entry);
    let init = vardec
        .init
        .map(|exp| typecheck_exp(exp, sym_table))
        .transpose()?;
    Ok(VarDec {
        name: vardec.name,
        init,
    })
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

pub fn check_types(ast: Ast) -> Result<(Ast, SymTable)> {
    let mut sym_table = SymTable::new();
    let Ast { functions } = ast;

    let functions = functions
        .into_iter()
        .map(|fundec| typecheck_fundec(fundec, &mut sym_table))
        .collect::<Result<Vec<_>>>()?;

    let ast = Ast { functions };

    Ok((ast, sym_table))
}