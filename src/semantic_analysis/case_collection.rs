use super::{Result, SemAnalysisError};
use crate::ast::*;

use std::collections::HashSet;

type Cases = HashSet<(Option<u64>, Identifier)>;

fn collect_labeled_st(label: String, st: Statement) -> Result<(Statement, Cases)> {
    let (body, cases) = collect_statement(st)?;
    let body = Box::new(body);
    let result = Statement::Labeled(label, body);

    Ok((result, cases))
}

fn collect_dcased(dcased: DCasedStatement) -> Result<(Statement, Cases)> {
    let DCasedStatement { body, label } = dcased;
    let (body, mut cases) = collect_statement(*body)?;
    let result_case = (None, label.clone());

    if !cases.insert(result_case) {
        return Err(SemAnalysisError::DuplicateCase("default".into()));
    }

    let body = Box::new(body);
    let result = Statement::DCased(DCasedStatement { body, label });

    Ok((result, cases))
}

fn collect_cased(cased_st: CasedStatement) -> Result<(Statement, Cases)> {
    let CasedStatement { exp, body, label } = cased_st;
    let u = exp
        .get_const()
        .ok_or(SemAnalysisError::NotAConstCase(exp.clone()))?;
    let (body, mut cases) = collect_statement(*body)?;
    let result_case = (Some(u), label.clone());

    if !cases.insert(result_case) {
        return Err(SemAnalysisError::DuplicateCase(u.to_string()));
    }

    let body = Box::new(body);
    let result = Statement::Cased(CasedStatement { exp, body, label });

    Ok((result, cases))
}

fn collect_switch(mut switch: Switch) -> Result<(Statement, Cases)> {
    let (body, cases) = collect_statement(*switch.body)?;
    switch.body = Box::new(body);
    switch.cases = cases.into_iter().collect();
    Ok((Statement::Switch(switch), Cases::new()))
}

fn collect_if_st(if_st: If) -> Result<(Statement, Cases)> {
    let If {
        condition,
        then,
        els,
    } = if_st;
    let (then_body, then_cases) = collect_statement(*then)?;
    let then_body = Box::new(then_body);
    if let Some(els) = els {
        let (els_body, els_cases) = collect_statement(*els)?;
        let els_body = Box::new(els_body);
        if let Some(case) = els_cases.intersection(&then_cases).next() {
            Err(SemAnalysisError::DuplicateCase(case.1.clone()))
        } else {
            let cases = els_cases.union(&then_cases).cloned().collect();
            let result = If {
                condition,
                then: then_body,
                els: Some(els_body),
            };
            Ok((Statement::If(result), cases))
        }
    } else {
        let cases = then_cases;
        let result = If {
            condition,
            then: then_body,
            els,
        };
        Ok((Statement::If(result), cases))
    }
}

fn collect_for_st(mut for_st: For) -> Result<(Statement, Cases)> {
    let (body, cases) = collect_statement(*for_st.body)?;
    for_st.body = Box::new(body);
    Ok((Statement::For(for_st), cases))
}

fn collect_dowhile(mut dowhile: DoWhile) -> Result<(Statement, Cases)> {
    let (body, cases) = collect_statement(*dowhile.body)?;
    dowhile.body = Box::new(body);
    Ok((Statement::DoWhile(dowhile), cases))
}

fn collect_while(mut while_st: While) -> Result<(Statement, Cases)> {
    let (body, cases) = collect_statement(*while_st.body)?;
    while_st.body = Box::new(body);
    Ok((Statement::While(while_st), cases))
}

fn collect_statement(statement: Statement) -> Result<(Statement, Cases)> {
    use Statement as S;
    match statement {
        S::Compound(block) => {
            collect_block(block).map(|(block, cases)| (S::Compound(block), cases))
        }
        S::Labeled(label, st) => collect_labeled_st(label, *st),
        S::DCased(dcased) => collect_dcased(dcased),
        S::Cased(cased) => collect_cased(cased),
        S::Switch(switch) => collect_switch(switch),
        S::If(if_st) => collect_if_st(if_st),
        S::For(for_st) => collect_for_st(for_st),
        S::DoWhile(dowhile) => collect_dowhile(dowhile),
        S::While(while_st) => collect_while(while_st),
        S::Null | S::Goto(_) | S::Return(_) | S::Continue(_) | S::Break(_) | S::Exp(_) => {
            Ok((statement, Cases::new()))
        }
    }
}

fn collect_bi(item: AstBlockItem) -> Result<(AstBlockItem, Cases)> {
    match item {
        AstBlockItem::S(st) => {
            let (st, cases) = collect_statement(st)?;
            Ok((AstBlockItem::S(st), cases))
        }
        AstBlockItem::D(_) => Ok((item, Cases::new())),
    }
}

fn collect_block(block: AstBlock) -> Result<(AstBlock, Cases)> {
    let AstBlock { items } = block;
    let mut cases = Cases::new();
    let mut result_items = AstBlockItems::new();

    for item in items {
        let (new_item, inner_cases) = collect_bi(item)?;
        if let Some(next) = cases.intersection(&inner_cases).next() {
            return Err(SemAnalysisError::DuplicateCase(next.1.clone()));
        }
        cases = cases.union(&inner_cases).cloned().collect();
        result_items.push(new_item);
    }
    let result_block = AstBlock {
        items: result_items,
    };
    Ok((result_block, cases))
}

fn collect_fundec(mut fundec: FunDec) -> Result<FunDec> {
    if let Some(body) = fundec.body {
        let (body, cases) = collect_block(body)?;
        if !cases.is_empty() {
            return Err(SemAnalysisError::CaseNotInSwitch);
        }
        fundec.body = Some(body);
    }
    Ok(fundec)
}

fn collect_toplevel_dec(dec: Declaration) -> Result<Declaration> {
    match dec {
        Declaration::Var(_) => Ok(dec),
        Declaration::Fun(fundec) => collect_fundec(fundec).map(Declaration::Fun),
    }
}

pub fn collect_cases(ast: Ast) -> Result<Ast> {
    let Ast { declarations } = ast;
    let declarations = declarations
        .into_iter()
        .map(collect_toplevel_dec)
        .collect::<Result<Vec<_>>>()?;
    Ok(Ast { declarations })
}
