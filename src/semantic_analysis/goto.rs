use crate::ast::*;
use crate::semantic_analysis::{Result, SemAnalysisError};
use std::collections::HashSet;

type LabelSet = HashSet<Identifier>;

fn collect_labels_statement(statement: &Statement, ls: &mut LabelSet) -> Result<()> {
    use Statement as S;
    match statement {
        S::While(While { body, .. })
        | S::DoWhile(DoWhile { body, .. })
        | S::For(For { body, .. })
        | S::Cased(CasedStatement { body, .. })
        | S::DCased(DCasedStatement { body, .. })
        | S::Switch(Switch { body, .. }) => collect_labels_statement(body, ls),
        S::Labeled(name, st) => {
            let is_duplicate = !ls.insert(name.clone());
            if is_duplicate {
                return Err(SemAnalysisError::LabelRedeclaration(name.clone()));
            }
            collect_labels_statement(st, ls)
        }
        S::If(If { then, els, .. }) => {
            collect_labels_statement(then, ls)?;
            if let Some(st) = els {
                collect_labels_statement(st, ls)?;
            }
            Ok(())
        }
        S::Compound(block) => {
            let AstBlock { items } = block;
            collect_labels_bims(items, ls)
        }
        S::Exp(_) | S::Break(_) | S::Continue(_) | S::Return(_) | S::Null | S::Goto(_) => Ok(()),
    }
}

fn collect_labels_bi(item: &AstBlockItem, ls: &mut LabelSet) -> Result<()> {
    if let AstBlockItem::S(s) = item {
        return collect_labels_statement(s, ls);
    }
    Ok(())
}

fn collect_labels_bims(items: &AstBlockItems, ls: &mut LabelSet) -> Result<()> {
    items
        .iter()
        .try_for_each(|item| collect_labels_bi(item, ls))
}

fn validate_statement(st: &Statement, ls: &LabelSet) -> Result<()> {
    use Statement as S;
    match st {
        S::Goto(label) => {
            if !ls.contains(label) {
                return Err(SemAnalysisError::UnknownLabel(label.clone()));
            }
            Ok(())
        }
        S::Compound(block) => validate_labels_b(block, ls),
        S::While(While { body, .. })
        | S::DoWhile(DoWhile { body, .. })
        | S::For(For { body, .. })
        | S::Switch(Switch { body, .. })
        | S::Cased(CasedStatement { body, .. })
        | S::DCased(DCasedStatement { body, .. })
        | S::Labeled(_, body) => validate_statement(body, ls),
        S::If(If { then, els, .. }) => {
            validate_statement(then, ls)?;
            if let Some(els) = els {
                validate_statement(els, ls)?;
            }
            Ok(())
        }
        S::Break(_) | S::Null | S::Continue(_) | S::Return(_) | S::Exp(_) => Ok(()),
    }
}

fn validate_labels_bi(item: &AstBlockItem, ls: &LabelSet) -> Result<()> {
    if let AstBlockItem::S(s) = item {
        validate_statement(s, ls)?;
    }
    Ok(())
}

fn validate_labels_b(block: &AstBlock, ls: &LabelSet) -> Result<()> {
    let AstBlock { items } = block;
    items
        .iter()
        .try_for_each(|item| validate_labels_bi(item, ls))
}

fn validate_function_body(body: &AstBlock) -> Result<()> {
    let AstBlock { items } = body;
    let mut ls = LabelSet::new();
    collect_labels_bims(items, &mut ls)?;
    validate_labels_b(body, &ls)
}

fn validate_fundec(fundec: &FunDec) -> Result<()> {
    let FunDec { body, .. } = fundec;
    body.as_ref().map(validate_function_body).transpose()?;
    Ok(())
}

pub fn ensure_goto_correctness(ast: &Ast) -> Result<()> {
    let Ast { functions } = ast;
    functions.iter().try_for_each(validate_fundec)
}
