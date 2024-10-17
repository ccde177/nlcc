use crate::semantic_analysis::{Result, SemAnalysisError};
use crate::ast::*;

use std::collections::HashSet;

type LabelSet = HashSet<Identifier>;

fn intersect_ls(ls: &LabelSet, other: &LabelSet) -> Result<LabelSet> {
    if let Some(wrong) = ls.intersection(other).next() {
        return Err(SemAnalysisError::LabelRedeclaration(wrong.clone()));
    }
    let union = ls.union(other).cloned().collect();
    Ok(union)
}

fn collect_labels_statement(statement: &AstStatement) -> Result<LabelSet> {
    let mut ls = LabelSet::new();
    match statement {
        AstStatement::While { body, .. }
        | AstStatement::DoWhile { body, .. }
        | AstStatement::For { body, .. }
        | AstStatement::Case {
            statement: body, ..
        }
        | AstStatement::DefaultCase {
            statement: body, ..
        }
        | AstStatement::Switch { body, .. } => {
            let body_collect = collect_labels_statement(body)?;
            ls = intersect_ls(&ls, &body_collect)?;
        }
        AstStatement::LabeledStatement(name, st) => {
            if ls.contains(name) {
                return Err(SemAnalysisError::LabelRedeclaration(name.clone()));
            }
            ls.insert(name.clone());
            let ls_inner = collect_labels_statement(st)?;
            ls = intersect_ls(&ls, &ls_inner)?;
        }
        AstStatement::If { then, els, .. } => {
            let then_collect = collect_labels_statement(then)?;
            ls = intersect_ls(&ls, &then_collect)?;
            if let Some(st) = els {
                let els_collect = collect_labels_statement(st)?;
                ls = intersect_ls(&ls, &els_collect)?;
            }
        }
        AstStatement::Compound(block) => {
            let AstBlock { items } = block;
            let inner_ls = collect_labels_bims(items)?;
            ls = intersect_ls(&ls, &inner_ls)?;
        }
        AstStatement::Exp(_)
        | AstStatement::Break(_)
        | AstStatement::Continue(_)
        | AstStatement::Return(_)
        | AstStatement::Null
        | AstStatement::Goto(_) => (),
    }
    Ok(ls)
}

fn collect_labels_bims(items: &AstBlockItems) -> Result<LabelSet> {
    let mut ls = LabelSet::new();
    for item in items {
        if let AstBlockItem::S(s) = item {
            let collect = collect_labels_statement(s)?;
            ls = intersect_ls(&ls, &collect)?;
        }
    }
    Ok(ls)
}

fn validate_statement(st: &AstStatement, ls: &LabelSet) -> Result<()> {
    match st {
        AstStatement::Goto(label) => {
            if !ls.contains(label) {
                return Err(SemAnalysisError::UnknownLabel(label.clone()));
            }
        }
        AstStatement::Compound(block) => {
            validate_labels_b(block, ls)?;
        }
        AstStatement::While { body, .. }
        | AstStatement::DoWhile { body, .. }
        | AstStatement::For { body, .. }
        | AstStatement::Switch { body, .. }
        | AstStatement::Case {
            statement: body, ..
        }
        | AstStatement::LabeledStatement(_, body)
        | AstStatement::DefaultCase {
            statement: body, ..
        } => {
            validate_statement(body, ls)?;
        }
        AstStatement::If { then, els, .. } => {
            validate_statement(then, ls)?;
            if let Some(els) = els {
                validate_statement(els, ls)?;
            }
        }
        AstStatement::Break(_)
        | AstStatement::Null
        | AstStatement::Continue(_)
        | AstStatement::Return(_)
        | AstStatement::Exp(_) => (),
    }
    Ok(())
}

fn validate_labels_bi(item: &AstBlockItem, ls: &LabelSet) -> Result<()> {
    if let AstBlockItem::S(s) = item {
        validate_statement(s, ls)?;
    }
    Ok(())
}

fn validate_labels_b(block: &AstBlock, ls: &LabelSet) -> Result<()> {
    let AstBlock { items } = block;
    for item in items {
        validate_labels_bi(item, ls)?;
    }
    Ok(())
}

fn validate_function_body(body: &AstBlock) -> Result<()> {
    let AstBlock { items } = body;
    let ls = collect_labels_bims(items)?;

    validate_labels_b(body, &ls)?;
    Ok(())
}

pub fn ensure_goto_correctness(f: &AstFunction) -> Result<()> {
    let AstFunction { body, .. } = f;
    validate_function_body(body)?;
    Ok(())
}
