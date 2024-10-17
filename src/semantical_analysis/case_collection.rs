use super::{Result, SemAnalysisError};
use crate::parser::*;

use std::collections::HashSet;

type Cases = HashSet<(Option<u64>, Identifier)>;

fn collect_statement(statement: AstStatement) -> Result<(AstStatement, Cases)> {
    match statement {
        AstStatement::Compound(block) => {
            let (block, cases) = collect_block(block)?;
            let result = AstStatement::Compound(block);
            Ok((result, cases))
        }
        AstStatement::LabeledStatement(label, st) => {
            let (body, cases) = collect_statement(*st)?;
            let body = Box::new(body);
            let result = AstStatement::LabeledStatement(label, body);

            Ok((result, cases))
        }
        AstStatement::DefaultCase { statement, label } => {
            let (statement, mut cases) = collect_statement(*statement)?;
            let result_case = (None, label.clone());

            if cases.contains(&result_case) {
                return Err(SemAnalysisError::DuplicateCase("default".into()));
            }
            cases.insert(result_case);

            let statement = Box::new(statement);
            let result = AstStatement::DefaultCase { statement, label };

            Ok((result, cases))
        }
        AstStatement::Case {
            exp,
            statement,
            label,
        } => {
            let u = exp
                .get_const()
                .ok_or(SemAnalysisError::NotAConstCase(exp.clone()))?;
            let (statement, mut cases) = collect_statement(*statement)?;
            let result_case = (Some(u), label.clone());

            if cases.contains(&result_case) {
                return Err(SemAnalysisError::DuplicateCase(u.to_string()));
            }
            cases.insert(result_case);

            let statement = Box::new(statement);
            let result = AstStatement::Case {
                exp,
                statement,
                label,
            };

            Ok((result, cases))
        }
        AstStatement::Switch {
            ctrl_exp,
            body,
            label,
            ..
        } => {
            let (body, cases) = collect_statement(*body)?;
            let body = Box::new(body);
            let cases = cases.into_iter().collect();
            let result = AstStatement::Switch {
                ctrl_exp,
                body,
                cases,
                label,
            };
            Ok((result, Cases::new()))
        }
        AstStatement::If {
            condition,
            then,
            els,
        } => {
            let (then_body, then_cases) = collect_statement(*then)?;
            let then_body = Box::new(then_body);
            if let Some(els) = els {
                let (els_body, els_cases) = collect_statement(*els)?;
                let els_body = Box::new(els_body);
                if let Some(case) = els_cases.intersection(&then_cases).next() {
                    Err(SemAnalysisError::DuplicateCase(case.1.clone()))
                } else {
                    let cases = els_cases.union(&then_cases).cloned().collect();
                    let result = AstStatement::If {
                        condition,
                        then: then_body,
                        els: Some(els_body),
                    };
                    Ok((result, cases))
                }
            } else {
                let cases = then_cases;
                let result = AstStatement::If {
                    condition,
                    then: then_body,
                    els,
                };
                Ok((result, cases))
            }
        }
        AstStatement::For {
            init,
            condition,
            post,
            body,
            label,
        } => {
            let (body, cases) = collect_statement(*body)?;
            let body = Box::new(body);
            let result = AstStatement::For {
                init,
                condition,
                post,
                body,
                label,
            };

            Ok((result, cases))
        }
        AstStatement::DoWhile {
            condition,
            body,
            label,
        } => {
            let (body, cases) = collect_statement(*body)?;
            let body = Box::new(body);
            let result = AstStatement::DoWhile {
                condition,
                body,
                label,
            };
            Ok((result, cases))
        }
        AstStatement::While {
            condition,
            body,
            label,
        } => {
            let (body, cases) = collect_statement(*body)?;
            let body = Box::new(body);
            let result = AstStatement::While {
                condition,
                body,
                label,
            };

            Ok((result, cases))
        }
        AstStatement::Null
        | AstStatement::Goto(_)
        | AstStatement::Return(_)
        | AstStatement::Continue(_)
        | AstStatement::Break(_)
        | AstStatement::Exp(_) => Ok((statement, Cases::new())),
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

    for item in items.into_iter() {
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

pub fn collect_cases(f: AstFunction) -> Result<AstFunction> {
    let AstFunction { name, body } = f;
    let (body, cases) = collect_block(body)?;

    if !cases.is_empty() {
        return Err(SemAnalysisError::CaseNotInSwitch);
    }

    Ok(AstFunction { name, body })
}
