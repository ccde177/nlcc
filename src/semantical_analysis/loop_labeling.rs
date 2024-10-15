use super::{Result, SemAnalysisError};
use crate::parser::*;

struct NameGenerator {
    count: i64,
}

impl NameGenerator {
    fn new() -> Self {
        Self { count: 0 }
    }

    fn generate_label(&mut self) -> Identifier {
        let id = self.count;
        self.count += 1;
        format!("loop_label.{id}")
    }
}

fn label_statement(
    statement: AstStatement,
    current_label: Option<Identifier>,
    ng: &mut NameGenerator,
) -> Result<AstStatement> {
    match statement {
        AstStatement::While {
            condition, body, ..
        } => {
            let new_label = ng.generate_label();
            let labeled_body = label_statement(*body, Some(new_label.clone()), ng).map(Box::new)?;

            Ok(AstStatement::While {
                label: new_label,
                body: labeled_body,
                condition,
            })
        }
        AstStatement::DoWhile {
            condition, body, ..
        } => {
            let new_label = ng.generate_label();
            let labeled_body = label_statement(*body, Some(new_label.clone()), ng).map(Box::new)?;

            Ok(AstStatement::DoWhile {
                label: new_label,
                body: labeled_body,
                condition,
            })
        }
        AstStatement::For {
            init,
            condition,
            post,
            body,
            ..
        } => {
            let new_label = ng.generate_label();
            let labeled_body = label_statement(*body, Some(new_label.clone()), ng).map(Box::new)?;
            Ok(AstStatement::For {
                label: new_label,
                body: labeled_body,
                init,
                condition,
                post,
            })
        }
        AstStatement::If {
            condition,
            then,
            els,
        } => {
            let labeled_then = label_statement(*then, current_label.clone(), ng).map(Box::new)?;
            let labeled_els = els.map_or(Ok(None), |st| {
                label_statement(*st, current_label, ng)
                    .map(Box::new)
                    .map(Some)
            })?;
            Ok(AstStatement::If {
                condition,
                then: labeled_then,
                els: labeled_els,
            })
        }
        AstStatement::Compound(block) => {
            let labeled_block = label_block(block, current_label, ng)?;
            Ok(AstStatement::Compound(labeled_block))
        }
        AstStatement::LabeledStatement(label, st) => {
            let labeled_body = label_statement(*st, current_label, ng).map(Box::new)?;
            Ok(AstStatement::LabeledStatement(label, labeled_body))
        }
        AstStatement::Break(_) => current_label
            .ok_or(SemAnalysisError::BreakOutsideOfLoop)
            .map(AstStatement::Break),
        AstStatement::Continue(_) => current_label
            .ok_or(SemAnalysisError::ContinueOutsideOfLoop)
            .map(AstStatement::Continue),
        _ => Ok(statement),
    }
}

fn label_block_item(
    item: AstBlockItem,
    current_label: Option<Identifier>,
    ng: &mut NameGenerator,
) -> Result<AstBlockItem> {
    match item {
        AstBlockItem::S(statement) => {
            let labeled_statement = label_statement(statement, current_label, ng)?;
            Ok(AstBlockItem::S(labeled_statement))
        }
        _ => Ok(item),
    }
}

fn label_block(
    block: AstBlock,
    current_label: Option<Identifier>,
    ng: &mut NameGenerator,
) -> Result<AstBlock> {
    let AstBlock { items } = block;
    let labeled_items = items
        .into_iter()
        .map(|item| label_block_item(item, current_label.clone(), ng))
        .collect::<Result<Vec<_>>>()?;

    Ok(AstBlock {
        items: labeled_items,
    })
}

pub fn label_loops(f: AstFunction) -> Result<AstFunction> {
    let AstFunction{name, body} = f;
    let mut ng = NameGenerator::new();
    let body = label_block(body, None, &mut ng)?;
    
    Ok(AstFunction{
        name,
        body
    })
}
