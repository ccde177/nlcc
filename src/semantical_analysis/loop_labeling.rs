use super::{Result, SemAnalysisError};
use crate::ast::*;

#[derive(Default)]
struct NameGenerator {
    counter: u64,
    loop_stack: Vec<Identifier>,
    switch_stack: Vec<Identifier>,
    ctx_stack: Vec<GeneratorCtx>,
    ctx: GeneratorCtx,
}

#[derive(Default, Copy, Clone)]
enum GeneratorCtx {
    Switch,
    Loop,
    #[default]
    None,
}

impl NameGenerator {
    fn new() -> Self {
        Self::default()
    }

    fn label_continue(&self) -> Option<Identifier> {
        self.loop_stack.last().map(|ll| format!("continue_{ll}"))
    }

    fn label_break(&self) -> Option<Identifier> {
        match self.ctx {
            GeneratorCtx::Switch => self.switch_stack.last().map(|sl| format!("break_{sl}")),
            GeneratorCtx::Loop => self.loop_stack.last().map(|ll| format!("break_{ll}")),
            GeneratorCtx::None => None,
        }
    }

    fn label_case(&self, case: u64) -> Option<Identifier> {
        self.switch_stack
            .last()
            .map(|sl| format!("case_{case}_{sl}"))
    }

    fn label_default_case(&self) -> Option<Identifier> {
        self.switch_stack
            .last()
            .map(|sl| format!("case_default_{sl}"))
    }

    fn new_switch_ctx(&mut self) -> Identifier {
        self.ctx_stack.push(self.ctx);
        self.ctx = GeneratorCtx::Switch;
        self.counter += 1;

        let id = self.counter;
        let label = format!("switch_label.{id}");

        self.switch_stack.push(label.clone());
        label
    }

    fn new_loop_ctx(&mut self) -> Identifier {
        self.ctx_stack.push(self.ctx);
        self.ctx = GeneratorCtx::Loop;
        self.counter += 1;

        let id = self.counter;
        let label = format!("loop_label.{id}");

        self.loop_stack.push(label.clone());
        label
    }

    fn exit_ctx(&mut self) {
        match self.ctx {
            GeneratorCtx::Loop => {
                self.loop_stack.pop();
            }
            GeneratorCtx::Switch => {
                self.switch_stack.pop();
            }
            GeneratorCtx::None => (),
        }
        if let Some(old_ctx) = self.ctx_stack.pop() {
            self.ctx = old_ctx;
        }
    }
}

fn label_statement(statement: AstStatement, ng: &mut NameGenerator) -> Result<AstStatement> {
    match statement {
        AstStatement::LabeledStatement(label, st) => {
            let st = label_statement(*st, ng).map(Box::new)?;
            Ok(AstStatement::LabeledStatement(label, st))
        }
        AstStatement::Compound(block) => {
            let block = label_block(block, ng)?;
            Ok(AstStatement::Compound(block))
        }
        AstStatement::Break(_) => {
            let label = ng
                .label_break()
                .ok_or(SemAnalysisError::BreakOutsideOfLoop)?;
            Ok(AstStatement::Break(label))
        }
        AstStatement::DefaultCase { statement, .. } => {
            let label = ng
                .label_default_case()
                .ok_or(SemAnalysisError::DefaultNotInSwitch)?;
            let statement = label_statement(*statement, ng).map(Box::new)?;
            Ok(AstStatement::DefaultCase { statement, label })
        }
        AstStatement::Case { exp, statement, .. } => {
            let const_exp = exp
                .get_const()
                .ok_or(SemAnalysisError::NotAConstCase(exp.clone()))?;
            let label = ng
                .label_case(const_exp)
                .ok_or(SemAnalysisError::CaseNotInSwitch)?;
            let statement = label_statement(*statement, ng).map(Box::new)?;

            Ok(AstStatement::Case {
                exp,
                statement,
                label,
            })
        }
        AstStatement::Switch {
            ctrl_exp,
            body,
            cases,
            ..
        } => {
            let label = ng.new_switch_ctx();
            let body = label_statement(*body, ng).map(Box::new)?;
            ng.exit_ctx();
            Ok(AstStatement::Switch {
                ctrl_exp,
                body,
                cases,
                label,
            })
        }
        AstStatement::If {
            condition,
            then,
            els,
        } => {
            let then = label_statement(*then, ng).map(Box::new)?;
            let els = els.map_or(Ok(None), |bst| {
                label_statement(*bst, ng).map(Box::new).map(Some)
            })?;
            Ok(AstStatement::If {
                condition,
                then,
                els,
            })
        }
        AstStatement::For {
            init,
            condition,
            post,
            body,
            ..
        } => {
            let label = ng.new_loop_ctx();
            let body = label_statement(*body, ng).map(Box::new)?;
            Ok(AstStatement::For {
                init,
                condition,
                post,
                body,
                label,
            })
        }
        AstStatement::Continue(_) => {
            let label = ng
                .label_continue()
                .ok_or(SemAnalysisError::ContinueOutsideOfLoop)?;
            Ok(AstStatement::Continue(label))
        }
        AstStatement::DoWhile {
            condition, body, ..
        } => {
            let label = ng.new_loop_ctx();
            let body = label_statement(*body, ng).map(Box::new)?;
            ng.exit_ctx();
            Ok(AstStatement::DoWhile {
                condition,
                body,
                label,
            })
        }
        AstStatement::While {
            condition, body, ..
        } => {
            let label = ng.new_loop_ctx();
            let body = label_statement(*body, ng).map(Box::new)?;
            ng.exit_ctx();
            Ok(AstStatement::While {
                condition,
                label,
                body,
            })
        }
        AstStatement::Null
        | AstStatement::Return(_)
        | AstStatement::Goto(_)
        | AstStatement::Exp(_) => Ok(statement),
    }
}

fn label_block_item(item: AstBlockItem, ng: &mut NameGenerator) -> Result<AstBlockItem> {
    match item {
        AstBlockItem::S(statement) => {
            let labeled_statement = label_statement(statement, ng)?;
            Ok(AstBlockItem::S(labeled_statement))
        }
        AstBlockItem::D(_) => Ok(item),
    }
}

fn label_block(block: AstBlock, ng: &mut NameGenerator) -> Result<AstBlock> {
    let AstBlock { items } = block;
    let labeled_items = items
        .into_iter()
        .map(|item| label_block_item(item, ng))
        .collect::<Result<Vec<_>>>()?;

    Ok(AstBlock {
        items: labeled_items,
    })
}

pub fn label_loops(f: AstFunction) -> Result<AstFunction> {
    let AstFunction { name, body } = f;
    let mut ng = NameGenerator::new();
    let body = label_block(body, &mut ng)?;

    Ok(AstFunction { name, body })
}
