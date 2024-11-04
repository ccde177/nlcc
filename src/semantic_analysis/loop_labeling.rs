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

    fn label_case(&self, case: AstConst) -> Option<Identifier> {
        let negative = case.is_negative();
        let changed_minus = if negative { "_" } else { "" };
        let case = case.abs();
        self.switch_stack
            .last()
            .map(|sl| format!("case_{changed_minus}{case}_{sl}"))
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

fn label_dcased(mut dcased: DCasedStatement, ng: &mut NameGenerator) -> Result<Statement> {
    dcased.label = ng
        .label_default_case()
        .ok_or_else(|| SemAnalysisError::DefaultNotInSwitch)?;
    dcased.body = label_statement(*dcased.body, ng).map(Box::new)?;
    Ok(Statement::DCased(dcased))
}

fn get_case_const_init(exp: &UntypedExp) -> Result<AstConst> {
    match exp {
        UntypedExp::Constant(c) => Ok(*c),
        _ => Err(SemAnalysisError::NotAConstCase(Exp::Untyped(exp.clone()))),
    }
}

fn label_cased(mut cased: CasedStatement, ng: &mut NameGenerator) -> Result<Statement> {
    let const_exp = get_case_const_init(&cased.exp)?;

    cased.label = ng
        .label_case(const_exp)
        .ok_or_else(|| SemAnalysisError::CaseNotInSwitch)?;
    cased.body = label_statement(*cased.body, ng).map(Box::new)?;
    Ok(Statement::Cased(cased))
}

fn label_switch(mut switch: Switch, ng: &mut NameGenerator) -> Result<Statement> {
    switch.label = ng.new_switch_ctx();
    switch.body = label_statement(*switch.body, ng).map(Box::new)?;
    ng.exit_ctx();
    Ok(Statement::Switch(switch))
}

fn label_if_st(mut if_st: If, ng: &mut NameGenerator) -> Result<Statement> {
    if_st.then = label_statement(*if_st.then, ng).map(Box::new)?;
    if_st.els = if_st.els.map_or(Ok(None), |bst| {
        label_statement(*bst, ng).map(Box::new).map(Some)
    })?;
    Ok(Statement::If(if_st))
}

fn label_for_st(mut for_st: For, ng: &mut NameGenerator) -> Result<Statement> {
    for_st.label = ng.new_loop_ctx();
    for_st.body = label_statement(*for_st.body, ng).map(Box::new)?;
    Ok(Statement::For(for_st))
}

fn label_dowhile(mut dowhile: DoWhile, ng: &mut NameGenerator) -> Result<Statement> {
    dowhile.label = ng.new_loop_ctx();
    dowhile.body = label_statement(*dowhile.body, ng).map(Box::new)?;
    ng.exit_ctx();
    Ok(Statement::DoWhile(dowhile))
}

fn label_while(mut while_st: While, ng: &mut NameGenerator) -> Result<Statement> {
    while_st.label = ng.new_loop_ctx();
    while_st.body = label_statement(*while_st.body, ng).map(Box::new)?;
    ng.exit_ctx();
    Ok(Statement::While(while_st))
}

fn label_statement(statement: Statement, ng: &mut NameGenerator) -> Result<Statement> {
    use Statement as S;
    match statement {
        S::Labeled(label, st) => label_statement(*st, ng)
            .map(Box::new)
            .map(|st| S::Labeled(label, st)),
        S::Break(_) => ng
            .label_break()
            .ok_or_else(|| SemAnalysisError::BreakOutsideOfLoop)
            .map(S::Break),
        S::Continue(_) => ng
            .label_continue()
            .ok_or_else(|| SemAnalysisError::ContinueOutsideOfLoop)
            .map(S::Continue),
        S::Compound(block) => label_block(block, ng).map(S::Compound),
        S::DCased(dcased) => label_dcased(dcased, ng),
        S::Cased(cased) => label_cased(cased, ng),
        S::Switch(switch) => label_switch(switch, ng),
        S::If(if_st) => label_if_st(if_st, ng),
        S::For(for_st) => label_for_st(for_st, ng),
        S::DoWhile(dowhile) => label_dowhile(dowhile, ng),
        S::While(while_st) => label_while(while_st, ng),
        S::Null | S::Return(_) | S::Goto(_) | S::Exp(_) => Ok(statement),
    }
}

fn label_block_item(item: AstBlockItem, ng: &mut NameGenerator) -> Result<AstBlockItem> {
    if let AstBlockItem::S(st) = item {
        label_statement(st, ng).map(AstBlockItem::S)
    } else {
        Ok(item)
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

fn label_fundec(mut fundec: FunDec, ng: &mut NameGenerator) -> Result<FunDec> {
    fundec.body = fundec
        .body
        .map(|block| label_block(block, ng))
        .transpose()?;
    Ok(fundec)
}

fn label_toplevel_dec(dec: Declaration, ng: &mut NameGenerator) -> Result<Declaration> {
    use Declaration as D;
    match dec {
        D::Fun(fundec) => label_fundec(fundec, ng).map(D::Fun),
        D::Var(_) => Ok(dec),
    }
}

pub fn label_loops(ast: Ast) -> Result<Ast> {
    let Ast {
        declarations: functions,
    } = ast;

    let mut ng = NameGenerator::new();
    let functions = functions
        .into_iter()
        .map(|dec| label_toplevel_dec(dec, &mut ng))
        .collect::<Result<Vec<_>>>()?;

    Ok(Ast {
        declarations: functions,
    })
}
