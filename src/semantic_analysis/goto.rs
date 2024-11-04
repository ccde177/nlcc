use crate::ast::*;
use crate::semantic_analysis::{Result, SemAnalysisError};
use std::collections::HashSet;

struct LabelSet {
    base: Identifier,
    inner: HashSet<Identifier>,
}

impl LabelSet {
    fn new(base: Identifier) -> Self {
        Self {
            base,
            inner: HashSet::new(),
        }
    }

    fn insert(&mut self, value: Identifier) -> bool {
        self.inner.insert(value)
    }

    fn get_new_name(&self, value: &Identifier) -> Identifier {
        format!("{}.{value}", self.base)
    }
    fn contains(&self, value: &Identifier) -> bool {
        self.inner.contains(value)
    }
}

fn collect_labels_statement(statement: &mut Statement, ls: &mut LabelSet) -> Result<()> {
    use Statement as S;
    match statement {
        S::While(While { body, .. })
        | S::DoWhile(DoWhile { body, .. })
        | S::For(For { body, .. })
        | S::Cased(CasedStatement { body, .. })
        | S::DCased(DCasedStatement { body, .. })
        | S::Switch(Switch { body, .. }) => collect_labels_statement(body, ls),
        S::Labeled(name, st) => {
            *name = ls.get_new_name(name);
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
        S::Goto(label) => {
            *label = ls.get_new_name(label);
            Ok(())
        }
        S::Exp(_) | S::Break(_) | S::Continue(_) | S::Return(_) | S::Null => Ok(()),
    }
}

fn collect_labels_bi(item: &mut AstBlockItem, ls: &mut LabelSet) -> Result<()> {
    if let AstBlockItem::S(s) = item {
        return collect_labels_statement(s, ls);
    }
    Ok(())
}

fn collect_labels_bims(items: &mut AstBlockItems, ls: &mut LabelSet) -> Result<()> {
    items
        .iter_mut()
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

fn validate_function_body(body: &mut AstBlock, ls: &mut LabelSet) -> Result<()> {
    let AstBlock { items } = body;
    collect_labels_bims(items, ls)?;
    validate_labels_b(body, ls)
}

fn validate_fundec(fundec: &mut FunDec) -> Result<()> {
    let FunDec { body, name, .. } = fundec;
    let mut ls = LabelSet::new(name.clone());
    body.as_mut()
        .map(|body| validate_function_body(body, &mut ls))
        .transpose()?;
    Ok(())
}

fn validate_toplevel_dec(dec: &mut Declaration) -> Result<()> {
    use Declaration as D;
    match dec {
        D::Fun(fundec) => validate_fundec(fundec),
        D::Var(_) => Ok(()),
    }
}

pub fn ensure_goto_correctness(ast: &mut Ast) -> Result<()> {
    let Ast { declarations } = ast;
    declarations.iter_mut().try_for_each(validate_toplevel_dec)
}
