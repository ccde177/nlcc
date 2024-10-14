use crate::parser::*;
use crate::semantical_analysis::{Result, SemAnalysisError};

use std::collections::HashMap;

#[derive(Debug)]
struct NameGenerator {
    count: u64,
}

impl NameGenerator {
    fn new() -> Self {
        Self { count: 0 }
    }

    fn generate_name(&mut self, before: &Identifier) -> Identifier {
        self.count += 1;
        format!("{before}..{}", self.count)
    }
}

struct VariableMap {
    inner: HashMap<Identifier, (Identifier, bool)>,
}

impl VariableMap {
    fn new() -> Self {
        let inner = HashMap::new();
        Self { inner }
    }
    fn get_uniq_name(&self, name: &Identifier) -> Option<Identifier> {
        self.inner.get(name).map(|mv| mv.0.clone())
    }
    fn is_in_current_scope(&self, name: &Identifier) -> bool {
        self.inner.get(name).filter(|mv| mv.1).is_some()
    }

    fn add_to_scope(&mut self, name: Identifier, ng: &mut NameGenerator) -> Identifier {
        let generated = ng.generate_name(&name);
        self.inner.insert(name, (generated.clone(), true));
        generated
    }

    fn new_scope_copy(other: &Self) -> Self {
        let vm = other
            .inner
            .clone()
            .into_iter()
            .map(|(name, (uniq_name, _))| (name, (uniq_name, false)))
            .collect();

        Self { inner: vm }
    }
}

fn resolve_declaration(
    dec: AstDeclaration,
    vm: &mut VariableMap,
    ng: &mut NameGenerator,
) -> Result<AstDeclaration> {
    if vm.is_in_current_scope(&dec.name) {
        return Err(SemAnalysisError::VariableRedeclaration(dec.name.clone()));
    }

    let uniq_name = vm.add_to_scope(dec.name, ng);

    let mut exp = None;
    if let Some(e) = dec.init {
        exp = Some(resolve_exp(e, vm)?);
    }

    Ok(AstDeclaration {
        name: uniq_name,
        init: exp,
    })
}

fn resolve_statement(
    st: AstStatement,
    vm: &mut VariableMap,
    ng: &mut NameGenerator,
) -> Result<AstStatement> {
    match st {
        AstStatement::If {
            condition,
            then,
            els,
        } => {
            let condition = resolve_exp(condition, vm)?;
            let then = Box::new(resolve_statement(*then, vm, ng)?);
            let els = els.map_or(Ok(None), |bs| {
                resolve_statement(*bs, vm, ng).map(Box::new).map(Some)
            })?;
            Ok(AstStatement::If {
                condition,
                then,
                els,
            })
        }
        AstStatement::Compound(block) => {
            let new_vm = VariableMap::new_scope_copy(vm);
            let block = resolve_block(block, new_vm, ng)?;
            Ok(AstStatement::Compound(block))
        }
        AstStatement::LabeledStatement(label, statement) => {
            let statement = resolve_statement(*statement, vm, ng).map(Box::new)?;
            Ok(AstStatement::LabeledStatement(label, statement))
        }
        AstStatement::Return(e) => Ok(AstStatement::Return(resolve_exp(e, vm)?)),
        AstStatement::Exp(e) => Ok(AstStatement::Exp(resolve_exp(e, vm)?)),
        statement => Ok(statement),
    }
}

fn resolve_exp(exp: AstExp, vm: &mut VariableMap) -> Result<AstExp> {
    match exp {
        AstExp::Conditional {
            condition,
            then,
            els,
        } => {
            let condition = resolve_exp(*condition, vm).map(Box::new)?;
            let then = resolve_exp(*then, vm).map(Box::new)?;
            let els = resolve_exp(*els, vm).map(Box::new)?;
            Ok(AstExp::Conditional {
                condition,
                then,
                els,
            })
        }
        AstExp::Assignment(left, right) => {
            if !left.is_var() {
                return Err(SemAnalysisError::WrongLvalue(*left));
            }
            let left = resolve_exp(*left, vm).map(Box::new)?;
            let right = resolve_exp(*right, vm).map(Box::new)?;
            Ok(AstExp::Assignment(left, right))
        }
        AstExp::Unary(
            op @ (AstUnaryOp::PostfixIncrement
            | AstUnaryOp::PrefixIncrement
            | AstUnaryOp::PostfixDecrement
            | AstUnaryOp::PrefixDecrement),
            e,
        ) => {
            let exp = resolve_exp(*e, vm)?;
            if !exp.is_var() {
                Err(SemAnalysisError::WrongLvalue(exp))
            } else {
                Ok(AstExp::Unary(op, Box::new(exp)))
            }
        }
        AstExp::Var(name) => vm
            .get_uniq_name(&name)
            .ok_or(SemAnalysisError::VariableNotDeclared(name.clone()))
            .map(|n| AstExp::Var(n.to_string())),
        AstExp::Unary(op, exp) => {
            let exp = resolve_exp(*exp, vm).map(Box::new)?;
            Ok(AstExp::Unary(op, exp))
        }
        AstExp::Binary(op, src, dst) => {
            let src = resolve_exp(*src, vm).map(Box::new)?;
            let dst = resolve_exp(*dst, vm).map(Box::new)?;
            Ok(AstExp::Binary(op, src, dst))
        }
        AstExp::Constant(_) => Ok(exp),
    }
}

fn resolve_block(block: AstBlock, mut vm: VariableMap, ng: &mut NameGenerator) -> Result<AstBlock> {
    let mut result = AstBlockItems::new();
    for item in block.items.into_iter() {
        match item {
            AstBlockItem::S(statement) => {
                let statement = resolve_statement(statement, &mut vm, ng)?;
                result.push(AstBlockItem::S(statement));
            }
            AstBlockItem::D(declaration) => {
                let declaration = resolve_declaration(declaration, &mut vm, ng)?;
                result.push(AstBlockItem::D(declaration));
            }
        }
    }

    Ok(AstBlock { items: result })
}

pub fn variable_resolution(block: AstBlock) -> Result<AstBlock> {
    let scope = VariableMap::new();
    let mut ng = NameGenerator::new();
    resolve_block(block, scope, &mut ng)
}
