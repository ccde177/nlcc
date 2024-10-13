use crate::parser::*;

use std::collections::{HashMap, HashSet};
use std::fmt;

pub type Identifier = String;
pub type Result<T> = std::result::Result<T, SemAnalysisError>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SemAnalysisError {
    VariableRedeclaration(Identifier),
    VariableNotDeclared(Identifier),
    WrongLvalue(AstExp),
    LabelRedeclaration(Identifier),
    UnknownLabel(Identifier),
}

impl fmt::Display for SemAnalysisError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::VariableRedeclaration(id) => write!(f, "Redeclaration of a variable {id}"),
            Self::VariableNotDeclared(id) => write!(f, "Unknown variable: {id}"),
            Self::WrongLvalue(exp) => write!(f, "Wrong lvalue: {exp:?}"),
            Self::LabelRedeclaration(name) => write!(f, "Label {name} redeclaration"),
            Self::UnknownLabel(name) => write!(f, "Unknown label {name}"),
        }
    }
}

impl std::error::Error for SemAnalysisError {}

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

type VariableMap = HashMap<Identifier, Identifier>;

fn resolve_declaration(
    dec: AstDeclaration,
    vm: &mut VariableMap,
    ng: &mut NameGenerator,
) -> Result<AstDeclaration> {
    if vm.contains_key(&dec.name) {
        return Err(SemAnalysisError::VariableRedeclaration(dec.name.clone()));
    }
    let uniq_name = ng.generate_name(&dec.name);
    vm.insert(dec.name.clone(), uniq_name.clone());

    let mut exp = None;
    if let Some(e) = dec.init {
        exp = Some(resolve_exp(e, vm)?);
    }

    Ok(AstDeclaration {
        name: uniq_name,
        init: exp,
    })
}

fn resolve_statement(st: AstStatement, vm: &mut VariableMap) -> Result<AstStatement> {
    match st {
        AstStatement::If {
            condition,
            then,
            els,
        } => {
            let condition = resolve_exp(condition, vm)?;
            let then = Box::new(resolve_statement(*then, vm)?);
            let els = els.map_or(Ok(None), |bs| {
                resolve_statement(*bs, vm).map(Box::new).map(Some)
            })?;
            Ok(AstStatement::If {
                condition,
                then,
                els,
            })
        }
        AstStatement::LabeledStatement(label, statement) => {
            let statement = resolve_statement(*statement, vm).map(Box::new)?;
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
            if !left.as_ref().is_var() {
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
            .get(&name)
            .ok_or(SemAnalysisError::VariableNotDeclared(name.clone()))
            .map(|n| AstExp::Var(n.to_string())),
        AstExp::Unary(op, exp) => {
            let exp = resolve_exp(*exp, vm)?;
            Ok(AstExp::Unary(op, Box::new(exp)))
        }
        AstExp::Binary(op, src, dst) => {
            let src = resolve_exp(*src, vm)?;
            let dst = resolve_exp(*dst, vm)?;
            Ok(AstExp::Binary(op, Box::new(src), Box::new(dst)))
        }
        AstExp::Constant(_) => Ok(exp),
    }
}

fn variable_resolution(blocks: AstBlockItems) -> Result<AstBlockItems> {
    let mut result = AstBlockItems::new();
    let mut ng = NameGenerator::new();
    let mut vm = VariableMap::new();
    for block in blocks.into_iter() {
        match block {
            AstBlockItem::S(statement) => {
                let statement = resolve_statement(statement, &mut vm)?;
                result.push(AstBlockItem::S(statement));
            }
            AstBlockItem::D(declaration) => {
                let declaration = resolve_declaration(declaration, &mut vm, &mut ng)?;
                result.push(AstBlockItem::D(declaration));
            }
        }
    }
    Ok(result)
}

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
        _ => (),
    }
    Ok(ls)
}
fn ensure_goto_correctness(blocks: &AstBlockItems) -> Result<()> {
    let mut ls = LabelSet::new();
    for block in blocks {
        if let AstBlockItem::S(s) = block {            
            let collect = collect_labels_statement(s)?;
            ls = intersect_ls(&ls, &collect)?;
        }
    }
    for block in blocks {
        if let AstBlockItem::S(AstStatement::Goto(label)) = block {
            if !ls.contains(label){
                return Err(SemAnalysisError::UnknownLabel(label.clone()));
            }
        }
    }
    Ok(())
}

pub fn validate(ast: Ast) -> Result<Ast> {
    let Ast::FunDef(function) = ast;
    let name = function.name;
    let body = variable_resolution(function.body)?;
    ensure_goto_correctness(&body)?;
    Ok(Ast::FunDef(AstFunction { name, body }))
}
