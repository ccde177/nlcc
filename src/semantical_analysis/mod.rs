use crate::parser::*;

use std::collections::HashMap;
use std::fmt;

pub type Identifier = String;
pub type Result<T> = std::result::Result<T, SemAnalysisError>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SemAnalysisError {
    VariableRedeclaration(Identifier),
    VariableNotDeclared(Identifier),
    WrongLvalue(AstExp),
}

impl fmt::Display for SemAnalysisError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::VariableRedeclaration(id) => write!(f, "Redeclaration of a variable {id}"),
            Self::VariableNotDeclared(id) => write!(f, "Unknown variable: {id}"),
            Self::WrongLvalue(exp) => write!(f, "Wrong lvalue: {exp:?}"),
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
        AstStatement::If{condition,then, els} => {
            let condition = resolve_exp(condition, vm)?;
            let then = Box::new(resolve_statement(*then, vm)?);
            let els = els.map_or(Ok(None), |bs| resolve_statement(*bs, vm).map(Box::new).map(Some))?;
            Ok(AstStatement::If{
                condition,
                then,
                els
            })
        },
        AstStatement::Return(e) => Ok(AstStatement::Return(resolve_exp(e, vm)?)),
        AstStatement::Exp(e) => Ok(AstStatement::Exp(resolve_exp(e, vm)?)),
        AstStatement::Null => Ok(AstStatement::Null),
    }
}

fn resolve_exp(exp: AstExp, vm: &mut VariableMap) -> Result<AstExp> {
    match exp {
        AstExp::Conditional { condition, then, els } => {
            let condition = resolve_exp(*condition, vm).map(Box::new)?;
            let then = resolve_exp(*then, vm).map(Box::new)?;
            let els = resolve_exp(*els, vm).map(Box::new)?;
            Ok(AstExp::Conditional{
                condition,
                then,
                els
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

pub fn validate(ast: Ast) -> Result<Ast> {
    match ast {
        Ast::FunDef(function) => {
            let name = function.name;
            let body = variable_resolution(function.body)?;
            Ok(Ast::FunDef(AstFunction { name, body }))
        }
    }
}
