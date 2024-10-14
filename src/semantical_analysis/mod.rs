mod goto;
mod variable_resolution;

use crate::parser::*;
use goto::ensure_goto_correctness;
use variable_resolution::variable_resolution;

use std::fmt;

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

pub fn validate(ast: Ast) -> Result<Ast> {
    let Ast::FunDef(function) = ast;

    ensure_goto_correctness(&function)?;

    let name = function.name;
    let body = variable_resolution(function.body)?;

    Ok(Ast::FunDef(AstFunction { name, body }))
}
