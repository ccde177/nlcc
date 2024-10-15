mod goto;
mod variable_resolution;
mod loop_labeling;

use crate::parser::*;
use goto::ensure_goto_correctness;
use variable_resolution::variable_resolution;
use loop_labeling::label_loops;

use std::fmt;

pub type Result<T> = std::result::Result<T, SemAnalysisError>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SemAnalysisError {
    VariableRedeclaration(Identifier),
    VariableNotDeclared(Identifier),
    WrongLvalue(AstExp),
    LabelRedeclaration(Identifier),
    UnknownLabel(Identifier),
    BreakOutsideOfLoop,
    ContinueOutsideOfLoop
}

impl fmt::Display for SemAnalysisError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::VariableRedeclaration(id) => write!(f, "Redeclaration of a variable {id}"),
            Self::VariableNotDeclared(id) => write!(f, "Unknown variable: {id}"),
            Self::WrongLvalue(exp) => write!(f, "Wrong lvalue: {exp:?}"),
            Self::LabelRedeclaration(name) => write!(f, "Label {name} redeclaration"),
            Self::UnknownLabel(name) => write!(f, "Unknown label {name}"),
            Self::BreakOutsideOfLoop => write!(f, "break statement outside of loop"),
            Self::ContinueOutsideOfLoop => write!(f, "continue statement outside of loop"),
        }
    }
}

impl std::error::Error for SemAnalysisError {}

pub fn validate(ast: Ast) -> Result<Ast> {
    let Ast::FunDef(function) = ast;
    let function = variable_resolution(function).and_then(label_loops)?;
    
    ensure_goto_correctness(&function)?;

    Ok(Ast::FunDef(function))
}
