mod case_collection;
mod goto;
mod loop_labeling;
mod variable_resolution;

use crate::ast::*;
use case_collection::collect_cases;
use goto::ensure_goto_correctness;
use loop_labeling::label_loops;
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
    DuplicateCase(Identifier),
    BreakOutsideOfLoop,
    ContinueOutsideOfLoop,
    CaseNotInSwitch,
    NotAConstCase(AstExp),
    DefaultNotInSwitch,
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
            Self::CaseNotInSwitch => write!(f, "case not in switch"),
            Self::NotAConstCase(exp) => {
                write!(f, "Not a const expression inside case label: {exp:?}")
            }
            Self::DefaultNotInSwitch => write!(f, "default case not in switch"),
            Self::DuplicateCase(case) => write!(f, "Duplicate case {case:?}"),
        }
    }
}

impl std::error::Error for SemAnalysisError {}

pub fn validate(ast: Ast) -> Result<Ast> {
    let Ast::FunDef(function) = ast;
    let function = variable_resolution(function)
        .and_then(label_loops)
        .and_then(collect_cases)?;

    ensure_goto_correctness(&function)?;

    Ok(Ast::FunDef(function))
}
