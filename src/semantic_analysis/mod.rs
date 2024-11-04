mod case_collection;
mod goto;
mod loop_labeling;
mod name_resolution;
mod typecheck;

use crate::ast::*;
use case_collection::collect_cases;
use goto::ensure_goto_correctness;
use loop_labeling::label_loops;
use name_resolution::name_resolution;
use std::fmt;
use typecheck::check_types;

pub use typecheck::{StaticInit, SYM_TABLE};

pub type Result<T> = std::result::Result<T, SemAnalysisError>;

#[derive(Clone, Debug)]
pub enum SemAnalysisError {
    IdentifierRedeclaration(Identifier),
    LocalFunDefinition(Identifier),
    VariableNotDeclared(Identifier),
    WrongLvalue(Exp),
    LabelRedeclaration(Identifier),
    UnknownLabel(Identifier),
    DuplicateCase(Identifier),
    BreakOutsideOfLoop,
    ContinueOutsideOfLoop,
    CaseNotInSwitch,
    NotAConstCase(Exp),
    DefaultNotInSwitch,
    IncompatibleFunDec(Identifier),
    UndeclaredFunction(Identifier),
    DuplicateDeclaration(Identifier),
    FunctionRedefinition(Identifier),
    VariableCall(Identifier),
    ExpectedArgsCountButGot(usize, usize, String),
    FunctionNameAsVariable(Identifier),
    NonConstantInit(Identifier),
    ConflictingLinkage(String),
    InitOnExternVar(String),
    StorageIdInForInit(String),
    StaticFunctionRedeclaredNonStatic(String),
}

impl fmt::Display for SemAnalysisError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::StaticFunctionRedeclaredNonStatic(name) => {
                write!(f, "static function {name} redeclared ad non-static")
            }
            Self::StorageIdInForInit(name) => write!(
                f,
                "variable {name} has storage keyword in a for loop init expression"
            ),
            Self::ConflictingLinkage(name) => {
                write!(f, "conflicting linkage for identifier {name}")
            }
            Self::InitOnExternVar(name) => write!(f, "extern variable {name} has initializer"),
            Self::NonConstantInit(name) => {
                write!(f, "global variable {name} has non-constant initializer")
            }
            Self::IdentifierRedeclaration(id) => write!(f, "redeclaration of a identifier {id}"),
            Self::LocalFunDefinition(name) => {
                write!(f, "attempt to define function{name} in local local context")
            }
            Self::VariableNotDeclared(id) => write!(f, "unknown variable: {id}"),
            Self::WrongLvalue(exp) => write!(f, "wrong lvalue: {exp:?}"),
            Self::LabelRedeclaration(name) => write!(f, "label {name} redeclaration"),
            Self::UnknownLabel(name) => write!(f, "unknown label {name}"),
            Self::BreakOutsideOfLoop => write!(f, "break statement outside of loop"),
            Self::ContinueOutsideOfLoop => write!(f, "continue statement outside of loop"),
            Self::CaseNotInSwitch => write!(f, "case not in switch"),
            Self::NotAConstCase(exp) => {
                write!(f, "not a const expression in case: {exp:?}")
            }
            Self::DefaultNotInSwitch => write!(f, "default case not in switch"),
            Self::DuplicateCase(case) => write!(f, "duplicate case {case:?}"),
            Self::UndeclaredFunction(s) => write!(f, "call to undeclared function {s}"),
            Self::DuplicateDeclaration(name) => write!(f, "redeclaration of {name}"),
            Self::IncompatibleFunDec(name) => {
                write!(f, "incompatible redeclaration of function {name}")
            }
            Self::FunctionRedefinition(name) => write!(f, "redefinition of a function {name}"),
            Self::VariableCall(name) => write!(f, "variable {name} called as a function"),
            Self::FunctionNameAsVariable(name) => write!(f, "function {name} used as variable"),
            Self::ExpectedArgsCountButGot(expected, got, name) => write!(
                f,
                "function {name} called with {got} arguments, but expected {expected}"
            ),
        }
    }
}

impl std::error::Error for SemAnalysisError {}

// The order must be:
// name_resolution > [goto >] check_types > ... > label_loops > .. > collect_cases > ..
pub fn validate(ast: Ast) -> Result<Ast> {
    let mut resolved = name_resolution(ast)?;
    ensure_goto_correctness(&mut resolved)?;

    check_types(resolved)
        .and_then(label_loops)
        .and_then(collect_cases)
}
