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

pub use typecheck::SYM_TABLE;

pub type Result<T> = std::result::Result<T, SemAnalysisError>;

#[derive(Clone, Debug, PartialEq, Eq)]
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
                write!(f, "Static function {name} redeclared ad non-static")
            }
            Self::StorageIdInForInit(name) => write!(
                f,
                "Variable {name} has storage keyword in a for loop init expression"
            ),
            Self::ConflictingLinkage(name) => {
                write!(f, "Conflicting linkage for identifier {name}")
            }
            Self::InitOnExternVar(name) => write!(f, "Extern variable {name} has initializer"),
            Self::NonConstantInit(name) => {
                write!(f, "Global variable {name} has non-constant initializer")
            }
            Self::IdentifierRedeclaration(id) => write!(f, "Redeclaration of a identifier {id}"),
            Self::LocalFunDefinition(name) => {
                write!(f, "Attempt to define function{name} in local local context")
            }
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
            Self::UndeclaredFunction(s) => write!(f, "Call to undeclared function {s}"),
            Self::DuplicateDeclaration(name) => write!(f, "Redeclaration of {name}"),
            Self::IncompatibleFunDec(name) => {
                write!(f, "Incompatible redeclaration of function {name}")
            }
            Self::FunctionRedefinition(name) => write!(f, "Redefinition of a function {name}"),
            Self::VariableCall(name) => write!(f, "Variable {name} called as a function"),
            Self::FunctionNameAsVariable(name) => write!(f, "Function {name} used as variable"),
            Self::ExpectedArgsCountButGot(expected, got, name) => write!(
                f,
                "Function {name} called with {got} arguments, but expected {expected}"
            ),
        }
    }
}

impl std::error::Error for SemAnalysisError {}

pub fn validate(ast: Ast) -> Result<Ast> {
    let mut validated = name_resolution(ast)
        .and_then(label_loops)
        .and_then(collect_cases)?;
    ensure_goto_correctness(&mut validated)?;
    let type_checked = check_types(validated)?;

    Ok(type_checked)
}
