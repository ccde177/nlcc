use crate::lexer::LexError;
use crate::parser::ParseError;
use crate::semantic_analysis::SemAnalysisError;

#[allow(dead_code)]
pub enum DriverError {
    InputFileDoesNotExist(String),
    PreprocessorFailed,
    AssemblerFailed,
    LexerError(String),
    ParserError(String),
    SemanticError(String),
    IoError(String),
}

impl std::fmt::Display for DriverError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::IoError(e) => write!(f, "io error: {e}"),
            Self::LexerError(e) => write!(f, "lex error: {e}"),
            Self::ParserError(e) => write!(f, "parse error: {e}"),
            Self::SemanticError(e) => write!(f, "semantic error: {e}"),
            Self::InputFileDoesNotExist(name) => write!(f, "File {name} does not exist"),
            Self::PreprocessorFailed => write!(f, "Failed to run preprocessor"),
            Self::AssemblerFailed => write!(f, "Failed to run assembler"),
        }
    }
}

impl std::fmt::Debug for DriverError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl std::error::Error for DriverError {}

macro_rules! from_error {
    ($e:ty, $variant:path) => {
        impl From<$e> for DriverError {
            fn from(e: $e) -> Self {
                $variant(e.to_string())
            }
        }
    };
}

from_error!(LexError, Self::LexerError);
from_error!(ParseError, Self::ParserError);
from_error!(SemAnalysisError, Self::SemanticError);
from_error!(std::io::Error, Self::IoError);
