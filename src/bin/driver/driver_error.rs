#[cfg(feature = "lexer")]
use crate::lexer::LexError;
#[cfg(feature = "parser")]
use crate::parser::ParseError;
#[cfg(feature = "semantic_analysis")]
use crate::semantic_analysis::SemAnalysisError;

pub(super) type Result<T> = std::result::Result<T, DriverError>;

#[allow(dead_code)]
pub enum DriverError {
    InputFileDoesNotExist(String),
    PreprocessorFailed,
    #[cfg(feature = "emission")]
    AssemblerFailed,
    #[cfg(feature = "lexer")]
    LexerError(String, u64),
    #[cfg(feature = "parser")]
    ParserError(String, u64),
    #[cfg(feature = "semantic_analysis")]
    SemanticError(String),
    IoError(String),
}

impl std::fmt::Display for DriverError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::IoError(e) => write!(f, "io error: {e}"),
            #[cfg(feature = "lexer")]
            Self::LexerError(e, ln) => write!(f, "lex error on line {ln}: {e}"),
            #[cfg(feature = "parser")]
            Self::ParserError(e, ln) => write!(f, "parse error on line {ln}: {e}"),
            #[cfg(feature = "semantic_analysis")]
            Self::SemanticError(e) => write!(f, "semantic error: {e}"),
            Self::InputFileDoesNotExist(name) => write!(f, "File {name} does not exist"),
            Self::PreprocessorFailed => write!(f, "Failed to run preprocessor"),
            #[cfg(feature = "emission")]
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

macro_rules! from_lined_error {
    ($e:ty, $variant:path) => {
        impl From<$e> for DriverError {
            fn from(e: $e) -> Self {
                let msg = e.to_string();
                let ln = e.get_ln();
                $variant(msg, ln)
            }
        }
    };
}

macro_rules! from_error {
    ($e:ty, $variant:path) => {
        impl From<$e> for DriverError {
            fn from(e: $e) -> Self {
                $variant(e.to_string())
            }
        }
    };
}

from_error!(std::io::Error, Self::IoError);
#[cfg(feature = "lexer")]
from_lined_error!(LexError, Self::LexerError);
#[cfg(feature = "parser")]
from_lined_error!(ParseError, Self::ParserError);
#[cfg(feature = "semantic_analysis")]
from_error!(SemAnalysisError, Self::SemanticError);
