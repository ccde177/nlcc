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

impl From<LexError> for DriverError {
    fn from(e: LexError) -> Self {
        Self::LexerError(e.to_string())
    }
}

impl From<ParseError> for DriverError {
    fn from(e: ParseError) -> Self {
        Self::ParserError(e.to_string())
    }
}

impl From<SemAnalysisError> for DriverError {
    fn from(e: SemAnalysisError) -> Self {
        Self::SemanticError(e.to_string())
    }
}

impl From<std::io::Error> for DriverError {
    fn from(e: std::io::Error) -> Self {
        Self::IoError(e.to_string())
    }
}
