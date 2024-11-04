use std::{error, fmt};

#[derive(Debug, Eq, PartialEq)]
pub enum LexError {
    UnexpectedChar(char),
    BadMcharOperator(String),
    BadConstantSuffix(char),
    ExpectedOperatorOrSeparator(char),
}

impl error::Error for LexError {}
impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::UnexpectedChar(c) => write!(f, "unexpected character: {c}"),
            Self::BadMcharOperator(s) => write!(f, "bad multi-char operator: {s}"),
            Self::BadConstantSuffix(c) => write!(f, "bad constant suffix: {c}"),
            Self::ExpectedOperatorOrSeparator(c) => {
                write!(f, "expected operator or separator, but got: {c}")
            }
        }
    }
}

pub type Result<T> = std::result::Result<T, LexError>;
