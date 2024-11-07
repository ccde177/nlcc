use std::{error, fmt};

#[derive(Debug, Eq, PartialEq)]
pub enum InnerLexError {
    UnexpectedChar(char),
    BadMcharOperator(String),
    BadConstantSuffix(char),
    ExpectedOperatorOrSeparator(char),
    BadFloatingPointConstant(String),
    UnexpectedEof,
}

pub struct LexError {
    inner: InnerLexError,
    ln: u64,
}

impl LexError {
    pub fn get_ln(&self) -> u64 {
        self.ln
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inner)
    }
}

impl InnerLexError {
    pub(super) fn set_line(self, ln: u64) -> LexError {
        LexError { inner: self, ln }
    }
}

impl error::Error for InnerLexError {}
impl fmt::Display for InnerLexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::BadFloatingPointConstant(s) => write!(f, "bad floating point constant {s}"),
            Self::UnexpectedEof => write!(f, "reached unexpected end of file"),
            Self::UnexpectedChar(c) => write!(f, "unexpected character: {c}"),
            Self::BadMcharOperator(s) => write!(f, "bad multi-char operator: {s}"),
            Self::BadConstantSuffix(c) => write!(f, "bad constant suffix: {c}"),
            Self::ExpectedOperatorOrSeparator(c) => {
                write!(f, "expected operator or separator, but got: {c}")
            }
        }
    }
}
