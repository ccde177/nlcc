use crate::lexer::Token;
use std::fmt::{Display, Formatter};

pub type Result<T> = std::result::Result<T, ParseError>;
#[derive(Debug, Eq, PartialEq)]
pub enum InnerParseError {
    ExpectedButGot(Token, Token),
    ExpectedIdentifierButGot(Token),
    UnexpectedToken(Token),
    BadFactor(Token),
    BadUnaryOp(Token),
    TrailingComma,
    UnexpectedEof,
    InvalidTypeSpecifiers(Vec<Token>),
    DuplicateSignSpecifiers(Token, Token),
    InvalidStorageClass(Vec<Token>),
    BadForInit,
}

#[derive(Debug)]
pub struct ParseError {
    pub inner: InnerParseError,
    pub ln: u64,
}

impl ParseError {
    pub fn get_ln(&self) -> u64 {
        self.ln
    }
}

impl InnerParseError {
    pub fn set_line(self, ln: u64) -> ParseError {
        ParseError { inner: self, ln }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner)
    }
}

impl Display for InnerParseError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use InnerParseError as PE;
        match self {
            PE::DuplicateSignSpecifiers(t1, t2) => {
                write!(f, "duplicate sign specifier {t1:?} and {t2:?}")
            }
            PE::ExpectedButGot(expected, got) => {
                write!(f, "expected token {expected:?}, but got {got:?}")
            }
            PE::ExpectedIdentifierButGot(token) => {
                write!(f, "expected identifier, but got {token:?}")
            }
            PE::UnexpectedToken(t) => write!(f, "unexpected token {t:?}"),
            PE::BadFactor(t) => write!(f, "bad factor {t:?}"),
            PE::BadUnaryOp(t) => write!(f, "bad unary operator {t:?}"),
            PE::UnexpectedEof => write!(f, "reached unexpected EOF"),
            PE::TrailingComma => write!(f, "trailing comman in parameter list"),
            PE::InvalidTypeSpecifiers(ss) => {
                write!(f, "invalid combination of type specifiers: {ss:?}")
            }
            PE::InvalidStorageClass(ss) => {
                write!(f, "invalid combination of storage class specifiers: {ss:?}")
            }
            PE::BadForInit => write!(
                f,
                "function declarations are not allowed inside for loop initialization"
            ),
        }
    }
}

impl std::error::Error for InnerParseError {}
