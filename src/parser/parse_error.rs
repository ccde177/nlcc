use crate::lexer::Token;
use std::fmt::{Display, Formatter};

pub type Result<T> = std::result::Result<T, ParseError>;
#[derive(Debug, Eq, PartialEq)]
pub enum ParseError {
    ExpectedButGot(Token, Token),
    ExpectedIdentifierButGot(Token),
    UnexpectedToken(Token),
    BadFactor(Token),
    BadUnaryOp(Token),
    TrailingComma,
    UnexpectedEof,
    InvalidTypeSpecifiers(Vec<Token>),
    InvalidStorageClass(Vec<Token>),
    BadForInit,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use ParseError as PE;
        match self {
            PE::ExpectedButGot(expected, got) => {
                write!(f, "Expected token {expected:?}, but got {got:?}")
            }
            PE::ExpectedIdentifierButGot(token) => {
                write!(f, "Expected identifier, but got {token:?}")
            }
            PE::UnexpectedToken(t) => write!(f, "Unexpected token {t:?}"),
            PE::BadFactor(t) => write!(f, "Bad factor {t:?}"),
            PE::BadUnaryOp(t) => write!(f, "Bad unary operator {t:?}"),
            PE::UnexpectedEof => write!(f, "Reached unexpected EOF"),
            PE::TrailingComma => write!(f, "Trailing comman in parameter list"),
            PE::InvalidTypeSpecifiers(ss) => {
                write!(f, "Invalid combination of type specifiers: {ss:?}")
            }
            PE::InvalidStorageClass(ss) => {
                write!(f, "Invalid combination of storage class specifiers: {ss:?}")
            }
            PE::BadForInit => write!(
                f,
                "Function declarations are not allowed inside for loop initialization"
            ),
        }
    }
}

impl std::error::Error for ParseError {}
