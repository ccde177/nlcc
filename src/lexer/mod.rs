use std::collections::VecDeque;
use std::{error, fmt};

#[cfg(test)]
mod lexer_tests;

pub type Tokens = VecDeque<Token>;
type Input = VecDeque<char>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token {
    Int,
    Identifier(String),
    OpenParanth,
    Void,
    CloseParanth,
    OpenCurly,
    Return,
    Constant(u64),
    Semicolon,
    CloseCurly,
    Tilde,
    Hyphen,
    Decrement,
    Plus,
    Asterisk,
    FSlash,
    Percent,
    Increment,
    LogicalNot,
    LogicalAnd,
    LogicalOr,
    IsEqual,
    IsNotEqual,
    IsLessThan,
    IsGreaterThan,
    IsLessThanOrEqual,
    IsGreaterThanOrEqual,
}

#[derive(Debug, Eq, PartialEq)]
pub enum LexError {
    UnexpectedChar(char),
    BadConstant(String),
}

impl error::Error for LexError {}
impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::UnexpectedChar(c) => write!(f, "Unexpected character: {c}"),
            Self::BadConstant(s) => write!(f, "Bad constant: {s}"),
        }
    }
}

impl TryFrom<char> for Token {
    type Error = LexError;
    fn try_from(c: char) -> Result<Self, Self::Error> {
        match c {
            ';' => Ok(Self::Semicolon),
            '(' => Ok(Self::OpenParanth),
            ')' => Ok(Self::CloseParanth),
            '{' => Ok(Self::OpenCurly),
            '}' => Ok(Self::CloseCurly),
            '-' => Ok(Self::Hyphen),
            '~' => Ok(Self::Tilde),
            '+' => Ok(Self::Plus),
            '%' => Ok(Self::Percent),
            '*' => Ok(Self::Asterisk),
            '/' => Ok(Self::FSlash),
            '!' => Ok(Self::LogicalNot),
            '<' => Ok(Self::IsLessThan),
            '>' => Ok(Self::IsGreaterThan),
            _ => Err(LexError::UnexpectedChar(c)),
        }
    }
}

impl From<String> for Token {
    fn from(s: String) -> Self {
        match s.as_str() {
            "int" => Self::Int,
            "return" => Self::Return,
            "void" => Self::Void,
            _ => Self::Identifier(s),
        }
    }
}

fn lex_mcharoperator(input: &mut Input) -> Result<Token, LexError> {
    let first = input.pop_front().expect("Should never fail");

    if input.is_empty() {
        return Token::try_from(first).map_err(|_| LexError::UnexpectedChar(first));
    }

    let result = match (first, input[0]) {
        ('-', '-') => Ok(Token::Decrement),
        ('+', '+') => Ok(Token::Increment),
        ('|', '|') => Ok(Token::LogicalOr),
        ('&', '&') => Ok(Token::LogicalAnd),
        ('=', '=') => Ok(Token::IsEqual),
        ('!', '=') => Ok(Token::IsNotEqual),
        ('>', '=') => Ok(Token::IsGreaterThanOrEqual),
        ('<', '=') => Ok(Token::IsLessThanOrEqual),
        _ => Err(LexError::UnexpectedChar(first)),
    };

    if let Ok(_) = result {
        input.pop_front();
    }

    result.or(Token::try_from(first))
}

fn lex_constant(input: &mut Input) -> Result<Token, LexError> {
    let mut buf = String::new();
    while !input.is_empty() && input[0].is_ascii_digit() {
        let digit = input.pop_front().expect("Should never fail");
        buf.push(digit);
    }

    if !input.is_empty() {
        match input[0] {
            'a'..='z' | 'A'..='Z' | '_' => return Err(LexError::UnexpectedChar(input[0])),
            _ => (),
        }
    }

    buf.parse()
        .map(|i| Token::Constant(i))
        .map_err(|_| LexError::BadConstant(buf.clone()))
}

fn lex_identifier(input: &mut Input) -> Result<Token, LexError> {
    let mut buf = String::new();
    while !input.is_empty() && input[0].is_ascii_alphanumeric() {
        let c = input.pop_front().expect("Should never fail");
        buf.push(c);
    }
    Ok(Token::from(buf))
}

pub fn lex(input: String) -> Result<Tokens, LexError> {
    let mut tokens = Tokens::new();
    let mut input: Input = input.chars().collect();

    while !input.is_empty() {
        match input[0] {
            ';' | '{' | '}' | '(' | ')' | '~' | '%' | '*' | '/' => {
                let token = Token::try_from(input[0]).expect("Should never fail");
                tokens.push_back(token);
                let _ = input.pop_front();
            }
            '-' | '+' | '=' | '!' | '>' | '<' | '|' | '&' => {
                let token = lex_mcharoperator(&mut input)?;
                tokens.push_back(token);
            }
            'a'..='z' | 'A'..='Z' => {
                let token = lex_identifier(&mut input)?;
                tokens.push_back(token);
            }
            '0'..='9' => {
                let token = lex_constant(&mut input)?;
                tokens.push_back(token);
            }
            c if c.is_whitespace() => {
                input.pop_front();
            }
            _ => return Err(LexError::UnexpectedChar(input[0])),
        }
    }
    Ok(tokens)
}
