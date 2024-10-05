use std::collections::VecDeque;
use std::{error, fmt};

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
}

#[derive(Debug)]
pub enum LexError {
    UnexpectedChar(char),
    UnknownMcharOperator(String),
    BadConstant(String),
}

impl error::Error for LexError {}
impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::UnexpectedChar(c) => write!(f, "Unexpected character: {c}"),
            Self::UnknownMcharOperator(s) => write!(f, "Unknown multi-character operator: {s}"),
            Self::BadConstant(s) => write!(f, "Bad constant: {s}"),
        }
    }
}

impl Token {
    fn from_mchoperator(s: String) -> Result<Token, LexError> {
        match s.as_str() {
            "--" => Ok(Self::Decrement),
            "++" => Ok(Self::Increment),
            _ => Err(LexError::UnknownMcharOperator(s)),
        }
    }

    pub fn is_binary(&self) -> bool {
	match self {
	    Self::Plus => true,
	    Self::Asterisk => true,
	    Self::Hyphen => true,
	    Self::Percent => true,
	    Self::FSlash => true,
	    _ => false
	}
    }

    pub fn get_prec(&self) -> u64 {
	match self {
	    Self::Plus => 45,
	    Self::Asterisk => 50,
	    Self::Hyphen => 45,
	    Self::Percent => 50,
	    Self::FSlash => 50,
	    _ => 0
	}
    }
}

impl TryFrom<char> for Token {
    type Error = &'static str;
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
            _ => Err("Not a special character"),
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
    match (first, input[0]) {
        ('-', '-') => Token::from_mchoperator(String::from("--")),
        ('+', '+') => Token::from_mchoperator(String::from("++")),
        _ => Token::try_from(first).map_err(|_| LexError::UnexpectedChar(first)),
    }
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
            '-' | '+' => {
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
