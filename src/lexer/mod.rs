mod cursor;
#[cfg(test)]
mod lexer_tests;

use cursor::Cursor;
use std::{error, fmt};

pub type Tokens = Vec<Token>;
type Result<T> = std::result::Result<T, LexError>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token {
    Int,
    Identifier(String),
    OpenParanth,
    Void,
    CloseParanth,
    OpenCurly,
    Return,
    Constant(i64),
    LConstant(i64),
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
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
    AssignAdd,
    AssignSub,
    AssignMul,
    AssignDiv,
    AssignMod,
    AssignAnd,
    AssignOr,
    AssignXor,
    AssignShr,
    AssignShl,
    Assign,
    If,
    Else,
    QuestionMark,
    Goto,
    Colon,
    Do,
    While,
    For,
    Break,
    Continue,
    Case,
    KwDefault,
    Switch,
    Comma,
    Static,
    Extern,
    Long,
}

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
            Self::UnexpectedChar(c) => write!(f, "Unexpected character: {c}"),
            Self::BadMcharOperator(s) => write!(f, "Bad multi-char operator: {s}"),
            Self::BadConstantSuffix(c) => write!(f, "Bad constant suffix: {c}"),
            Self::ExpectedOperatorOrSeparator(c) => {
                write!(f, "Expected operator or separator, but got: {c}")
            }
        }
    }
}

impl Token {
    #[inline]
    pub fn is_type(&self) -> bool {
        matches!(self, Self::Int | Self::Long)
    }

    #[inline]
    pub fn is_compound_assign(&self) -> bool {
        matches!(
            self,
            Self::AssignAdd
                | Self::AssignSub
                | Self::AssignMul
                | Self::AssignDiv
                | Self::AssignMod
                | Self::AssignAnd
                | Self::AssignOr
                | Self::AssignXor
                | Self::AssignShr
                | Self::AssignShl
        )
    }

    #[inline]
    pub fn is_specifier(&self) -> bool {
        self.is_type() || matches!(self, Token::Extern | Token::Static)
    }

    #[inline]
    pub fn is_unaryop(&self) -> bool {
        matches!(
            self,
            Token::Tilde | Token::Hyphen | Token::LogicalNot | Token::Increment | Token::Decrement
        )
    }

    #[inline]
    pub fn is_binaryop(&self) -> bool {
        matches!(
            self,
            Token::Plus
                | Token::Hyphen
                | Token::Asterisk
                | Token::FSlash
                | Token::Percent
                | Token::LogicalAnd
                | Token::LogicalOr
                | Token::IsEqual
                | Token::IsNotEqual
                | Token::IsLessThan
                | Token::IsLessThanOrEqual
                | Token::IsGreaterThan
                | Token::IsGreaterThanOrEqual
                | Token::Assign
                | Token::BitwiseAnd
                | Token::BitwiseOr
                | Token::BitwiseXor
                | Token::ShiftLeft
                | Token::ShiftRight
                | Token::AssignAdd
                | Token::AssignSub
                | Token::AssignMul
                | Token::AssignDiv
                | Token::AssignMod
                | Token::AssignAnd
                | Token::AssignOr
                | Token::AssignXor
                | Token::AssignShl
                | Token::AssignShr
                | Token::Increment
                | Token::Decrement
                | Token::QuestionMark
        )
    }

    #[must_use]
    pub fn compound_to_single(&self) -> Self {
        match self {
            Self::AssignAdd => Self::Plus,
            Self::AssignMul => Self::Asterisk,
            Self::AssignSub => Self::Hyphen,
            Self::AssignDiv => Self::FSlash,
            Self::AssignMod => Self::Percent,
            Self::AssignAnd => Self::BitwiseAnd,
            Self::AssignOr => Self::BitwiseOr,
            Self::AssignXor => Self::BitwiseXor,
            Self::AssignShl => Self::ShiftLeft,
            Self::AssignShr => Self::ShiftRight,
            _ => self.clone(),
        }
    }
}

impl TryFrom<char> for Token {
    type Error = LexError;
    fn try_from(c: char) -> Result<Self> {
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
            '=' => Ok(Self::Assign),
            '&' => Ok(Self::BitwiseAnd),
            '|' => Ok(Self::BitwiseOr),
            '^' => Ok(Self::BitwiseXor),
            ':' => Ok(Self::Colon),
            '?' => Ok(Self::QuestionMark),
            ',' => Ok(Self::Comma),
            _ => Err(LexError::ExpectedOperatorOrSeparator(c)),
        }
    }
}

impl From<&str> for Token {
    fn from(s: &str) -> Self {
        match s {
            "int" => Self::Int,
            "long" => Self::Long,
            "return" => Self::Return,
            "void" => Self::Void,
            "if" => Self::If,
            "else" => Self::Else,
            "goto" => Self::Goto,
            "do" => Self::Do,
            "while" => Self::While,
            "for" => Self::For,
            "break" => Self::Break,
            "continue" => Self::Continue,
            "case" => Self::Case,
            "default" => Self::KwDefault,
            "switch" => Self::Switch,
            "extern" => Self::Extern,
            "static" => Self::Static,
            _ => Self::Identifier(s.to_owned()),
        }
    }
}

fn lex_mcharop3(first: char, second: char, third: char) -> Result<Token> {
    match (first, second, third) {
        ('>', '>', '=') => Ok(Token::AssignShr),
        ('<', '<', '=') => Ok(Token::AssignShl),
        _ => Err(LexError::BadMcharOperator(format!(
            "{first}{second}{third}"
        ))),
    }
}

fn lex_mcharoperator(cursor: &mut Cursor) -> Result<Token> {
    let first = cursor.take().expect("Is always Some");
    let second = cursor.peek();
    let eq = cursor.peek_2nd().filter(|c| *c == '=');

    if eq.is_some() {
        let second = second.expect("Is always Some");
        let third = eq.expect("Is always Some");
        let result = lex_mcharop3(first, second, third);
        if result.is_ok() {
            cursor.take();
            cursor.take();
            return result;
        }
    }

    if let Some(second) = second {
        let op = match (first, second) {
            ('-', '-') => Ok(Token::Decrement),
            ('+', '+') => Ok(Token::Increment),
            ('|', '|') => Ok(Token::LogicalOr),
            ('&', '&') => Ok(Token::LogicalAnd),
            ('=', '=') => Ok(Token::IsEqual),
            ('!', '=') => Ok(Token::IsNotEqual),
            ('>', '=') => Ok(Token::IsGreaterThanOrEqual),
            ('<', '=') => Ok(Token::IsLessThanOrEqual),
            ('<', '<') => Ok(Token::ShiftLeft),
            ('>', '>') => Ok(Token::ShiftRight),
            ('+', '=') => Ok(Token::AssignAdd),
            ('-', '=') => Ok(Token::AssignSub),
            ('*', '=') => Ok(Token::AssignMul),
            ('/', '=') => Ok(Token::AssignDiv),
            ('%', '=') => Ok(Token::AssignMod),
            ('&', '=') => Ok(Token::AssignAnd),
            ('|', '=') => Ok(Token::AssignOr),
            ('^', '=') => Ok(Token::AssignXor),
            _ => Err(LexError::BadMcharOperator(format!("{first}{second}"))),
        };
        if op.is_ok() {
            cursor.take();
        }
        return op.or(Token::try_from(first));
    }
    Token::try_from(first)
}

fn lex_constant(cursor: &mut Cursor) -> Result<Token> {
    let start = cursor.as_str();
    let mut count = 0;

    while cursor.skip_if(|c| c.is_ascii_digit()) {
        count += 1;
    }

    let is_long = cursor.bump_if('l') || cursor.bump_if('L');

    if let Some(next) = cursor.peek() {
        if next.is_alphabetic() || next == '_' {
            return Err(LexError::BadConstantSuffix(next));
        }
    }

    let const_str = &start[..count];
    let constant = if is_long {
        const_str
            .parse::<i64>()
            .map(Token::LConstant)
            .expect("Should never fail")
    } else {
        const_str
            .parse::<i64>()
            .map(Token::Constant)
            .expect("Should never fail")
    };

    Ok(constant)
}

fn lex_identifier(cursor: &mut Cursor) -> Token {
    let start = cursor.as_str();
    let mut count = 0;

    while let Some(c) = cursor.peek() {
        if !(c.is_ascii_alphanumeric() || c == '_') {
            break;
        }
        cursor.take();
        count += 1;
    }

    Token::from(&start[..count])
}

pub fn lex(input: &str) -> Result<Tokens> {
    let mut tokens = Tokens::with_capacity(input.len());
    let mut cursor = Cursor::new(input);
    cursor.skip_whitespaces();

    while let Some(next) = cursor.peek() {
        match next {
            ';' | '{' | '}' | '(' | ')' | '~' | '?' | ':' | ',' => {
                let token = Token::try_from(next).expect("Should never fail");
                tokens.push(token);
                cursor.take();
            }
            '%' | '^' | '/' | '*' | '-' | '+' | '=' | '!' | '>' | '<' | '|' | '&' => {
                let token = lex_mcharoperator(&mut cursor)?;
                tokens.push(token);
            }
            '_' | 'a'..='z' | 'A'..='Z' => {
                let token = lex_identifier(&mut cursor);
                tokens.push(token);
            }
            '0'..='9' => {
                let token = lex_constant(&mut cursor)?;
                tokens.push(token);
            }
            _ => return Err(LexError::UnexpectedChar(next)),
        }
        cursor.skip_whitespaces();
    }
    Ok(tokens)
}
