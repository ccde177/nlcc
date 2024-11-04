use super::{LexError, Result};

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