use crate::lexer::{Token, Tokens};
use std::fmt;

#[cfg(test)]
mod parser_tests;

type Identifier = String;

#[derive(Debug, Clone)]
pub enum Ast {
    FunDef(AstFunction),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AstFunction {
    pub name: Identifier,
    pub body: AstBlockItems,
}

pub type AstBlockItems = Vec<AstBlockItem>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AstBlockItem {
    S(AstStatement),
    D(AstDeclaration),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AstDeclaration {
    pub name: Identifier,
    pub init: Option<AstExp>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AstStatement {
    Return(AstExp),
    Exp(AstExp),
    Null,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstExp {
    Binary(AstBinaryOp, Box<AstExp>, Box<AstExp>),
    Unary(AstUnaryOp, Box<AstExp>),
    Assignment(Box<AstExp>, Box<AstExp>),
    Var(Identifier),
    Constant(u64),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AstBinaryOp {
    Add,
    Multiply,
    Div,
    Mod,
    Substract,
    LogicalAnd,
    LogicalOr,
    IsEqual,
    IsNotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AstUnaryOp {
    Complement,
    Negate,
    LogicalNot,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ParseError {
    ExpectedButGot(Token, Token),
    ExpectedButGotNone(Token),
    MoreTokensThanExpected(Tokens),
    BadExpression(Token),
    ExpectedExpression,
    BadDeclaration,
    UnexpectedEof,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ExpectedButGot(t1, t2) => {
                write!(f, "Expected token of type {:?} but got {:?}", t1, t2)
            }
            Self::ExpectedButGotNone(t) => write!(f, "Expected token of type {:?} but got None", t),
            Self::MoreTokensThanExpected(ts) => write!(f, "Trailing tokens: {:?}", ts),
            Self::ExpectedExpression => write!(f, "Expected expression"),
            Self::BadExpression(t) => write!(f, "Bad expression starting with {:?}", t),
            Self::BadDeclaration => write!(f, "Bad declaration"),
            Self::UnexpectedEof => write!(f, "Unexpected EOF"),
        }
    }
}

impl std::error::Error for ParseError {}

impl AstExp {
    pub fn is_var(&self) -> bool {
        matches!(self, Self::Var(_))
    }
}

fn expect_token(tokens: &mut Tokens, token: Token) -> Result<(), ParseError> {
    tokens
        .pop_front()
        .map_or(Err(ParseError::ExpectedButGotNone(token.clone())), |t| {
            if t == token {
                Ok(())
            } else {
                Err(ParseError::ExpectedButGot(token.clone(), t.clone()))
            }
        })
}

fn expect_identifier(tokens: &mut Tokens) -> Result<Identifier, ParseError> {
    let dummy = Token::Identifier("".into());
    tokens.pop_front().map_or(
        Err(ParseError::ExpectedButGotNone(dummy.clone())),
        |t| match t {
            Token::Identifier(i) => Ok(i),
            _ => Err(ParseError::ExpectedButGot(dummy.clone(), t.clone())),
        },
    )
}

fn parse_unary(tokens: &mut Tokens) -> Result<AstUnaryOp, ParseError> {
    if let Some(token) = take_token(tokens) {
        match token {
            Token::Hyphen => Ok(AstUnaryOp::Negate),
            Token::Tilde => Ok(AstUnaryOp::Complement),
            Token::LogicalNot => Ok(AstUnaryOp::LogicalNot),
            _ => Err(ParseError::ExpectedButGot(Token::Hyphen, token)),
        }
    } else {
        Err(ParseError::ExpectedButGotNone(Token::Hyphen))
    }
}

fn take_token(tokens: &mut Tokens) -> Option<Token> {
    tokens.pop_front()
}

fn parse_binop(tokens: &mut Tokens) -> Result<AstBinaryOp, ParseError> {
    if tokens.is_empty() {
        return Err(ParseError::ExpectedExpression);
    }
    let next_token = take_token(tokens).expect("Should never fail");

    match next_token {
        Token::Plus => Ok(AstBinaryOp::Add),
        Token::Hyphen => Ok(AstBinaryOp::Substract),
        Token::FSlash => Ok(AstBinaryOp::Div),
        Token::Percent => Ok(AstBinaryOp::Mod),
        Token::Asterisk => Ok(AstBinaryOp::Multiply),
        Token::LogicalAnd => Ok(AstBinaryOp::LogicalAnd),
        Token::LogicalOr => Ok(AstBinaryOp::LogicalOr),
        Token::IsEqual => Ok(AstBinaryOp::IsEqual),
        Token::IsNotEqual => Ok(AstBinaryOp::IsNotEqual),
        Token::IsLessThan => Ok(AstBinaryOp::LessThan),
        Token::IsLessThanOrEqual => Ok(AstBinaryOp::LessOrEqual),
        Token::IsGreaterThan => Ok(AstBinaryOp::GreaterThan),
        Token::IsGreaterThanOrEqual => Ok(AstBinaryOp::GreaterOrEqual),
        _ => Err(ParseError::BadExpression(next_token.clone())),
    }
}

fn token_is_binaryop(token: &Token) -> bool {
    matches!(
        token,
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
    )
}

fn get_prec(token: &Token) -> u64 {
    match token {
        Token::Asterisk => 50,
        Token::FSlash => 50,
        Token::Percent => 50,
        Token::Plus => 45,
        Token::Hyphen => 45,
        Token::IsLessThan => 35,
        Token::IsLessThanOrEqual => 35,
        Token::IsGreaterThan => 35,
        Token::IsGreaterThanOrEqual => 35,
        Token::IsEqual => 30,
        Token::IsNotEqual => 30,
        Token::LogicalAnd => 10,
        Token::LogicalOr => 5,
        Token::Assign => 1,
        _ => 0,
    }
}

fn parse_exp(tokens: &mut Tokens, min_prec: u64) -> Result<AstExp, ParseError> {
    let mut left = parse_factor(tokens)?;
    while !tokens.is_empty() {
        let next_token = tokens.front().expect("Should never fail").clone();
        let prec = get_prec(&next_token);
        if !(token_is_binaryop(&next_token) && prec >= min_prec) {
            break;
        }

        if next_token == Token::Assign {
            take_token(tokens);
            let right = parse_exp(tokens, get_prec(&next_token))?;
            left = AstExp::Assignment(Box::new(left), Box::new(right));
        } else {
            let operator = parse_binop(tokens)?;
            let right = parse_exp(tokens, prec + 1)?;

            left = AstExp::Binary(operator, Box::new(left), Box::new(right));
        }
    }
    Ok(left)
}

fn parse_factor(tokens: &mut Tokens) -> Result<AstExp, ParseError> {
    if tokens.is_empty() {
        return Err(ParseError::ExpectedExpression);
    }
    let next_token = tokens.front().expect("Should never fail");

    match next_token {
        Token::Tilde | Token::Hyphen | Token::LogicalNot => {
            let operator = parse_unary(tokens)?;
            let inner_exp = Box::new(parse_factor(tokens)?);
            Ok(AstExp::Unary(operator, inner_exp))
        }
        Token::OpenParanth => {
            take_token(tokens);
            let inner_exp = parse_exp(tokens, 0)?;
            expect_token(tokens, Token::CloseParanth)?;
            Ok(inner_exp)
        }
        Token::Constant(i) => {
            let inner = *i;
            take_token(tokens);
            Ok(AstExp::Constant(inner))
        }
        Token::Identifier(id) => {
            let inner = id.clone();
            take_token(tokens);
            Ok(AstExp::Var(inner))
        }
        _ => Err(ParseError::BadExpression(next_token.clone())),
    }
}

fn parse_statement(tokens: &mut Tokens) -> Result<AstStatement, ParseError> {
    let next = peek(tokens).ok_or(ParseError::UnexpectedEof)?;
    match next {
        Token::Return => {
            take_token(tokens);
            let exp = parse_exp(tokens, 0)?;
            expect_token(tokens, Token::Semicolon)?;
            Ok(AstStatement::Return(exp))
        }
        Token::Semicolon => {
            take_token(tokens);
            Ok(AstStatement::Null)
        }
        _ => {
            let exp = parse_exp(tokens, 0)?;
            expect_token(tokens, Token::Semicolon)?;
            Ok(AstStatement::Exp(exp))
        }
    }
}

fn parse_declaration(tokens: &mut Tokens) -> Result<AstDeclaration, ParseError> {
    expect_token(tokens, Token::Int)?;
    let id = expect_identifier(tokens)?;
    let exp = match peek(tokens) {
        Some(Token::Assign) => {
            take_token(tokens);
            let exp = parse_exp(tokens, get_prec(&Token::Assign))?;
            Some(exp)
        }
        Some(Token::Semicolon) => None,
        _ => return Err(ParseError::BadDeclaration),
    };

    expect_token(tokens, Token::Semicolon)?;

    Ok(AstDeclaration {
        name: id,
        init: exp,
    })
}

fn parse_block_item(tokens: &mut Tokens) -> Result<AstBlockItem, ParseError> {
    match peek(tokens) {
        Some(Token::Int) => Ok(AstBlockItem::D(parse_declaration(tokens)?)),
        Some(_) => Ok(AstBlockItem::S(parse_statement(tokens)?)),
        None => Err(ParseError::UnexpectedEof),
    }
}

fn peek(tokens: &Tokens) -> Option<Token> {
    tokens.front().cloned()
}

fn parse_function(tokens: &mut Tokens) -> Result<AstFunction, ParseError> {
    expect_token(tokens, Token::Int)?;
    let identifier = expect_identifier(tokens)?;
    expect_token(tokens, Token::OpenParanth)?;
    expect_token(tokens, Token::Void)?;
    expect_token(tokens, Token::CloseParanth)?;
    expect_token(tokens, Token::OpenCurly)?;
    let mut body = AstBlockItems::new();

    while !tokens.is_empty() && peek(tokens).unwrap() != Token::CloseCurly {
        let next_block_item = parse_block_item(tokens)?;
        body.push(next_block_item);
    }
    expect_token(tokens, Token::CloseCurly)?;
    if !tokens.is_empty() {
        Err(ParseError::MoreTokensThanExpected(tokens.clone()))
    } else {
        Ok(AstFunction {
            name: identifier,
            body,
        })
    }
}

fn parse_program(tokens: &mut Tokens) -> Result<Ast, ParseError> {
    Ok(Ast::FunDef(parse_function(tokens)?))
}

pub fn parse(mut tokens: Tokens) -> Result<Ast, ParseError> {
    parse_program(&mut tokens)
}
