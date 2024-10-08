use crate::lexer::{Token, Tokens};
use std::fmt;

#[cfg(test)]
mod parser_tests;

type Identifier = String;

#[derive(Debug, Clone)]
pub enum Program {
    FunDef(Function),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Identifier,
    pub body: Statement,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Statement {
    Return(Expression),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    Constant(u64),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BinaryOp {
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
pub enum UnaryOperator {
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
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ExpectedButGot(t1, t2) => {
                write!(f, "Expected token of type {:?} but got {:?}", t2, t1)
            }
            Self::ExpectedButGotNone(t) => write!(f, "Expected token of type {:?} but got None", t),
            Self::MoreTokensThanExpected(ts) => write!(f, "Trailing tokens: {:?}", ts),
            Self::ExpectedExpression => write!(f, "Expected expression"),
            Self::BadExpression(t) => write!(f, "Bad expression starting with {:?}", t),
        }
    }
}

impl std::error::Error for ParseError {}

fn expect_token(tokens: &mut Tokens, token: Token) -> Result<(), ParseError> {
    tokens
        .pop_front()
        .map_or(Err(ParseError::ExpectedButGotNone(token.clone())), |t| {
            if t == token {
                Ok(())
            } else {
                Err(ParseError::ExpectedButGot(t.clone(), token.clone()))
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

fn parse_unary(tokens: &mut Tokens) -> Result<UnaryOperator, ParseError> {
    if let Some(token) = take_token(tokens) {
        match token {
            Token::Hyphen => Ok(UnaryOperator::Negate),
            Token::Tilde => Ok(UnaryOperator::Complement),
            Token::LogicalNot => Ok(UnaryOperator::LogicalNot),
            _ => Err(ParseError::ExpectedButGot(Token::Hyphen, token)),
        }
    } else {
        Err(ParseError::ExpectedButGotNone(Token::Hyphen))
    }
}

fn take_token(tokens: &mut Tokens) -> Option<Token> {
    tokens.pop_front()
}

fn parse_binop(tokens: &mut Tokens) -> Result<BinaryOp, ParseError> {
    if tokens.is_empty() {
        return Err(ParseError::ExpectedExpression);
    }
    let next_token = take_token(tokens).expect("Should never fail");

    match next_token {
        Token::Plus => Ok(BinaryOp::Add),
        Token::Hyphen => Ok(BinaryOp::Substract),
        Token::FSlash => Ok(BinaryOp::Div),
        Token::Percent => Ok(BinaryOp::Mod),
        Token::Asterisk => Ok(BinaryOp::Multiply),
        Token::LogicalAnd => Ok(BinaryOp::LogicalAnd),
        Token::LogicalOr => Ok(BinaryOp::LogicalOr),
        Token::IsEqual => Ok(BinaryOp::IsEqual),
        Token::IsNotEqual => Ok(BinaryOp::IsNotEqual),
        Token::IsLessThan => Ok(BinaryOp::LessThan),
        Token::IsLessThanOrEqual => Ok(BinaryOp::LessOrEqual),
        Token::IsGreaterThan => Ok(BinaryOp::GreaterThan),
        Token::IsGreaterThanOrEqual => Ok(BinaryOp::GreaterOrEqual),
        _ => Err(ParseError::BadExpression(next_token.clone())),
    }
}

fn token_is_binaryop(token: &Token) -> bool {
    match token {
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
        | Token::IsGreaterThanOrEqual => true,
        _ => false,
    }
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
        _ => 0
    }
}

fn parse_exp(tokens: &mut Tokens, min_prec: u64) -> Result<Expression, ParseError> {
    let mut left = parse_factor(tokens)?;
    while !tokens.is_empty() {
        let next_token = tokens.front().expect("Should never fail").clone();
        let prec = get_prec(&next_token);
        if !(token_is_binaryop(&next_token) && prec >= min_prec) {
            break;
        }

        let operator = parse_binop(tokens)?;
        let right = parse_exp(tokens, prec + 1)?;

        left = Expression::Binary(operator, Box::new(left), Box::new(right));
    }
    Ok(left)
}

fn parse_factor(tokens: &mut Tokens) -> Result<Expression, ParseError> {
    if tokens.is_empty() {
        return Err(ParseError::ExpectedExpression);
    }
    let next_token = tokens.front().expect("Should never fail");

    match next_token {
        Token::Tilde | Token::Hyphen | Token::LogicalNot => {
            let operator = parse_unary(tokens)?;
            let inner_exp = Box::new(parse_factor(tokens)?);
            Ok(Expression::Unary(operator, inner_exp))
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
            Ok(Expression::Constant(inner))
        }
        _ => Err(ParseError::BadExpression(next_token.clone())),
    }
}

fn parse_statement(tokens: &mut Tokens) -> Result<Statement, ParseError> {
    expect_token(tokens, Token::Return)?;
    let exp = parse_exp(tokens, 0)?;
    expect_token(tokens, Token::Semicolon)?;
    Ok(Statement::Return(exp))
}

fn parse_function(tokens: &mut Tokens) -> Result<Function, ParseError> {
    expect_token(tokens, Token::Int)?;
    let identifier = expect_identifier(tokens)?;
    expect_token(tokens, Token::OpenParanth)?;
    expect_token(tokens, Token::Void)?;
    expect_token(tokens, Token::CloseParanth)?;
    expect_token(tokens, Token::OpenCurly)?;
    let statement = parse_statement(tokens)?;
    expect_token(tokens, Token::CloseCurly)?;
    if !tokens.is_empty() {
        Err(ParseError::MoreTokensThanExpected(tokens.clone()))
    } else {
        Ok(Function {
            name: identifier,
            body: statement,
        })
    }
}

fn parse_program(tokens: &mut Tokens) -> Result<Program, ParseError> {
    Ok(Program::FunDef(parse_function(tokens)?))
}

pub fn parse(mut tokens: Tokens) -> Result<Program, ParseError> {
    Ok(parse_program(&mut tokens)?)
}

