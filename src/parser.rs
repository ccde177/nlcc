use crate::lexer::{Token, Tokens};
use std::fmt;

type Identifier = String;

pub enum Program {
    FunDef(Function),
}

pub struct Function {
    pub name: Identifier,
    pub body: Statement,
}

pub enum Statement {
    Return(Expression),
}

pub enum Expression {
    Constant(u64),
}

#[derive(Debug)]
pub enum ParseError {
    ExpectedButGot(Token, Token),
    ExpectedButGotNone(Token),
    MoreTokensThanExpected(Tokens),
    ExpectedConstant
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::ExpectedButGot(t1, t2) => {
                write!(f, "Expected token of type {:?} but got {:?}", t2, t1)
            }
            Self::ExpectedButGotNone(t) => write!(f, "Expected token of type {:?} but got None", t),
            Self::MoreTokensThanExpected(ts) => write!(f, "Trailing tokens: {:?}", ts),
            Self::ExpectedConstant => write!(f, "Expected constant"),
        }
    }
}

impl std::error::Error for ParseError {}


fn expect_token(tokens: &mut Tokens, token: Token) -> Result<(), ParseError> {
    tokens
        .pop()
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
    tokens.pop()
        .map_or(Err(ParseError::ExpectedButGotNone(dummy.clone())), |t| {
            match t {
                Token::Identifier(i) => Ok(i),
                _ => Err(ParseError::ExpectedButGot(dummy.clone(), t.clone())),
    }})
}
fn expect_constant(tokens: &mut Tokens) -> Result<u64, ParseError> { 
    tokens.pop().and_then(|t| match t {
        Token::Constant(i) => Some(i),
        _ => None
    }).ok_or(ParseError::ExpectedConstant)
}

fn parse_expresssion(tokens: &mut Tokens) -> Result<Expression, ParseError> {
    let constant = expect_constant(tokens)?;
    Ok(Expression::Constant(constant))
}

fn parse_statement(tokens: &mut Tokens) -> Result<Statement, ParseError> {
    expect_token(tokens, Token::Return)?;
    let exp = parse_expresssion(tokens)?;
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
        Ok(Function { name: identifier, body: statement })
    }
}
fn parse_program(tokens: &mut Tokens) -> Result<Program, ParseError> {
    Ok(Program::FunDef(parse_function(tokens)?))
}
pub fn parse(mut tokens: Tokens) -> Result<Program, ParseError> {
    tokens.reverse();
    Ok(parse_program(&mut tokens)?)
}
