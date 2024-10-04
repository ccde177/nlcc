use crate::lexer::{Token, Tokens};
use std::fmt;

type Identifier = String;

#[derive(Clone)]
pub enum Program {
    FunDef(Function),
}

#[derive(Clone)]
pub struct Function {
    pub name: Identifier,
    pub body: Statement,
}

#[derive(Clone)]
pub enum Statement {
    Return(Expression),
}

#[derive(Clone)]
pub enum Factor {
    Constant(u64),
    Unary(UnaryOperator, Box<Factor>),
    Nested(Box<Expression>)
}

#[derive(Clone)]
pub enum Expression {
    Factor(Box<Factor>),
    BinaryExpr(Box<Expression>, BinaryOp, Box<Expression>)
}

#[derive(Copy, Clone)]
pub enum BinaryOp {
    Plus,
    Mul,
    Div,
    Mod,
    Minus
}

#[derive(Clone)]
pub enum UnaryOperator {
    Complement,
    Negate,
}

#[derive(Debug)]
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
    tokens.pop().map_or(
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
            _ => Err(ParseError::ExpectedButGot(Token::Hyphen, token)),
        }
    } else {
        Err(ParseError::ExpectedButGotNone(Token::Hyphen))
    }
}

fn take_token(tokens: &mut Tokens) -> Option<Token> {
    tokens.pop()
}

fn parse_binop(tokens: &mut Tokens) -> Result<BinaryOp, ParseError> {
    if tokens.is_empty() {
	return Err(ParseError::ExpectedExpression);
    }
    let next_token = take_token(tokens).expect("Should never fail");

    match next_token {
	Token::Plus => Ok(BinaryOp::Plus),
	Token::Hyphen => Ok(BinaryOp::Minus),
	Token::FSlash => Ok(BinaryOp::Div),
	Token::Percent => Ok(BinaryOp::Mod),
	Token::Asterisk => Ok(BinaryOp::Mul),
	_ => Err(ParseError::BadExpression(next_token.clone()))
    }
}

fn parse_exp(tokens: &mut Tokens, min_prec: u64) -> Result<Expression, ParseError> {
    let mut left = Expression::Factor(Box::new(parse_factor(tokens)?));
    dbg!("parse_exp called");
    while !tokens.is_empty() {
	let next_token = tokens.last().expect("Should never fail").clone();
	let prec = next_token.get_prec();
	dbg!(next_token.clone());
	dbg!(next_token.is_binary());
	if !(next_token.is_binary() && prec >= min_prec) {
	    break;
	}

	let operator = parse_binop(tokens)?;
	let right = parse_exp(tokens, next_token.get_prec() + 1)?;

	left = Expression::BinaryExpr(Box::new(left), operator, Box::new(right));
	
    }
    Ok(left)
}

fn parse_factor(tokens: &mut Tokens) -> Result<Factor, ParseError> {
    if tokens.is_empty() {
	return Err(ParseError::ExpectedExpression);
    }
    let next_token = tokens.last().expect("Should never fail");
    
    match next_token {
	Token::Tilde | Token::Hyphen => {
	    let operator = parse_unary(tokens)?;
	    let inner_exp = Box::new(parse_factor(tokens)?);
	    Ok(Factor::Unary(operator, inner_exp))
	}
	Token::OpenParanth => {
	    take_token(tokens);
	    let inner_exp = Box::new(parse_exp(tokens, 0)?);
	    expect_token(tokens, Token::CloseParanth)?;
	    Ok(Factor::Nested(inner_exp))
	}
	Token::Constant(i) => {
	    let inner = *i;
	    take_token(tokens);
	    Ok(Factor::Constant(inner))
	}
	_ => Err(ParseError::BadExpression(next_token.clone()))
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
    tokens.reverse();
    Ok(parse_program(&mut tokens)?)
}
