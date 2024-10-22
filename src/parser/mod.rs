mod cursor;
#[cfg(test)]
mod parser_tests;

use crate::ast::*;
use crate::lexer::Token;
use cursor::Cursor;

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
        }
    }
}

impl std::error::Error for ParseError {}

#[allow(clippy::match_same_arms)]
fn get_prec(token: &Token) -> u64 {
    match token {
        Token::Asterisk => 50,
        Token::FSlash => 50,
        Token::Percent => 50,
        Token::Plus => 45,
        Token::Hyphen => 45,
        Token::ShiftLeft => 40,
        Token::ShiftRight => 40,
        Token::IsLessThan => 35,
        Token::IsLessThanOrEqual => 35,
        Token::IsGreaterThan => 35,
        Token::IsGreaterThanOrEqual => 35,
        Token::IsEqual => 30,
        Token::IsNotEqual => 30,
        Token::BitwiseAnd => 25,
        Token::BitwiseXor => 20,
        Token::BitwiseOr => 15,
        Token::LogicalAnd => 10,
        Token::LogicalOr => 5,
        Token::QuestionMark => 3,
        Token::Assign => 1,
        Token::AssignAdd
        | Token::AssignSub
        | Token::AssignMul
        | Token::AssignDiv
        | Token::AssignMod
        | Token::AssignAnd
        | Token::AssignOr
        | Token::AssignXor
        | Token::AssignShl
        | Token::AssignShr => 1,
        Token::Increment | Token::Decrement => 80,
        _ => 0,
    }
}

fn parse_identifier(cursor: &mut Cursor) -> Result<Identifier> {
    let next = cursor.next_or_error()?;
    if let Token::Identifier(name) = next {
        Ok(name.to_owned())
    } else {
        Err(ParseError::ExpectedIdentifierButGot(next.clone()))
    }
}

fn parse_parameter(cursor: &mut Cursor) -> Result<Identifier> {
    cursor.expect(&Token::Int)?;
    let name = parse_identifier(cursor)?;
    Ok(name)
}

fn parse_params(cursor: &mut Cursor) -> Result<Vec<Identifier>> {
    let mut params = Vec::new();

    let void = cursor.bump_if(&Token::Void);

    if void {
        return Ok(params);
    }

    let mut expect_more = false;
    while let Token::Int = cursor.peek_or_error()? {
        let parameter = parse_parameter(cursor)?;
        params.push(parameter);
        expect_more = cursor.bump_if(&Token::Comma);
    }

    if expect_more {
        return Err(ParseError::TrailingComma);
    }

    Ok(params)
}

fn parse_vardec(cursor: &mut Cursor) -> Result<VarDec> {
    cursor.expect(&Token::Int)?;
    let name = parse_identifier(cursor)?;
    let assign = cursor.bump_if(&Token::Assign);
    let init = assign.then(|| parse_exp(cursor, 0)).transpose()?;
    cursor.expect(&Token::Semicolon)?;

    Ok(VarDec { name, init })
}

fn parse_conditional_middle(cursor: &mut Cursor) -> Result<Exp> {
    cursor.expect(&Token::QuestionMark)?;
    let exp = parse_exp(cursor, 0)?;
    cursor.expect(&Token::Colon)?;
    Ok(exp)
}

impl TryFrom<&Token> for AstBinaryOp {
    type Error = ParseError;
    fn try_from(value: &Token) -> Result<Self> {
        match value {
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
            Token::BitwiseAnd => Ok(AstBinaryOp::BitwiseAnd),
            Token::BitwiseOr => Ok(AstBinaryOp::BitwiseOr),
            Token::BitwiseXor => Ok(AstBinaryOp::BitwiseXor),
            Token::ShiftLeft => Ok(AstBinaryOp::ShiftLeft),
            Token::ShiftRight => Ok(AstBinaryOp::ShiftRight),
            _ => Err(ParseError::UnexpectedToken(value.clone())),
        }
    }
}

fn parse_exp_conditional(cursor: &mut Cursor, prec: u64, left: Exp) -> Result<Exp> {
    let then = parse_conditional_middle(cursor).map(Box::new)?;
    let els = parse_exp(cursor, prec).map(Box::new)?;
    let conditional = ConditionalExp {
        condition: Box::new(left),
        then,
        els
    };
    
    Ok(Exp::Conditional(conditional))
}

fn parse_exp_compassign(cursor: &mut Cursor, prec: u64, left: Exp) -> Result<Exp> {
    let t = cursor.next_or_error()?;
    if !t.is_compound_assign() {
        return Err(ParseError::UnexpectedToken(t.clone()));
    }
    let op = t.compound_to_single();
    let op = AstBinaryOp::try_from(&op)?;
    let right = parse_exp(cursor, prec).map(Box::new)?;
    let operation = Exp::Binary(op, Box::new(left.clone()), right);
    Ok(Exp::Assignment(Box::new(left), Box::new(operation)))
}

fn parse_exp_assign(cursor: &mut Cursor, prec: u64, left: Exp) -> Result<Exp> {
    cursor.expect(&Token::Assign)?;
    let right = parse_exp(cursor, prec).map(Box::new)?;
    let left = Box::new(left);
    Ok(Exp::Assignment(left, right))
}

fn parse_exp_postfixop(cursor: &mut Cursor, _prec: u64, left: Exp) -> Result<Exp> {
    let op = parse_postfixop(cursor)?;
    let left = Box::new(left);
    Ok(Exp::Unary(op, left))
}

fn parse_binary_op(cursor: &mut Cursor) -> Result<AstBinaryOp> {
    let next = cursor.next_or_error()?;
    AstBinaryOp::try_from(next)
}

fn parse_exp_binary(cursor: &mut Cursor, prec: u64, left: Exp) -> Result<Exp> {
    let op = parse_binary_op(cursor)?;
    let right = parse_exp(cursor, prec + 1).map(Box::new)?;
    Ok(Exp::Binary(op, Box::new(left), right))
}

fn parse_exp(cursor: &mut Cursor, min_prec: u64) -> Result<Exp> {
    let mut left = parse_factor(cursor)?;
    
    while let Some(next_token) = cursor.peek() {
        let prec = get_prec(next_token);
        if !next_token.is_binaryop() || prec < min_prec {
            break;
        }
        match next_token {
            t if t.is_compound_assign() => {
                left = parse_exp_compassign(cursor, prec, left)?;
            }
            Token::QuestionMark => {
                left = parse_exp_conditional(cursor, prec, left)?;
            }
            Token::Assign => {
                left = parse_exp_assign(cursor, prec, left)?;
            }
            Token::Increment | Token::Decrement => {
                left = parse_exp_postfixop(cursor, prec, left)?;
            }
            _ => {
                left = parse_exp_binary(cursor, prec, left)?;
            }
        }
    }
    
    Ok(left)
}

fn parse_declaration(cursor: &mut Cursor) -> Result<Declaration> {
    let third = cursor.peek_nth_or_error(2)?;
    match third {
        Token::Semicolon | Token::Assign => {
            let vardec = parse_vardec(cursor)?;
            Ok(Declaration::Var(vardec))
        }
        Token::OpenParanth => {
            let fundec = parse_fundec(cursor)?;
            Ok(Declaration::Fun(fundec))
        }
        _ => Err(ParseError::UnexpectedToken(third.clone())),
    }
}

#[allow(clippy::single_match_else)]
fn parse_forinit(cursor: &mut Cursor) -> Result<AstForInit> {
    let next = cursor.peek_or_error()?;
    match next {
        Token::Int => {
            let vardec = parse_vardec(cursor)?;
            Ok(AstForInit::InitDecl(vardec))
        }
        _ => {
            let exp = parse_optional_exp(cursor, &Token::Semicolon)?;
            Ok(AstForInit::InitExp(exp))
        }
    }
}

fn parse_optional_exp(cursor: &mut Cursor, delim: &Token) -> Result<Option<Exp>> {
    let not_met_delim = !cursor.bump_if(delim);
    let exp = not_met_delim.then(|| parse_exp(cursor, 0)).transpose()?;

    if not_met_delim {
        cursor.expect(delim)?;
    }

    Ok(exp)
}

fn parse_for(cursor: &mut Cursor) -> Result<Statement> {
    cursor.expect(&Token::For)?;
    cursor.expect(&Token::OpenParanth)?;
    let init = parse_forinit(cursor)?;
    let condition = parse_optional_exp(cursor, &Token::Semicolon)?;
    let post = parse_optional_exp(cursor, &Token::CloseParanth)?;
    let body = parse_statement(cursor).map(Box::new)?;
    let label = String::new();
    let for_st = For {
        init,
        condition,
        post,
        body,
        label,
    };

    Ok(Statement::For(for_st))
}

fn parse_if(cursor: &mut Cursor) -> Result<Statement> {
    cursor.expect(&Token::If)?;
    cursor.expect(&Token::OpenParanth)?;
    let condition = parse_exp(cursor, 0)?;
    cursor.expect(&Token::CloseParanth)?;
    let then = parse_statement(cursor).map(Box::new)?;
    let else_present = cursor.bump_if(&Token::Else);
    let els = else_present
        .then(|| parse_statement(cursor))
        .transpose()?
        .map(Box::new);
    let if_st = If {
        condition,
        then,
        els,
    };
    
    Ok(Statement::If(if_st))
}

fn parse_while(cursor: &mut Cursor) -> Result<Statement> {
    cursor.expect(&Token::While)?;
    cursor.expect(&Token::OpenParanth)?;
    let condition = parse_exp(cursor, 0)?;
    cursor.expect(&Token::CloseParanth)?;
    let body = parse_statement(cursor).map(Box::new)?;
    let label = String::new();
    let while_st = While {
        condition,
        body,
        label,
    };
    
    Ok(Statement::While(while_st))
}

fn parse_dowhile(cursor: &mut Cursor) -> Result<Statement> {
    cursor.expect(&Token::Do)?;
    let body = parse_statement(cursor).map(Box::new)?;
    cursor.expect(&Token::While)?;
    cursor.expect(&Token::OpenParanth)?;
    let condition = parse_exp(cursor, 0)?;
    cursor.expect(&Token::CloseParanth)?;
    cursor.expect(&Token::Semicolon)?;
    let label = String::new();
    let dowhile = DoWhile {
        body,
        condition,
        label
    };
    
    Ok(Statement::DoWhile(dowhile))
}

fn parse_continue(cursor: &mut Cursor) -> Result<Statement> {
    cursor.expect(&Token::Continue)?;
    cursor.expect(&Token::Semicolon)?;
    let label = String::new();
    Ok(Statement::Continue(label))
}

fn parse_break(cursor: &mut Cursor) -> Result<Statement> {
    cursor.expect(&Token::Break)?;
    cursor.expect(&Token::Semicolon)?;
    let label = String::new();
    Ok(Statement::Break(label))
}

fn parse_return(cursor: &mut Cursor) -> Result<Statement> {
    cursor.expect(&Token::Return)?;
    let exp = parse_exp(cursor, 0)?;
    cursor.expect(&Token::Semicolon)?;
    Ok(Statement::Return(exp))
}

fn parse_switch(cursor: &mut Cursor) -> Result<Statement> {
    cursor.expect(&Token::Switch)?;
    cursor.expect(&Token::OpenParanth)?;
    let ctrl_exp = parse_exp(cursor, 0)?;
    cursor.expect(&Token::CloseParanth)?;
    let body = parse_statement(cursor).map(Box::new)?;
    let cases = Vec::new();
    let label = String::new();
    let switch = Switch {
        ctrl_exp,
        body,
        cases,
        label,
    };
    
    Ok(Statement::Switch(switch))
}

fn parse_case(cursor: &mut Cursor) -> Result<Statement> {
    cursor.expect(&Token::Case)?;
    let exp = parse_exp(cursor, 0)?;
    cursor.expect(&Token::Colon)?;
    let body = parse_statement(cursor).map(Box::new)?;
    let label = String::new();
    let cased_statement = CasedStatement {
        exp,
        body,
        label
    };
    
    Ok(Statement::Cased(cased_statement))
}

fn parse_default_case(cursor: &mut Cursor) -> Result<Statement> {
    cursor.expect(&Token::KwDefault)?;
    cursor.expect(&Token::Colon)?;
    let body = parse_statement(cursor).map(Box::new)?;
    let label = String::new();
    let dcs = DCasedStatement {
        body,
        label,
    };
    
    Ok(Statement::DCased(dcs))
}

fn parse_goto(cursor: &mut Cursor) -> Result<Statement> {
    cursor.expect(&Token::Goto)?;
    let label = parse_identifier(cursor)?;
    cursor.expect(&Token::Semicolon)?;

    Ok(Statement::Goto(label))
}

fn parse_labeled_statement(cursor: &mut Cursor) -> Result<Statement> {
    let name = parse_identifier(cursor)?;
    cursor.expect(&Token::Colon)?;
    let statement = parse_statement(cursor).map(Box::new)?;

    Ok(Statement::Labeled(name, statement))
}

fn parse_statement_exp(cursor: &mut Cursor) -> Result<Statement> {
    let exp = parse_exp(cursor, 0)?;
    cursor.expect(&Token::Semicolon)?;

    Ok(Statement::Exp(exp))
}

fn parse_statement_label_or_exp(cursor: &mut Cursor) -> Result<Statement> {
    let second = cursor.peek_nth_or_error(1)?;
    if let Token::Colon = second {
        parse_labeled_statement(cursor)
    } else {
        parse_statement_exp(cursor)
    }
}

impl TryFrom<&Token> for AstUnaryOp {
    type Error = ParseError;
    fn try_from(value: &Token) -> Result<Self> {
        match value {
            Token::Hyphen => Ok(Self::Negate),
            Token::Tilde => Ok(Self::Complement),
            Token::LogicalNot => Ok(Self::LogicalNot),
            Token::Increment => Ok(Self::PrefixIncrement),
            Token::Decrement => Ok(Self::PrefixDecrement),
            _ => Err(ParseError::BadUnaryOp(value.clone())),
        }
    }
}

fn parse_unary_operation(cursor: &mut Cursor) -> Result<Exp> {
    let next = cursor.next_or_error()?;
    let op = AstUnaryOp::try_from(next)?;
    let inner = parse_factor(cursor).map(Box::new)?;

    Ok(Exp::Unary(op, inner))
}

fn parse_arguments(cursor: &mut Cursor) -> Result<Vec<Exp>> {
    let mut args = Vec::new();

    while cursor.peek_or_error()? != &Token::CloseParanth {
        let comma = cursor.bump_if(&Token::Comma);
        if comma && args.is_empty() {
            return Err(ParseError::TrailingComma);
        }
        let exp = parse_exp(cursor, 0)?;
        args.push(exp);
    }

    Ok(args)
}

fn parse_factor_call(cursor: &mut Cursor, name: String) -> Result<Exp> {
    cursor.expect(&Token::OpenParanth)?;
    let arguments = parse_arguments(cursor)?;
    cursor.expect(&Token::CloseParanth)?;
    Ok(Exp::Call(name, arguments))
}

fn parse_factor_postfixop(cursor: &mut Cursor, inner: Exp) -> Result<Exp> {
    let op = parse_postfixop(cursor)?;
    let inner = Box::new(inner);
    Ok(Exp::Unary(op, inner))
}

fn parse_factor_identifier(cursor: &mut Cursor) -> Result<Exp> {
    let name = parse_identifier(cursor)?;
    let var = Exp::Var(name.clone());
    let peek = cursor.peek_or_error()?;
    match peek {
        Token::OpenParanth => parse_factor_call(cursor, name),
        Token::Increment | Token::Decrement => parse_factor_postfixop(cursor, var),
        _ => Ok(var),
    }
}

fn parse_factor_subexp(cursor: &mut Cursor) -> Result<Exp> {
    cursor.expect(&Token::OpenParanth)?;
    let exp = parse_exp(cursor, 0)?;
    cursor.expect(&Token::CloseParanth)?;

    let peek = cursor.peek();
    match peek {
        Some(Token::Increment | Token::Decrement) => {
            parse_factor_postfixop(cursor, exp)
        }
        _ => Ok(exp),
    }
}

fn parse_postfixop(cursor: &mut Cursor) -> Result<AstUnaryOp> {
    let next = cursor.next_or_error()?;
    match next {
        Token::Increment => Ok(AstUnaryOp::PostfixIncrement),
        _ => Ok(AstUnaryOp::PostfixDecrement),
    }
}

fn parse_factor(cursor: &mut Cursor) -> Result<Exp> {
    let peek = cursor.peek_or_error()?;
    match peek {
        Token::Identifier(_) => parse_factor_identifier(cursor),
        Token::OpenParanth => parse_factor_subexp(cursor),
        Token::Constant(u) => {
            let constant = Exp::Constant(*u);
            cursor.bump();
            Ok(constant)
        }
        t if t.is_unaryop() => parse_unary_operation(cursor),
        _ => Err(ParseError::BadFactor(peek.clone())),
    }
}

fn parse_statement(cursor: &mut Cursor) -> Result<Statement> {
    let next = cursor.peek_or_error()?;
    match next {
        Token::Goto => parse_goto(cursor),
        Token::Case => parse_case(cursor),
        Token::KwDefault => parse_default_case(cursor),
        Token::Switch => parse_switch(cursor),
        Token::Continue => parse_continue(cursor),
        Token::Break => parse_break(cursor),
        Token::Return => parse_return(cursor),
        Token::For => parse_for(cursor),
        Token::Do => parse_dowhile(cursor),
        Token::While => parse_while(cursor),
        Token::If => parse_if(cursor),
        Token::Identifier(_) => parse_statement_label_or_exp(cursor),
        Token::Semicolon => {
            cursor.bump();
            Ok(Statement::Null)
        }
        Token::OpenCurly => {
            let block = parse_block(cursor)?;
            Ok(Statement::Compound(block))
        }
        _ => parse_statement_exp(cursor),
    }
}

fn parse_block_item(cursor: &mut Cursor) -> Result<AstBlockItem> {
    let next = cursor.peek_or_error()?;
    match next {
        Token::Int => Ok(AstBlockItem::D(parse_declaration(cursor)?)),
        _ => Ok(AstBlockItem::S(parse_statement(cursor)?)),
    }
}

fn parse_block(cursor: &mut Cursor) -> Result<AstBlock> {
    let mut items = Vec::new();

    cursor.expect(&Token::OpenCurly)?;

    while cursor.peek_or_error()? != &Token::CloseCurly {
        let item = parse_block_item(cursor)?;
        items.push(item);
    }

    cursor.expect(&Token::CloseCurly)?;

    Ok(AstBlock { items })
}

fn parse_fundec(cursor: &mut Cursor) -> Result<FunDec> {
    cursor.expect(&Token::Int)?;
    let name = parse_identifier(cursor)?;

    cursor.expect(&Token::OpenParanth)?;
    let params = parse_params(cursor)?;
    cursor.expect(&Token::CloseParanth)?;

    let semicolon = cursor.bump_if(&Token::Semicolon);
    let body = (!semicolon).then(|| parse_block(cursor)).transpose()?;

    Ok(FunDec { name, params, body })
}

pub fn parse(tokens: &[Token]) -> Result<Ast> {
    let mut functions = Vec::new();
    let mut cursor = Cursor::new(tokens);

    while !cursor.at_end() {
        let f = parse_fundec(&mut cursor)?;
        functions.push(f);
    }
    Ok(Ast { functions })
}
