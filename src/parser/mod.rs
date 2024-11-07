mod cursor;
mod parse_error;
#[cfg(test)]
mod parser_tests;

use crate::ast::*;
use crate::lexer::{LinedToken, Token};
use cursor::Cursor;
pub use parse_error::{InnerParseError, ParseError, Result};

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
    let line = cursor.get_line();
    let next = cursor.next_or_error()?;
    if let Token::Identifier(name) = next {
        Ok(name.to_owned())
    } else {
        Err(InnerParseError::ExpectedIdentifierButGot(next.clone()).set_line(line))
    }
}

fn parse_type_specifiers(cursor: &mut Cursor) -> Result<Type> {
    let mut types = Vec::new();
    let mut sign = None;
    let predicate = |t: &Token| t.is_type_specifier() || t.is_sign_specifier();
    let line = cursor.get_line();
    while let Some(next) = cursor.next_if(predicate).cloned() {
        if next.is_sign_specifier() {
            let line = cursor.get_line();
            set_sign_specifier(&mut sign, next).map_err(|err| err.set_line(line))?;
        } else {
            types.push(next);
        }
    }
    parse_type(&types, sign).map_err(|err| err.set_line(line))
}

fn parse_params(cursor: &mut Cursor) -> Result<(Vec<Type>, Vec<Identifier>)> {
    let mut params = Vec::new();
    let mut ptypes = Vec::new();

    //TODO: Maybe add || cursor.bump_if(&Token::CloseParanth)
    let void = cursor.bump_if(&Token::Void);

    if void {
        return Ok((ptypes, params));
    }

    let mut expect_more = false;
    while cursor.peek_or_error()?.is_type() {
        let ptype = parse_type_specifiers(cursor)?;
        ptypes.push(ptype);
        let parameter = parse_identifier(cursor)?;
        params.push(parameter);
        expect_more = cursor.bump_if(&Token::Comma);
    }

    if expect_more {
        let line = cursor.get_line();
        return Err(InnerParseError::TrailingComma.set_line(line));
    }

    Ok((ptypes, params))
}

fn parse_conditional_middle(cursor: &mut Cursor) -> Result<Exp> {
    cursor.expect(&Token::QuestionMark)?;
    let exp = parse_exp(cursor, 0)?;
    cursor.expect(&Token::Colon)?;
    Ok(exp)
}

impl TryFrom<&Token> for AstBinaryOp {
    type Error = InnerParseError;
    fn try_from(value: &Token) -> std::result::Result<Self, InnerParseError> {
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
            _ => Err(InnerParseError::UnexpectedToken(value.clone())),
        }
    }
}

fn parse_exp_conditional(cursor: &mut Cursor, prec: u64, left: Exp) -> Result<Exp> {
    let then = parse_conditional_middle(cursor).map(Box::new)?;
    let els = parse_exp(cursor, prec).map(Box::new)?;
    let conditional = ConditionalExp {
        condition: Box::new(left),
        then,
        els,
    };

    Ok(Exp::conditional(conditional))
}

fn parse_exp_compassign(cursor: &mut Cursor, prec: u64, left: Exp) -> Result<Exp> {
    let t = cursor.next_or_error()?;
    if !t.is_compound_assign() {
        let t = t.clone();
        let line = cursor.get_line();
        return Err(InnerParseError::UnexpectedToken(t).set_line(line));
    }

    let op = t.compound_to_single();
    let op = AstBinaryOp::try_from(&op);
    let line = cursor.get_line();
    let op = op.map_err(|err| err.set_line(line))?;

    let right = parse_exp(cursor, prec).map(Box::new)?;
    let operation = Exp::binary(op, Box::new(left.clone()), right);
    Ok(Exp::assignment(Box::new(left), Box::new(operation)))
}

fn parse_exp_assign(cursor: &mut Cursor, prec: u64, left: Exp) -> Result<Exp> {
    cursor.expect(&Token::Assign)?;
    let right = parse_exp(cursor, prec).map(Box::new)?;
    let left = Box::new(left);
    Ok(Exp::assignment(left, right))
}

fn parse_exp_postfixop(cursor: &mut Cursor, _prec: u64, left: Exp) -> Result<Exp> {
    let op = parse_postfixop(cursor)?;
    let left = Box::new(left);
    Ok(Exp::unary(op, left))
}

fn parse_binary_op(cursor: &mut Cursor) -> Result<AstBinaryOp> {
    let next = cursor.next_or_error()?;
    let next = AstBinaryOp::try_from(next);
    let line = cursor.get_line();
    next.map_err(|err| err.set_line(line))
}

fn parse_exp_binary(cursor: &mut Cursor, prec: u64, left: Exp) -> Result<Exp> {
    let op = parse_binary_op(cursor)?;
    let right = parse_exp(cursor, prec + 1).map(Box::new)?;
    Ok(Exp::binary(op, Box::new(left), right))
}

fn parse_exp(cursor: &mut Cursor, min_prec: u64) -> Result<Exp> {
    let mut left = parse_factor(cursor)?;

    while let Some(next_token) = cursor.peek().filter(|t| t.is_binaryop()) {
        let prec = get_prec(next_token);
        if prec < min_prec {
            break;
        }
        left = match next_token {
            t if t.is_compound_assign() => parse_exp_compassign(cursor, prec, left)?,
            Token::QuestionMark => parse_exp_conditional(cursor, prec, left)?,
            Token::Assign => parse_exp_assign(cursor, prec, left)?,
            Token::Increment | Token::Decrement => parse_exp_postfixop(cursor, prec, left)?,
            _ => parse_exp_binary(cursor, prec, left)?,
        };
    }

    Ok(left)
}

fn parse_signed_type(
    types: &[Token],
    is_specifier_present: bool,
) -> std::result::Result<Type, InnerParseError> {
    match types {
        [Token::Long, Token::Int] | [Token::Int, Token::Long] | [Token::Long] => Ok(Type::Long),
        [Token::Int] => Ok(Type::Int),
        [] if is_specifier_present => Ok(Type::Int),
        _ => Err(InnerParseError::InvalidTypeSpecifiers(types.to_vec())),
    }
}

fn parse_unsigned_type(types: &[Token]) -> std::result::Result<Type, InnerParseError> {
    match types {
        [Token::Long, Token::Int] | [Token::Int, Token::Long] | [Token::Long] => Ok(Type::ULong),
        [Token::Int] | [] => Ok(Type::UInt),
        _ => Err(InnerParseError::InvalidTypeSpecifiers(types.to_vec())),
    }
}

fn parse_storage_class(classes: &[Token]) -> std::result::Result<StorageClass, InnerParseError> {
    match classes {
        [Token::Extern] => Ok(StorageClass::Extern),
        [Token::Static] => Ok(StorageClass::Static),
        [] => Ok(StorageClass::Auto),
        _ => Err(InnerParseError::InvalidStorageClass(classes.to_vec())),
    }
}

fn set_sign_specifier(
    sign_specifier: &mut Option<Token>,
    next: Token,
) -> std::result::Result<(), InnerParseError> {
    use InnerParseError::DuplicateSignSpecifiers as SignSpecErr;
    if let Some(previous) = sign_specifier {
        return Err(SignSpecErr(previous.clone(), next));
    } else {
        *sign_specifier = Some(next);
    }
    Ok(())
}

fn parse_type(types: &[Token], sign: Option<Token>) -> std::result::Result<Type, InnerParseError> {
    if matches!(types, [Token::Double]) {
        return sign
            .is_none()
            .then_some(Type::Double)
            .ok_or_else(|| InnerParseError::InvalidTypeSpecifiers(types.to_vec()));
    }

    match sign {
        Some(Token::Unsigned) => parse_unsigned_type(types),
        Some(Token::Signed) => parse_signed_type(types, true),
        _ => parse_signed_type(types, false),
    }
}

fn parse_specifiers(cursor: &mut Cursor) -> Result<(Type, StorageClass)> {
    let mut types = Vec::new();
    let mut storage_classes = Vec::new();
    let mut sign = None;
    while let Some(token) = cursor.next_if(Token::is_specifier).cloned() {
        if token.is_sign_specifier() {
            let line = cursor.get_line();
            set_sign_specifier(&mut sign, token).map_err(|err| err.set_line(line))?;
        } else if token.is_type_specifier() {
            types.push(token);
        } else {
            storage_classes.push(token);
        }
    }
    let line = cursor.get_line();
    let set_line = |err: InnerParseError| err.set_line(line);
    let rtype = parse_type(&types, sign).map_err(set_line)?;
    let storage_class = parse_storage_class(&storage_classes).map_err(set_line)?;

    Ok((rtype, storage_class))
}

#[allow(clippy::single_match_else)]
fn parse_forinit(cursor: &mut Cursor) -> Result<AstForInit> {
    let peek = cursor.peek_or_error()?;
    if peek.is_specifier() {
        let dec = parse_declaration(cursor)?;
        let line = cursor.get_line();
        match dec {
            Declaration::Var(vardec) => Ok(AstForInit::InitDecl(vardec)),
            Declaration::Fun(_) => Err(InnerParseError::BadForInit.set_line(line)),
        }
    } else {
        let exp = parse_optional_exp(cursor, &Token::Semicolon)?;
        Ok(AstForInit::InitExp(exp))
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
        label,
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
    let cased_statement = CasedStatement { exp, body, label };

    Ok(Statement::Cased(cased_statement))
}

fn parse_default_case(cursor: &mut Cursor) -> Result<Statement> {
    cursor.expect(&Token::KwDefault)?;
    cursor.expect(&Token::Colon)?;
    let body = parse_statement(cursor).map(Box::new)?;
    let label = String::new();
    let dcs = DCasedStatement { body, label };

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
    type Error = InnerParseError;
    fn try_from(value: &Token) -> std::result::Result<Self, InnerParseError> {
        match value {
            Token::Hyphen => Ok(Self::Negate),
            Token::Tilde => Ok(Self::Complement),
            Token::LogicalNot => Ok(Self::LogicalNot),
            Token::Increment => Ok(Self::PrefixIncrement),
            Token::Decrement => Ok(Self::PrefixDecrement),
            _ => Err(InnerParseError::BadUnaryOp(value.clone())),
        }
    }
}

fn parse_unary_operation(cursor: &mut Cursor) -> Result<Exp> {
    let next = cursor.next_or_error()?;
    let op = AstUnaryOp::try_from(next);
    let line = cursor.get_line();
    let op = op.map_err(|err| err.set_line(line))?;
    let inner = parse_factor(cursor).map(Box::new)?;

    Ok(Exp::unary(op, inner))
}

fn parse_arguments(cursor: &mut Cursor) -> Result<Vec<Exp>> {
    let mut args = Vec::new();

    while !cursor.peek_is(&Token::CloseParanth) {
        let comma = cursor.bump_if(&Token::Comma);
        if comma && args.is_empty() {
            let line = cursor.get_line();
            return Err(InnerParseError::TrailingComma.set_line(line));
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
    Ok(Exp::call(name, arguments))
}

fn parse_factor_postfixop(cursor: &mut Cursor, inner: Exp) -> Result<Exp> {
    let op = parse_postfixop(cursor)?;
    let inner = Box::new(inner);
    Ok(Exp::unary(op, inner))
}

fn parse_factor_identifier(cursor: &mut Cursor) -> Result<Exp> {
    let name = parse_identifier(cursor)?;
    let var = Exp::var(name.clone());
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
        Some(Token::Increment | Token::Decrement) => parse_factor_postfixop(cursor, exp),
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

fn parse_typecast_or_subexp(cursor: &mut Cursor) -> Result<Exp> {
    let peek = cursor.peek_nth_or_error(1)?;
    if peek.is_type() {
        parse_typecast(cursor)
    } else {
        parse_factor_subexp(cursor)
    }
}

fn parse_typecast(cursor: &mut Cursor) -> Result<Exp> {
    cursor.expect(&Token::OpenParanth)?;
    let rtype = parse_type_specifiers(cursor)?;
    cursor.expect(&Token::CloseParanth)?;

    let subexp = parse_factor(cursor).map(Box::new)?;
    Ok(Exp::cast(rtype, subexp))
}

fn parse_factor(cursor: &mut Cursor) -> Result<Exp> {
    let peek = cursor.peek_or_error()?;
    match peek {
        Token::Identifier(_) => parse_factor_identifier(cursor),
        Token::OpenParanth => parse_typecast_or_subexp(cursor),
        Token::UnsignedLConst(u) => {
            let constant = Exp::constant(AstConst::ULong(*u));
            cursor.bump();
            Ok(constant)
        }
        Token::UnsignedConst(u) => {
            let constant = Exp::constant_from_unsigned(*u);
            cursor.bump();
            Ok(constant)
        }
        Token::Constant(i) => {
            let constant = Exp::constant_from_signed(*i);
            cursor.bump();
            Ok(constant)
        }
        Token::LConstant(i) => {
            let constant = Exp::constant(AstConst::Long(*i));
            cursor.bump();
            Ok(constant)
        }
        Token::FPDouble(f) => {
            let constant = Exp::constant(AstConst::Double(*f));
            cursor.bump();
            Ok(constant)
        }
        t if t.is_unaryop() => parse_unary_operation(cursor),
        _ => {
            let peek = peek.clone();
            let line = cursor.get_line();
            Err(InnerParseError::BadFactor(peek).set_line(line))
        }
    }
}

fn parse_statement(cursor: &mut Cursor) -> Result<Statement> {
    let peek = cursor.peek_or_error()?;
    match peek {
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
    let peek = cursor.peek_or_error()?;
    match peek {
        t if t.is_specifier() => parse_declaration(cursor).map(AstBlockItem::D),
        _ => parse_statement(cursor).map(AstBlockItem::S),
    }
}

fn parse_block(cursor: &mut Cursor) -> Result<AstBlock> {
    let mut items = Vec::new();

    cursor.expect(&Token::OpenCurly)?;

    while !cursor.bump_if(&Token::CloseCurly) {
        let item = parse_block_item(cursor)?;
        items.push(item);
    }

    Ok(AstBlock { items })
}

fn parse_declaration(cursor: &mut Cursor) -> Result<Declaration> {
    let (rtype, storage_class) = parse_specifiers(cursor)?;
    let name = parse_identifier(cursor)?;
    let next = cursor.next_or_error()?;

    match next {
        Token::Assign => {
            let exp = parse_exp(cursor, 0)?;
            cursor.expect(&Token::Semicolon)?;
            Ok(Declaration::Var(VarDec {
                init: Some(exp),
                name,
                storage_class,
                var_type: rtype,
            }))
        }
        Token::Semicolon => Ok(Declaration::Var(VarDec {
            init: None,
            name,
            storage_class,
            var_type: rtype,
        })),
        Token::OpenParanth => {
            let (ptypes, params) = parse_params(cursor)?;
            cursor.expect(&Token::CloseParanth)?;
            let has_body = !cursor.bump_if(&Token::Semicolon);
            let body = has_body.then(|| parse_block(cursor)).transpose()?;
            let fun_type = Type::Fun {
                ptypes,
                return_type: Box::new(rtype),
            };
            Ok(Declaration::Fun(FunDec {
                name,
                params,
                body,
                storage_class,
                fun_type,
            }))
        }
        _ => {
            let next = next.clone();
            let line = cursor.get_line();
            Err(InnerParseError::UnexpectedToken(next.clone()).set_line(line))
        }
    }
}

pub fn parse(tokens: &[LinedToken]) -> Result<Ast> {
    let mut declarations = Vec::new();
    let mut cursor = Cursor::new(tokens);

    while !cursor.at_end() {
        let f = parse_declaration(&mut cursor)?;
        declarations.push(f);
    }
    Ok(Ast { declarations })
}
