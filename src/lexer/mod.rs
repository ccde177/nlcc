mod cursor;
mod lexer_error;
#[cfg(test)]
mod lexer_tests;
mod token;

use cursor::Cursor;
pub use lexer_error::{InnerLexError, LexError};
pub use token::{LinedToken, Token};

pub type Tokens = Vec<LinedToken>;

fn lex_mcharop3(first: char, second: char, third: char) -> Result<Token, InnerLexError> {
    match (first, second, third) {
        ('>', '>', '=') => Ok(Token::AssignShr),
        ('<', '<', '=') => Ok(Token::AssignShl),
        _ => Err(InnerLexError::BadMcharOperator(format!(
            "{first}{second}{third}"
        ))),
    }
}

fn lex_mcharop2(first: char, second: char) -> Result<Token, InnerLexError> {
    match (first, second) {
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
        _ => Err(InnerLexError::BadMcharOperator(format!("{first}{second}"))),
    }
}

fn lex_mcharoperator(cursor: &mut Cursor) -> Result<Token, InnerLexError> {
    let first = cursor.take().expect("Is always Some");
    let second = cursor.peek();
    let eq = cursor.peek_2nd().filter(|c| *c == '=');

    if let Some(third) = eq {
        let second = second.expect("Is always Some");
        let result = lex_mcharop3(first, second, third);
        if result.is_ok() {
            cursor.take();
            cursor.take();
            return result;
        }
    }

    if let Some(second) = second {
        let op = lex_mcharop2(first, second);
        if op.is_ok() {
            cursor.take();
        }
        return op.or(Token::try_from(first));
    }

    Token::try_from(first)
}

fn check_const_bad_suffix(cursor: &mut Cursor) -> Result<(), InnerLexError> {
    let is_bad = |c: &char| c.is_alphabetic() || *c == '_';
    if let Some(bad_suffix) = cursor.peek().filter(is_bad) {
        return Err(InnerLexError::BadConstantSuffix(bad_suffix));
    }
    Ok(())
}

fn lex_dconstant_h(
    cursor: &mut Cursor,
    start: &str,
    mut count: usize,
) -> Result<Token, InnerLexError> {
    let mut met_e = false;
    let is_e = |c: char| matches!(c, 'e' | 'E');
    let is_sign = |c: char| matches!(c, '+' | '-');
    let predicate = |c: &char| matches!(c, '0'..='9' | '.') || is_e(*c);

    while let Some(peek) = cursor.peek().filter(predicate) {
        let peek_is_e = is_e(peek);
        if peek_is_e && met_e {
            return Err(InnerLexError::BadFloatingPointConstant(
                start[..count].to_owned(),
            ));
        }
        cursor.take();
        let met_sign = peek_is_e && cursor.skip_if(is_sign);

        met_e = peek_is_e;
        count += 1 + usize::from(met_sign);
    }

    check_const_bad_suffix(cursor)?;

    let f64_result = start[..count]
        .parse::<f64>()
        .map_err(|_| InnerLexError::BadFloatingPointConstant(start[..count].to_owned()))?;

    Ok(Token::FPDouble(f64_result))
}

fn lex_dconstant(cursor: &mut Cursor) -> Result<Token, InnerLexError> {
    let start = cursor.as_str();
    let count = 0;
    lex_dconstant_h(cursor, start, count)
}

fn lex_constant(cursor: &mut Cursor) -> Result<Token, InnerLexError> {
    let start = cursor.as_str();
    let mut count = 0;

    while cursor.skip_if(|c| c.is_ascii_digit()) {
        count += 1;
    }

    let predicate = |c: &char| matches!(c, '.' | 'E' | 'e');
    let is_float = cursor.peek().filter(predicate).is_some();
    if is_float {
        return lex_dconstant_h(cursor, start, count);
    }

    let mut is_long = cursor.bump_if('l') || cursor.bump_if('L');
    let is_unsigned = cursor.bump_if('u') || cursor.bump_if('U');

    //Retry in case of ul/UL/etc
    if !is_long {
        is_long = cursor.bump_if('l') || cursor.bump_if('L');
    }

    check_const_bad_suffix(cursor)?;

    let const_str = &start[..count];

    let constant = if is_unsigned {
        let parsed = const_str.parse::<u64>().expect("Should never fail");
        if is_long {
            Token::UnsignedLConst(parsed)
        } else {
            Token::UnsignedConst(parsed)
        }
    } else {
        let parsed = const_str.parse::<i64>().expect("Should never fail");
        if is_long {
            Token::LConstant(parsed)
        } else {
            Token::Constant(parsed)
        }
    };

    Ok(constant)
}

fn lex_identifier(cursor: &mut Cursor) -> Token {
    let start = cursor.as_str();
    let mut len = 0;

    let predicate = |c: char| c.is_ascii_alphanumeric() || c == '_';
    while cursor.skip_if(predicate) {
        len += 1;
    }

    Token::from(&start[..len])
}

pub fn lex(input: &str) -> Result<Tokens, LexError> {
    let mut tokens = Tokens::with_capacity(input.len());
    let mut cursor = Cursor::new(input);
    cursor.skip_whitespaces();

    while let Some(peek) = cursor.peek() {
        let ln = cursor.get_ln();
        let set_line = |t: Token| LinedToken::new(t, ln);
        let set_err_line = |err: InnerLexError| err.set_line(ln);
        let token = match peek {
            ';' | '{' | '}' | '(' | ')' | '~' | '?' | ':' | ',' => {
                cursor.take();
                Token::try_from(peek)
            }
            '%' | '^' | '/' | '*' | '-' | '+' | '=' | '!' | '>' | '<' | '|' | '&' => {
                lex_mcharoperator(&mut cursor)
            }
            '_' | 'a'..='z' | 'A'..='Z' => Ok(lex_identifier(&mut cursor)),
            '0'..='9' => lex_constant(&mut cursor),
            '.' => lex_dconstant(&mut cursor),
            _ => Err(InnerLexError::UnexpectedChar(peek)),
        }
        .map(set_line)
        .map_err(set_err_line)?;
        tokens.push(token);
        cursor.skip_whitespaces();
    }

    Ok(tokens)
}
