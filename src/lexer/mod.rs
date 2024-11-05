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

fn lex_constant(cursor: &mut Cursor) -> Result<Token, InnerLexError> {
    let start = cursor.as_str();
    let mut count = 0;

    while cursor.skip_if(|c| c.is_ascii_digit()) {
        count += 1;
    }

    let mut is_long = cursor.bump_if('l') || cursor.bump_if('L');
    let is_unsigned = cursor.bump_if('u') || cursor.bump_if('U');

    //Retry in case of ul/UL/etc
    if !is_long {
        is_long = cursor.bump_if('l') || cursor.bump_if('L');
    }

    if let Some(next) = cursor.peek() {
        if next.is_alphabetic() || next == '_' {
            return Err(InnerLexError::BadConstantSuffix(next));
        }
    }

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

    while let Some(next) = cursor.peek() {
        let ln = cursor.get_ln();
        let set_line = |t: Token| LinedToken::new(t, ln);
        let set_err_line = |err: InnerLexError| err.set_line(ln);
        match next {
            ';' | '{' | '}' | '(' | ')' | '~' | '?' | ':' | ',' => {
                let token = Token::try_from(next)
                    .map(set_line)
                    .expect("Should never fail");
                tokens.push(token);
                cursor.take();
            }
            '%' | '^' | '/' | '*' | '-' | '+' | '=' | '!' | '>' | '<' | '|' | '&' => {
                let token = lex_mcharoperator(&mut cursor)
                    .map(set_line)
                    .map_err(set_err_line)?;
                tokens.push(token);
            }
            '_' | 'a'..='z' | 'A'..='Z' => {
                let token = lex_identifier(&mut cursor);
                tokens.push(set_line(token));
            }
            '0'..='9' => {
                let token = lex_constant(&mut cursor)
                    .map(set_line)
                    .map_err(set_err_line)?;
                tokens.push(token);
            }
            _ => return Err(InnerLexError::UnexpectedChar(next).set_line(ln)),
        }
        cursor.skip_whitespaces();
    }
    Ok(tokens)
}
