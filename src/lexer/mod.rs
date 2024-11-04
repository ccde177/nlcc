mod cursor;
mod lexer_error;
#[cfg(test)]
mod lexer_tests;
mod token;

use cursor::Cursor;
pub use lexer_error::{LexError, Result};
pub use token::Token;

pub type Tokens = Vec<Token>;

fn lex_mcharop3(first: char, second: char, third: char) -> Result<Token> {
    match (first, second, third) {
        ('>', '>', '=') => Ok(Token::AssignShr),
        ('<', '<', '=') => Ok(Token::AssignShl),
        _ => Err(LexError::BadMcharOperator(format!(
            "{first}{second}{third}"
        ))),
    }
}

fn lex_mcharop2(first: char, second: char) -> Result<Token> {
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
        _ => Err(LexError::BadMcharOperator(format!("{first}{second}"))),
    }
}

fn lex_mcharoperator(cursor: &mut Cursor) -> Result<Token> {
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
    let mut len = 0;

    let predicate = |c: char| c.is_ascii_alphanumeric() || c == '_';
    while cursor.skip_if(predicate) {
        len += 1;
    }

    Token::from(&start[..len])
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
