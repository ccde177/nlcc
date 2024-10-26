use crate::lexer::Token;
use crate::parser::{ParseError, Result};

#[derive(Debug)]
pub struct Cursor<'a> {
    tokens: &'a [Token],
    position: usize,
}

impl<'a> Cursor<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }

    pub fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.position)
    }

    pub fn bump(&mut self) {
        self.position += 1;
    }

    pub fn bump_if(&mut self, t: &Token) -> bool {
        let condition = self.peek() == Some(t);
        if condition {
            self.bump();
        }
        condition
    }

    pub fn at_end(&self) -> bool {
        self.position >= self.tokens.len()
    }

    pub fn peek_nth(&self, n: usize) -> Option<&Token> {
        self.tokens.get(self.position + n)
    }

    pub fn next_if(&mut self, p: impl FnOnce(&Token) -> bool) -> Option<&Token> {
        let current = self.tokens.get(self.position).filter(|&x| p(x));
        if current.is_some() {
            self.position += 1;
        }
        current
    }

    pub fn expect(&mut self, t: &Token) -> Result<()> {
        let next = self.next_or_error()?;
        if next == t {
            Ok(())
        } else {
            Err(ParseError::ExpectedButGot(t.clone(), next.clone()))
        }
    }

    pub fn peek_nth_or_error(&self, n: usize) -> Result<&Token> {
        self.peek_nth(n).ok_or(ParseError::UnexpectedEof)
    }

    pub fn peek_or_error(&mut self) -> Result<&Token> {
        self.peek().ok_or(ParseError::UnexpectedEof)
    }

    pub fn next_or_error(&mut self) -> Result<&Token> {
        let next = self
            .tokens
            .get(self.position)
            .ok_or(ParseError::UnexpectedEof)?;
        self.position += 1;
        Ok(next)
    }
}
