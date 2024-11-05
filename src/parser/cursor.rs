use crate::lexer::{LinedToken, Token};
use crate::parser::{InnerParseError, ParseError};

#[derive(Debug)]
pub struct Cursor<'a> {
    tokens: &'a [LinedToken],
    position: usize,
    line: u64,
}

#[allow(dead_code)]
impl<'a> Cursor<'a> {
    pub fn new(tokens: &'a [LinedToken]) -> Self {
        Self {
            tokens,
            position: 0,
            line: 0,
        }
    }

    pub fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.position).map(|lined| &lined.inner)
    }

    pub fn bump(&mut self) {
        self.position += 1;
        if let Some(next) = self.tokens.get(self.position) {
            self.line = next.get_line()
        }
    }

    pub fn skip_if(&mut self, p: impl FnOnce(&Token) -> bool) -> bool {
        let condition = self.tokens.get(self.position).filter(|&t| p(t)).is_some();
        if condition {
            self.bump()
        }
        condition
    }

    pub fn bump_if(&mut self, t: &Token) -> bool {
        let predicate = |token: &Token| token == t;
        let condition = self
            .tokens
            .get(self.position)
            .filter(|&x| predicate(x))
            .is_some();
        if condition {
            self.bump();
        }
        condition
    }

    pub fn at_end(&self) -> bool {
        self.position >= self.tokens.len()
    }

    pub fn get_line(&self) -> u64 {
        self.line
    }

    pub fn peek_nth(&self, n: usize) -> Option<&Token> {
        self.tokens.get(self.position + n).map(|t| &t.inner)
    }

    pub fn next_if(&mut self, p: impl FnOnce(&Token) -> bool) -> Option<&Token> {
        let next = self.tokens.get(self.position).filter(|&x| p(x));
        if next.is_some() {
            self.bump();
        }
        next.map(|v| &**v)
    }

    pub fn expect(&mut self, t: &Token) -> Result<(), ParseError> {
        let next = self.next_or_error()?;
        if next == t {
            Ok(())
        } else {
            Err(InnerParseError::ExpectedButGot(t.clone(), next.clone()).set_line(self.line))
        }
    }

    pub fn peek_nth_or_error(&self, n: usize) -> Result<&Token, ParseError> {
        self.peek_nth(n)
            .ok_or(InnerParseError::UnexpectedEof.set_line(self.line))
    }

    pub fn peek_or_error(&mut self) -> Result<&Token, ParseError> {
        self.peek()
            .ok_or(InnerParseError::UnexpectedEof.set_line(self.line))
    }

    pub fn peek_is(&self, t: &Token) -> bool {
        self.peek() == Some(t)
    }

    pub fn next_or_error(&mut self) -> Result<&Token, ParseError> {
        let next = self
            .tokens
            .get(self.position)
            .ok_or(InnerParseError::UnexpectedEof.set_line(self.line))?;
        self.line = next.get_line();
        self.position += 1;
        Ok(&next.inner)
    }
}
