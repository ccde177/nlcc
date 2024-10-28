use std::str::Chars;

#[derive(Clone)]
pub struct Cursor<'a> {
    chars: Chars<'a>,
}

impl<'a> Cursor<'a> {
    pub fn new(s: &'a str) -> Self {
        let chars = s.chars();
        Self { chars }
    }

    pub fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    pub fn peek_2nd(&self) -> Option<char> {
        // cheap to clone
        let mut chars = self.chars.clone();
        //faster than .nth(1)
        chars.next();
        chars.next()
    }

    pub fn skip_whitespaces(&mut self) {
        while self.peek().filter(|c| c.is_whitespace()).is_some() {
            self.take();
        }
    }

    pub fn take(&mut self) -> Option<char> {
        self.chars.next()
    }

    pub fn as_str(&self) -> &'a str {
        self.chars.as_str()
    }
}
