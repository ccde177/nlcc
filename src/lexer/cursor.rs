use std::str::Chars;

#[derive(Clone)]
pub struct Cursor<'a> {
    chars: Chars<'a>,
    ln: u64,
}

impl<'a> Cursor<'a> {
    pub fn new(s: &'a str) -> Self {
        let chars = s.chars();
        let ln = 0;
        Self { chars, ln }
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

    pub fn bump_if(&mut self, value: char) -> bool {
        let should_skip = self.peek().filter(|&c| c == value).is_some();
        if should_skip {
            self.take();
        }
        should_skip
    }

    #[inline]
    pub fn skip_if(&mut self, p: impl FnOnce(char) -> bool) -> bool {
        let skipped = self.peek().filter(|&c| p(c)).is_some();
        if skipped {
            self.take();
        }
        skipped
    }

    pub fn get_ln(&self) -> u64 {
        self.ln
    }

    pub fn skip_whitespaces(&mut self) {
        while self.skip_if(char::is_whitespace) {}
    }

    pub fn take(&mut self) -> Option<char> {
        if self.chars.clone().next() == Some('\n') {
            self.ln += 1;
        }
        self.chars.next()
    }

    pub fn as_str(&self) -> &'a str {
        self.chars.as_str()
    }
}
