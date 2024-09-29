#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token {
    Int,
    Identifier(String),
    OpenParanth,
    Void,
    CloseParanth,
    OpenCurly,
    Return,
    Constant(u64),
    Semicolon,
    CloseCurly,
}

pub type Tokens = Vec<Token>;

#[derive(Debug)]
pub enum LexError {
    UnexpectedChar(char)
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::UnexpectedChar(c) => write!(f, "Unexpected character {c}")
        }
    }
}

impl std::error::Error for LexError {}

impl TryFrom<char> for Token {
    type Error = &'static str;
    fn try_from(c: char) -> Result<Self, Self::Error> {
        match c {
            ';' => Ok(Self::Semicolon),
            '(' => Ok(Self::OpenParanth),
            ')' => Ok(Self::CloseParanth),
            '{' => Ok(Self::OpenCurly),
            '}' => Ok(Self::CloseCurly),
            _ => Err("Not a special character"),
        }
    }
}

impl From<String> for Token {
    fn from(s: String) -> Self {
        match s.as_str() {
            "int" => Self::Int,
            "return" => Self::Return,
            "void" => Self::Void,
            _ => Self::Identifier(s)
        }
    }
}


#[derive(Eq, PartialEq)]
enum LexerState {
    Default,
    Constant,
    Identifier,
}

impl LexerState {
    fn new() -> Self {
        Self::Default
    }

    fn is_default(&self) -> bool {
        *self == LexerState::Default
    }

    fn is_constant(&self) -> bool {
        *self == LexerState::Constant
    }

    fn is_identifier(&self) -> bool {
        *self == LexerState::Identifier
    }
}


fn finalize_state(state: &mut LexerState, buffer: &mut String, tokens: &mut Tokens) {
    if state.is_default() {
        return;
    }

    let token = match state {
        LexerState::Identifier => Token::from(buffer.clone()),
        LexerState::Constant => Token::Constant(buffer.parse().expect("Should never fail")),
        _ => panic!("Should never be reached"),
    };

    tokens.push(token);
    *state = LexerState::Default;
    buffer.clear();
}

pub fn lex(input: String) -> Result<Tokens, LexError> {
    let mut tokens = Vec::new();
    let mut buf = String::new();
    let mut state = LexerState::new();

    for c in input.chars() {
        match c {
            ';' | '{' | '}' | '(' | ')' => {
                if !state.is_default() {
                    finalize_state(&mut state, &mut buf, &mut tokens);
                }
                tokens.push(Token::try_from(c).expect("Should never fail"));
            }
            '0'..='9' => {
                if state.is_default() {
                    state = LexerState::Constant;
                }
                buf.push(c);
            }
            'a'..='z' | 'A'..='Z' => {
                if state.is_constant() {
                    return Err(LexError::UnexpectedChar(c));
                }
                state = LexerState::Identifier;
                buf.push(c);
            }
            c if c.is_whitespace() => {
                if !state.is_default() {
                    finalize_state(&mut state, &mut buf, &mut tokens);
                }
            }
            _ => return Err(LexError::UnexpectedChar(c)),
        }
    }

    if !state.is_default() {
        finalize_state(&mut state, &mut buf, &mut tokens);
    }

    Ok(tokens)
}
