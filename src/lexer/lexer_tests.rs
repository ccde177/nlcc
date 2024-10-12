use super::*;

#[test]
fn test_basic1() {
    let input = String::from("int main(void) {}");
    let lexed = lex(input);
    let expected = VecDeque::from(vec![
        Token::Int,
        Token::Identifier(String::from("main")),
        Token::OpenParanth,
        Token::Void,
        Token::CloseParanth,
        Token::OpenCurly,
        Token::CloseCurly,
    ]);
    assert_eq!(Ok(expected), lexed);
}

#[test]
fn test_bad_atsign() {
    let input = String::from("int main(void) {\nreturn 0@1;\n}");
    let lexed = lex(input);
    let expected = Err(LexError::UnexpectedChar('@'));
    assert_eq!(expected, lexed);
}

#[test]
fn test_bad_constant() {
    let input = String::from("int main(void) {\nreturn 1foo;\n}");
    let lexed = lex(input);
    let expected = Err(LexError::UnexpectedChar('f'));
    assert_eq!(expected, lexed);
}

#[test]
fn test_precedence() {
    let input = String::from("int main(void) {\nreturn (0 == 0 && 3 == 2 + 1 > 1) + 1;\n}");
    let lexed = lex(input);
    let expected = VecDeque::from(vec![
        Token::Int,
        Token::Identifier(String::from("main")),
        Token::OpenParanth,
        Token::Void,
        Token::CloseParanth,
        Token::OpenCurly,
        Token::Return,
        Token::OpenParanth,
        Token::Constant(0),
        Token::IsEqual,
        Token::Constant(0),
        Token::LogicalAnd,
        Token::Constant(3),
        Token::IsEqual,
        Token::Constant(2),
        Token::Plus,
        Token::Constant(1),
        Token::IsGreaterThan,
        Token::Constant(1),
        Token::CloseParanth,
        Token::Plus,
        Token::Constant(1),
        Token::Semicolon,
        Token::CloseCurly,
    ]);
    assert_eq!(Ok(expected), lexed);
}
