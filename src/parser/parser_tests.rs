use super::*;
use crate::lexer;

#[test]
fn test_expression_precedense_1() {
    let exp = String::from("1 * 2 - 3 * (4 + 5)");
    let tokens = lexer::lex(&exp).unwrap();
    let mut cursor = Cursor::new(&tokens);
    
    let parsed = parse_exp(&mut cursor, 0);
    dbg!(cursor);
    let expected = Exp::Binary(
        AstBinaryOp::Substract,
        Box::new(Exp::Binary(
            AstBinaryOp::Multiply,
            Box::new(Exp::Constant(1)),
            Box::new(Exp::Constant(2)),
        )),
        Box::new(Exp::Binary(
            AstBinaryOp::Multiply,
            Box::new(Exp::Constant(3)),
            Box::new(Exp::Binary(
                AstBinaryOp::Add,
                Box::new(Exp::Constant(4)),
                Box::new(Exp::Constant(5)),
            )),
        )),
    );
    assert_eq!(Ok(expected), parsed);
}
