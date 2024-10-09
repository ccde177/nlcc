use super::*;
use crate::lexer;

#[test]
fn test_expression_precedense_1() {
    let exp = String::from("1 * 2 - 3 * (4 + 5)");
    let mut tokens = lexer::lex(exp).unwrap();
    let parsed = parse_exp(&mut tokens, 0);
    let expected = AstExp::Binary(
        AstBinaryOp::Substract,
        Box::new(AstExp::Binary(
            AstBinaryOp::Multiply,
            Box::new(AstExp::Constant(1)),
            Box::new(AstExp::Constant(2)),
        )),
        Box::new(AstExp::Binary(
            AstBinaryOp::Multiply,
            Box::new(AstExp::Constant(3)),
            Box::new(AstExp::Binary(
                AstBinaryOp::Add,
                Box::new(AstExp::Constant(4)),
                Box::new(AstExp::Constant(5)),
            )),
        )),
    );
    assert_eq!(Ok(expected), parsed);
}
