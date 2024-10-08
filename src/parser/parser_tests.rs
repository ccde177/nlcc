use super::*;
use crate::lexer;

#[test]
fn test_expression_precedense_1() {
    let exp = String::from("1 * 2 - 3 * (4 + 5)");
    let mut tokens = lexer::lex(exp).unwrap();
    let parsed = parse_exp(&mut tokens, 0);
    let expected = Expression::Binary(
        BinaryOp::Substract,
        Box::new(Expression::Binary(
            BinaryOp::Multiply,
            Box::new(Expression::Constant(1)),
            Box::new(Expression::Constant(2)),
        )),
        Box::new(Expression::Binary(
            BinaryOp::Multiply,
            Box::new(Expression::Constant(3)),
            Box::new(Expression::Binary(
                BinaryOp::Add,
                Box::new(Expression::Constant(4)),
                Box::new(Expression::Constant(5)),
            )),
        )),
    );
    assert_eq!(Ok(expected), parsed);
}
