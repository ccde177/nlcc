use super::*;

#[test]
fn test_shortcircuiting_and() {
    let body = vec![AstBlockItem::S(AstStatement::Return(
        AstExp::Binary(
            AstBinaryOp::LogicalAnd,
            Box::new(AstExp::Constant(1)),
            Box::new(AstExp::Constant(2))
        )
    ))];
    let ast = Ast::FunDef(
        AstFunction {
            name: "".into(),
            body
        }
    );

    let tast = emit_tacky(ast);
    let expected_tinstructions = TInstructions::from(vec![
        /*
        jz 1, false_label
        jz 2, false_label
        retvar = 1
        jmp exit_label
        false_label:
        retvar = 0
        exit_label:
        ret retevar
        ret 0
        */
        TInstruction::JumpIfZero(TValue::Constant(1), "label_0".into()),
        TInstruction::JumpIfZero(TValue::Constant(2), "label_0".into()),
        TInstruction::Copy(TValue::Constant(1), TValue::Var("tmp.0".into())),
        TInstruction::Jump("label_1".into()),
        TInstruction::Label("label_0".into()),
        TInstruction::Copy(TValue::Constant(0), TValue::Var("tmp.0".into())),
        TInstruction::Label("label_1".into()),
        TInstruction::Return(TValue::Var("tmp.0".into())),
        TInstruction::Return(TValue::Constant(0)),
    ]);
    let expected = TAst::Program(
        TFunction::FunDef("".into(), expected_tinstructions)
    );
    assert_eq!(expected, tast);
}

#[test]
fn test_shortcircuiting_or() {
    let body = vec![AstBlockItem::S(AstStatement::Return(
        AstExp::Binary(
            AstBinaryOp::LogicalOr,
            Box::new(AstExp::Constant(1)),
            Box::new(AstExp::Constant(2))
        )
    ))];
    let ast = Ast::FunDef(
        AstFunction {
            name: "".into(),
            body
        }
    );

    let tast = emit_tacky(ast);
    let expected_tinstructions = TInstructions::from(vec![
        /*
        jnz 1, true_label
        jnz 2, true_label
        retvar = 0
        jmp exit_label
        true_label:
        retvar = 1
        exit_label:
        ret retevar
        ret 0
        */
        TInstruction::JumpIfNotZero(TValue::Constant(1), "label_0".into()),
        TInstruction::JumpIfNotZero(TValue::Constant(2), "label_0".into()),
        TInstruction::Copy(TValue::Constant(0), TValue::Var("tmp.0".into())),
        TInstruction::Jump("label_1".into()),
        TInstruction::Label("label_0".into()),
        TInstruction::Copy(TValue::Constant(1), TValue::Var("tmp.0".into())),
        TInstruction::Label("label_1".into()),
        TInstruction::Return(TValue::Var("tmp.0".into())),
        TInstruction::Return(TValue::Constant(0))
    ]);
    let expected = TAst::Program(
        TFunction::FunDef("".into(), expected_tinstructions)
    );
    assert_eq!(expected, tast);
}
