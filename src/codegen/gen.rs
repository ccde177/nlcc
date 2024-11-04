use super::fix::fix_instructions;
use super::stack::allocate_stack;
use crate::ast::{AstConst, Identifier};
use crate::codegen::asm_ast::*;
use crate::codegen::ASM_SYM_TABLE;
use crate::tacky::{
    TBinaryOp, TFunction, TInstruction, TInstructions, TUnaryOp, TValue, TopLevelItem,
};
use std::iter::successors;

fn get_const_type(c: AstConst) -> AsmType {
    match c {
        AstConst::Int(_) => AsmType::Longword,
        AstConst::Long(_) => AsmType::Quadword,
    }
}

fn get_asm_type(value: &TValue) -> AsmType {
    match value {
        TValue::Constant(c) => get_const_type(*c),
        TValue::Var(name) => ASM_SYM_TABLE.get_type(name).unwrap(),
    }
}

fn tshift_to_asm(
    op: TBinaryOp,
    val1: TValue,
    val2: TValue,
    val3: TValue,
    instructions: &mut AsmInstructions,
) {
    let src1_type = get_asm_type(&val1);
    let src2_type = get_asm_type(&val2);
    let src1 = Operand::from(val1);
    let src2 = Operand::from(val2);
    let dst = Operand::from(val3);

    let cx = Operand::Reg(Register::Cx);
    let op = BinaryOp::from(op);
    let mov = AsmInstruction::Mov(src1_type, src1, dst.clone());
    let mov2 = AsmInstruction::Mov(src2_type, src2, cx.clone());
    let operation = AsmInstruction::Binary(src1_type, op, cx, dst);

    instructions.push(mov);
    instructions.push(mov2);
    instructions.push(operation);
}

fn tdivrem_to_asm(
    op: TBinaryOp,
    val1: TValue,
    val2: TValue,
    val3: TValue,
    instructions: &mut AsmInstructions,
) {
    let src1_type = get_asm_type(&val1);
    let src1 = Operand::from(val1);
    let src2 = Operand::from(val2);
    let dst = Operand::from(val3);
    let is_rem = op.is_rem();

    let ax = Operand::Reg(Register::Ax);
    let dx = Operand::Reg(Register::Dx);
    let mov1 = AsmInstruction::Mov(src1_type, src1, ax.clone());
    let cdq = AsmInstruction::Cdq(src1_type);
    let idiv = AsmInstruction::Idiv(src1_type, src2);
    let last = if is_rem { dx } else { ax };
    let mov2 = AsmInstruction::Mov(src1_type, last, dst);

    instructions.push(mov1);
    instructions.push(cdq);
    instructions.push(idiv);
    instructions.push(mov2);
}

fn tbinary_to_asm(
    op: TBinaryOp,
    val1: TValue,
    val2: TValue,
    val3: TValue,
    instructions: &mut AsmInstructions,
) {
    let src1_type = get_asm_type(&val1);
    let src1 = Operand::from(val1);
    let src2 = Operand::from(val2);
    let dst = Operand::from(val3);

    let op = BinaryOp::from(op);
    let mov = AsmInstruction::Mov(src1_type, src1, dst.clone());
    let operation = AsmInstruction::Binary(src1_type, op, src2, dst);

    instructions.push(mov);
    instructions.push(operation);
}

fn trelational_to_asm(
    op: TBinaryOp,
    src1: TValue,
    src2: TValue,
    dst: TValue,
    instructions: &mut AsmInstructions,
) {
    let src1_type = get_asm_type(&src1);
    let dst_type = get_asm_type(&dst);
    let src1 = Operand::from(src1);
    let src2 = Operand::from(src2);
    let dst = Operand::from(dst);
    let cmp = AsmInstruction::Cmp(src1_type, src2, src1);
    let mov = AsmInstruction::Mov(dst_type, Operand::Imm(0), dst.clone());
    let setcc = AsmInstruction::SetCC(Condition::from(op), dst);

    instructions.push(cmp);
    instructions.push(mov);
    instructions.push(setcc);
}

fn truncate_to_asm(src: TValue, dst: TValue, instructions: &mut AsmInstructions) {
    let src = Operand::from(src);
    let dst = Operand::from(dst);
    let instruction = AsmInstruction::Mov(AsmType::Longword, src, dst);
    instructions.push(instruction);
}

fn sign_extend_to_asm(src: TValue, dst: TValue, instructions: &mut AsmInstructions) {
    let src = Operand::from(src);
    let dst = Operand::from(dst);
    let instruction = AsmInstruction::Movsx(src, dst);
    instructions.push(instruction);
}

fn tacky_to_asm(body: TInstructions, instructions: &mut AsmInstructions) {
    use TInstruction as TI;
    for inst in body {
        match inst {
            TI::Unary(TUnaryOp::LogicalNot, src, dst) => {
                tlogical_not_to_asm(src, dst, instructions);
            }
            TI::Unary(op, val1, val2) => tunary_to_asm(val1, val2, op, instructions),
            TI::Binary(op, v1, v2, v3) if op.is_relational() => {
                trelational_to_asm(op, v1, v2, v3, instructions);
            }
            TI::Binary(op, v1, v2, v3) if op.is_shift() => {
                tshift_to_asm(op, v1, v2, v3, instructions);
            }
            TI::Binary(op, v1, v2, v3) if op.is_divrem() => {
                tdivrem_to_asm(op, v1, v2, v3, instructions);
            }
            TI::Binary(op, v1, v2, v3) => tbinary_to_asm(op, v1, v2, v3, instructions),
            TI::JumpIfZero(val, target) => tjz_to_asm(val, target, instructions),
            TI::JumpIfNotZero(val, target) => tjnz_to_asm(val, target, instructions),
            TI::Copy(src, dst) => tcopy_to_asm(src, dst, instructions),
            TI::Label(id) => instructions.push(AsmInstruction::Label(id)),
            TI::Jump(target) => instructions.push(AsmInstruction::Jmp(target)),
            TI::FunCall { name, args, dst } => tcall_to_asm(name, args, dst, instructions),
            TI::Truncate(src, dst) => truncate_to_asm(src, dst, instructions),
            TI::SignExtend(src, dst) => sign_extend_to_asm(src, dst, instructions),
            TI::Return(val) => treturn_to_asm(val, instructions),
        }
    }
}

fn tcall_to_asm(
    name: Identifier,
    args: Vec<TValue>,
    dst: TValue,
    instructions: &mut AsmInstructions,
) {
    let reg_operands = [
        Register::Di,
        Register::Si,
        Register::Dx,
        Register::Cx,
        Register::R8,
        Register::R9,
    ]
    .into_iter()
    .map(Operand::Reg);

    let stack_args_count = args.len().saturating_sub(6);
    let stack_padding = (stack_args_count & 1) * 8;
    if stack_padding != 0 {
        let sp = Operand::Reg(Register::Sp);
        let allocate_stack = AsmInstruction::Binary(
            AsmType::Quadword,
            BinaryOp::Sub,
            Operand::Imm(stack_padding as i64),
            sp,
        );
        instructions.push(allocate_stack);
    }

    let mov_reg_args_types = args.iter().take(6).map(get_asm_type);
    let mov_reg_args = args
        .iter()
        .take(6)
        .cloned()
        .map(Operand::from)
        .zip(reg_operands)
        .zip(mov_reg_args_types)
        .map(|((operand, reg), t)| AsmInstruction::Mov(t, operand, reg));
    instructions.extend(mov_reg_args);

    let stack_args_types = args
        .clone()
        .into_iter()
        .skip(6)
        .rev()
        .map(|tv| get_asm_type(&tv));
    let stack_args = args.into_iter().skip(6).rev().map(Operand::from);

    for (stack_arg, t) in stack_args.zip(stack_args_types) {
        if matches!(stack_arg, Operand::Reg(_) | Operand::Imm(_)) || matches!(t, AsmType::Quadword)
        {
            let push = AsmInstruction::Push(stack_arg);
            instructions.push(push);
        } else {
            let ax = Operand::Reg(Register::Ax);
            let save_to_ax = AsmInstruction::Mov(t, stack_arg, ax.clone());
            let push_ax = AsmInstruction::Push(ax);
            instructions.push(save_to_ax);
            instructions.push(push_ax);
        }
    }
    let call = AsmInstruction::Call(name);
    instructions.push(call);

    let bytes_to_remove = 8 * stack_args_count + stack_padding;

    if bytes_to_remove != 0 {
        let sp = Operand::Reg(Register::Sp);
        let dealloc = AsmInstruction::Binary(
            AsmType::Quadword,
            BinaryOp::Add,
            Operand::Imm(bytes_to_remove as i64),
            sp,
        );
        instructions.push(dealloc);
    }
    let dst_type = get_asm_type(&dst);
    let asm_dst = Operand::from(dst);
    let ax = Operand::Reg(Register::Ax);
    let mov = AsmInstruction::Mov(dst_type, ax, asm_dst);
    instructions.push(mov);
}

fn tunary_to_asm(val1: TValue, val2: TValue, op: TUnaryOp, instructions: &mut AsmInstructions) {
    let src_type = get_asm_type(&val1);
    let src = Operand::from(val1);
    let dst = Operand::from(val2);
    let op = UnaryOp::from(op);
    let mov = AsmInstruction::Mov(src_type, src, dst.clone());
    let unary = AsmInstruction::Unary(src_type, op, dst);

    instructions.push(mov);
    instructions.push(unary);
}

fn tlogical_not_to_asm(src: TValue, dst: TValue, instructions: &mut AsmInstructions) {
    let src_type = get_asm_type(&src);
    let dst_type = get_asm_type(&dst);
    let src = Operand::from(src);
    let dst = Operand::from(dst);
    let cmp = AsmInstruction::Cmp(src_type, Operand::Imm(0), src);
    let mov = AsmInstruction::Mov(dst_type, Operand::Imm(0), dst.clone());
    let setcc = AsmInstruction::SetCC(Condition::E, dst);

    instructions.push(cmp);
    instructions.push(mov);
    instructions.push(setcc);
}

fn treturn_to_asm(val: TValue, instructions: &mut AsmInstructions) {
    let rtype = get_asm_type(&val);
    let src = Operand::from(val);
    let dst = Operand::Reg(Register::Ax);
    let mov = AsmInstruction::Mov(rtype, src, dst);
    let ret = AsmInstruction::Ret;

    instructions.push(mov);
    instructions.push(ret);
}

fn tjz_to_asm(val: TValue, target: String, instructions: &mut AsmInstructions) {
    let ctype = get_asm_type(&val);
    let src = Operand::from(val);
    let cmp = AsmInstruction::Cmp(ctype, Operand::Imm(0), src);
    let jmp = AsmInstruction::JmpCC(Condition::E, target);

    instructions.push(cmp);
    instructions.push(jmp);
}

fn tjnz_to_asm(val: TValue, target: String, instructions: &mut AsmInstructions) {
    let ctype = get_asm_type(&val);
    let src = Operand::from(val);
    let cmp = AsmInstruction::Cmp(ctype, Operand::Imm(0), src);
    let jmp = AsmInstruction::JmpCC(Condition::NE, target);

    instructions.push(cmp);
    instructions.push(jmp);
}

fn tcopy_to_asm(src: TValue, dst: TValue, instructions: &mut Vec<AsmInstruction>) {
    let src_type = get_asm_type(&src);
    let src = Operand::from(src);
    let dst = Operand::from(dst);
    let mov = AsmInstruction::Mov(src_type, src, dst);

    instructions.push(mov);
}

fn gen_fundef(f: TFunction) -> AsmFunction {
    let TFunction {
        name,
        params,
        body,
        global,
    } = f;

    let param_types = params
        .clone()
        .into_iter()
        .map(|s| ASM_SYM_TABLE.get_type(&s).unwrap());
    let params = params.into_iter().map(Operand::Pseudo);
    let reg_src = [
        Register::Di,
        Register::Si,
        Register::Dx,
        Register::Cx,
        Register::R8,
        Register::R9,
    ]
    .into_iter()
    .map(Operand::Reg);
    let stack_src = successors(Some(16), |n| Some(n + 8)).map(Operand::Stack);
    let src_iter = reg_src.chain(stack_src);

    let mut instructions = src_iter
        .zip(params)
        .zip(param_types)
        .map(|((src, dst), t)| AsmInstruction::Mov(t, src, dst))
        .collect::<AsmInstructions>();

    tacky_to_asm(body, &mut instructions);
    allocate_stack(&mut instructions);
    fix_instructions(&mut instructions);
    AsmFunction {
        name,
        body: instructions,
        global,
    }
}

pub fn gen_toplevel_item(item: TopLevelItem) -> AsmTopLevelItem {
    match item {
        TopLevelItem::Fun(tfun) => AsmTopLevelItem::Fun(gen_fundef(tfun)),
        TopLevelItem::Var(staticvar) => AsmTopLevelItem::from(staticvar),
    }
}
