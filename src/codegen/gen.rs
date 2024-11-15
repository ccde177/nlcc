use super::asm_ast::*;
use super::fix::fix_instructions;
use super::stack::allocate_stack;
use super::ASM_SYM_TABLE;

use crate::ast::{AstConst, Identifier};
use crate::semantic_analysis::StaticInit;
use crate::tacky::{
    TBinaryOp, TFunction, TInstruction, TInstructions, TUnaryOp, TValue, TopLevelItem,
};

use std::collections::HashMap;
use std::iter::successors;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{LazyLock, RwLock};

pub(super) static CONST_TABLE: ConstTable = ConstTable::new();
pub(super) struct ConstTable {
    inner: LazyLock<RwLock<HashMap<AstConst, Identifier>>>,
}

impl ConstTable {
    const fn new() -> Self {
        Self {
            inner: LazyLock::new(|| RwLock::new(HashMap::new())),
        }
    }

    pub(crate) fn get_const_name(&self, c: &AstConst) -> Identifier {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        if let Some(name) = self.inner.read().expect("Should not be poisoned").get(c) {
            return name.clone();
        }
        let new_name = format!("__const_{}", COUNTER.fetch_add(1, Ordering::AcqRel));
        ASM_SYM_TABLE.add_static_const(new_name.clone(), *c);
        self.inner
            .write()
            .expect("Should not be poisoned")
            .insert(*c, new_name.clone());

        new_name
    }

    pub(super) fn collect(&self) -> Vec<AsmTopLevelItem> {
        self.inner
            .read()
            .expect("Should not be poisoned")
            .iter()
            .map(|(c, name)| AsmStaticConst {
                name: name.clone(),
                alignment: c.get_align(),
                init: StaticInit::from(*c),
            })
            .map(AsmTopLevelItem::StaticConst)
            .collect()
    }
}

fn get_const_type(c: AstConst) -> AsmType {
    match c {
        AstConst::Int(_) => AsmType::Longword,
        AstConst::Long(_) => AsmType::Quadword,
        AstConst::ULong(_) => AsmType::Quadword,
        AstConst::UInt(_) => AsmType::Longword,
        AstConst::Double(_) => AsmType::Double,
    }
}

fn get_asm_type(value: &TValue) -> AsmType {
    match value {
        TValue::Constant(c) => get_const_type(*c),
        TValue::Var(name) => ASM_SYM_TABLE.get_type(name).unwrap(),
    }
}

fn shiftop_to_asm(op: TBinaryOp, is_signed: bool) -> AsmBinaryOp {
    match op {
        TBinaryOp::ShiftRight => {
            if is_signed {
                AsmBinaryOp::Sar
            } else {
                AsmBinaryOp::Shr
            }
        }
        TBinaryOp::ShiftLeft => AsmBinaryOp::Sal,
        _ => panic!("Attempt to get shift asm operator from {op:?}"),
    }
}

fn tshift_to_asm(
    op: TBinaryOp,
    val1: TValue,
    val2: TValue,
    val3: TValue,
    instructions: &mut AsmInstructions,
) {
    let is_signed = val1.get_type().is_signed();
    let src1_type = get_asm_type(&val1);
    let src2_type = get_asm_type(&val2);
    let src1 = Operand::from(val1);
    let src2 = Operand::from(val2);
    let dst = Operand::from(val3);
    let cx = Operand::Reg(Register::CX);
    let op = shiftop_to_asm(op, is_signed);

    let mov = AsmInstruction::Mov(src1_type, src1, dst.clone());
    let mov2 = AsmInstruction::Mov(src2_type, src2, cx.clone());
    let operation = AsmInstruction::Binary(src1_type, op, cx, dst);

    instructions.push(mov);
    instructions.push(mov2);
    instructions.push(operation);
}

fn tdobulediv_to_asm(
    src1: Operand,
    src2: Operand,
    dst: Operand,
    instructions: &mut AsmInstructions,
) {
    assemble!(instructions {
        movsd src1, dst;
        divsd src2, dst;
    });
}

fn tdivrem_to_asm(
    op: TBinaryOp,
    val1: TValue,
    val2: TValue,
    val3: TValue,
    instructions: &mut AsmInstructions,
) {
    let is_signed = val1.get_type().is_signed();
    let src1_type = get_asm_type(&val1);
    let is_rem = op.is_rem();
    let src1 = Operand::from(val1);
    let src2 = Operand::from(val2);
    let dst = Operand::from(val3);
    if src1_type.is_double() {
        return tdobulediv_to_asm(src1, src2, dst, instructions);
    }
    let ax = Operand::Reg(Register::AX);
    let dx = Operand::Reg(Register::DX);
    let mov1 = AsmInstruction::Mov(src1_type, src1, ax.clone());
    let cdq_or_mov = if is_signed {
        AsmInstruction::Cdq(src1_type)
    } else {
        AsmInstruction::Mov(src1_type, Operand::Imm(0), dx.clone())
    };

    let idiv_or_div = if is_signed {
        AsmInstruction::Idiv(src1_type, src2)
    } else {
        AsmInstruction::Div(src1_type, src2)
    };

    let last = if is_rem { dx } else { ax };
    let mov2 = AsmInstruction::Mov(src1_type, last, dst);

    instructions.push(mov1);
    instructions.push(cdq_or_mov);
    instructions.push(idiv_or_div);
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

    let op = AsmBinaryOp::from(op);
    let mov = AsmInstruction::Mov(src1_type, src1, dst.clone());
    let operation = AsmInstruction::Binary(src1_type, op, src2, dst);

    instructions.push(mov);
    instructions.push(operation);
}

fn relative_to_condition(op: &TBinaryOp) -> Condition {
    match op {
        TBinaryOp::IsGreaterOrEqual => Condition::GE,
        TBinaryOp::IsLessOrEqual => Condition::LE,
        TBinaryOp::IsGreaterThan => Condition::G,
        TBinaryOp::IsLessThan => Condition::L,
        TBinaryOp::IsEqual => Condition::E,
        TBinaryOp::IsNotEqual => Condition::NE,
        _ => {
            panic!("Internal Error: attempt to get conditional code from not a realative operator")
        }
    }
}

fn trelational_to_asm(
    op: TBinaryOp,
    src1: TValue,
    src2: TValue,
    dst: TValue,
    instructions: &mut AsmInstructions,
) {
    // This should cover the double case
    let is_signed = src1.get_type().is_signed();
    let src1_type = get_asm_type(&src1);
    let dst_type = get_asm_type(&dst);
    let src1 = Operand::from(src1);
    let src2 = Operand::from(src2);
    let dst = Operand::from(dst);
    let condition = if is_signed {
        relative_to_condition(&op).to_signed()
    } else {
        relative_to_condition(&op).to_unsigned()
    };

    if src1_type.is_double() {
        if condition == Condition::E {
            return handle_nan_e(src1, src2, dst, instructions);
        }
        if condition == Condition::NE {
            return handle_nan_ne(src1, src2, dst, instructions);
        }
    }

    if src1_type.is_double() {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        let counter = COUNTER.fetch_add(1, Ordering::AcqRel);
        let nan_label = format!("__nan.{counter}");
        assemble!(instructions {
            comisd src2, src1;
            mov dst_type, #0, dst;
            jp nan_label;
            setcc condition, dst;
            nan_label:
        });
    } else {
        assemble!(instructions {
            cmp src1_type, src2, src1;
            mov dst_type, #0, dst;
            setcc condition, dst;
        });
    }
}

fn handle_nan_ne(src1: Operand, src2: Operand, dst: Operand, instructions: &mut AsmInstructions) {
    assemble!(instructions{
        xorl dst, dst;
        xorl %eax, %eax;
        comisd src2, src1;
        movl #1, %edx;
        setp %eax;
        cmovnel %edx, %eax;
        movl %eax, dst;
    });
}

fn handle_nan_e(src1: Operand, src2: Operand, dst: Operand, instructions: &mut AsmInstructions) {
    assemble!(instructions {
        xorl %eax, %eax;
        xorl dst, dst;
        comisd src2, src1;
        movl #0, %edx;
        setnp %eax;
        cmovnel %edx, %eax;
        movl %eax, dst;
    });
}

fn truncate_to_asm(src: TValue, dst: TValue, instructions: &mut AsmInstructions) {
    let src = Operand::from(src);
    let dst = Operand::from(dst);
    assemble!(instructions {
        movl src, dst;
    });
}

fn sign_extend_to_asm(src: TValue, dst: TValue, instructions: &mut AsmInstructions) {
    let src = Operand::from(src);
    let dst = Operand::from(dst);

    assemble!(instructions {
        movsx src, dst;
    });
}

fn tdouble_to_int(src: TValue, dst: TValue, instructions: &mut AsmInstructions) {
    let dst_type = get_asm_type(&dst);
    let src = Operand::from(src);
    let dst = Operand::from(dst);

    assemble!(instructions {
        cvttsd2si dst_type, src, dst;
    });
}

fn tdouble_to_ulong(src: TValue, dst: TValue, instructions: &mut AsmInstructions) {
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    let counter = COUNTER.fetch_add(1, Ordering::AcqRel);

    let oor_label = format!("_ulong_out_of_range_{counter}");
    let end_label = format!("_double_to_ulong_end_{counter}");
    let upper_bound = CONST_TABLE.get_const_name(&AstConst::Double(9223372036854775808.0));

    let src = Operand::from(src);
    let dst = Operand::from(dst);

    assemble! (instructions {
        comisd [upper_bound], src;
        jae oor_label;
        cvttsd2siq src, dst;
        jmp end_label;
    oor_label:
        movsd src, %xmm1;
        subsd [upper_bound], %xmm1;
        cvttsd2siq %xmm1, dst;
        movq #9223372036854775808, %rdx;
        addq %rdx, dst;
    end_label:
    });
}

fn tdouble_to_uint(src: TValue, dst: TValue, instructions: &mut AsmInstructions) {
    let dst_type = get_asm_type(&dst);
    if dst_type.is_quadword() {
        return tdouble_to_ulong(src, dst, instructions);
    }
    let src = Operand::from(src);
    let dst = Operand::from(dst);

    assemble!(instructions {
        cvttsd2siq src, %rax;
        movl %eax, dst;
    });
}

fn tacky_to_asm(body: TInstructions, instructions: &mut AsmInstructions) {
    use TInstruction as TI;
    for inst in body {
        match inst {
            TI::DoubleToUInt(src, dst) => tdouble_to_uint(src, dst, instructions),
            TI::DoubleToInt(src, dst) => tdouble_to_int(src, dst, instructions),
            TI::UIntToDouble(src, dst) => tuint_to_double(src, dst, instructions),
            TI::IntToDouble(src, dst) => tint_to_double(src, dst, instructions),
            TI::ZeroExtend(src, dst) => {
                tzx_to_asm(src, dst, instructions);
            }
            TI::Unary(TUnaryOp::LogicalNot, src, dst) => {
                tlogical_not_to_asm(src, dst, instructions);
            }
            TI::Unary(TUnaryOp::Negate, val1, val2) => tnegate_to_asm(val1, val2, instructions),
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
            TI::FunCall { name, args, dst } => tcall_to_asm(name, args, dst, instructions),
            TI::Truncate(src, dst) => truncate_to_asm(src, dst, instructions),
            TI::SignExtend(src, dst) => sign_extend_to_asm(src, dst, instructions),
            TI::Return(val) => treturn_to_asm(val, instructions),
            TI::Label(id) => instructions.push(AsmInstruction::Label(id)),
            TI::Jump(target) => instructions.push(AsmInstruction::Jmp(target)),
        }
    }
}

fn tuint_to_double(src: TValue, dst: TValue, instructions: &mut AsmInstructions) {
    let src_type = get_asm_type(&src);
    let src = Operand::from(src);
    let dst = Operand::from(dst);
    if src_type.is_quadword() {
        return tulong_to_double(src, dst, instructions);
    }

    assemble!(instructions {
        movzx src, %eax;
        cvtsi2sdq %rax, dst;
    });
}

fn tulong_to_double(src: Operand, dst: Operand, instructions: &mut AsmInstructions) {
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    let counter = COUNTER.fetch_add(1, Ordering::AcqRel);
    let oor_label = format!("__ulong_to_double_oor_{counter}");
    let end_label = format!("__ulong_to_double_end_{counter}");

    assemble!(instructions {
        cmpq #0, src;
        jl oor_label;
        cvtsi2sdq src, dst;
        jmp end_label;
    oor_label:
        movq src, %rax;
        movq %rax, %rdx;
        shrq %rdx;
        andq #1, %rax;
        orq %rax, %rdx;
        cvtsi2sdq %rdx, dst;
        addsd dst, dst;
    end_label:
    });
}

fn tint_to_double(src: TValue, dst: TValue, instructions: &mut AsmInstructions) {
    let src_type = get_asm_type(&src);
    let src = Operand::from(src);
    let dst = Operand::from(dst);
    assemble!(instructions {
        cvtsi2sd src_type, src, dst;
    });
}

fn double_negate_to_asm(src: TValue, dst: TValue, instructions: &mut AsmInstructions) {
    let neg_zero = CONST_TABLE.get_const_name(&AstConst::Double(-0.));
    let src = Operand::from(src);
    let dst = Operand::from(dst);
    assemble!(instructions {
        movsd src, dst;
        xorsd [neg_zero], dst;
    });
}

fn tnegate_to_asm(src: TValue, dst: TValue, instructions: &mut AsmInstructions) {
    let src_type = src.get_type();
    if src_type.is_double() {
        double_negate_to_asm(src, dst, instructions);
    } else {
        tunary_to_asm(src, dst, TUnaryOp::Negate, instructions);
    }
}

fn tzx_to_asm(src: TValue, dst: TValue, instructions: &mut AsmInstructions) {
    let src = Operand::from(src);
    let dst = Operand::from(dst);
    assemble!(instructions {
        movzx src, dst;
    });
}

fn classify_parameters(
    values: Vec<TValue>,
) -> (
    Vec<(AsmType, Operand)>,
    Vec<Operand>,
    Vec<(AsmType, Operand)>,
) {
    let mut int_reg_args = Vec::new();
    let mut double_reg_args = Vec::new();
    let mut stack_args = Vec::new();

    for v in values {
        let t = get_asm_type(&v);
        let operand = Operand::from(v);
        if t.is_double() {
            if double_reg_args.len() < 8 {
                double_reg_args.push(operand);
            } else {
                stack_args.push((t, operand));
            }
        } else {
            if int_reg_args.len() < 6 {
                int_reg_args.push((t, operand));
            } else {
                stack_args.push((t, operand));
            }
        }
    }

    (int_reg_args, double_reg_args, stack_args)
}

fn tcall_to_asm(
    name: Identifier,
    args: Vec<TValue>,
    dst: TValue,
    instructions: &mut AsmInstructions,
) {
    let reg_operands = [
        Register::DI,
        Register::SI,
        Register::DX,
        Register::CX,
        Register::R8,
        Register::R9,
    ]
    .into_iter()
    .map(Operand::Reg);
    let double_registers = [
        Register::XMM0,
        Register::XMM1,
        Register::XMM2,
        Register::XMM3,
        Register::XMM4,
        Register::XMM5,
        Register::XMM6,
        Register::XMM7,
    ]
    .into_iter()
    .map(Operand::Reg);

    let (int_args, double_args, stack_args) = classify_parameters(args);
    let stack_padding = (stack_args.len() & 1) * 8;
    if stack_padding != 0 {
        assemble!(instructions {
            subq #stack_padding, %rsp;
        });
    }

    int_args
        .into_iter()
        .zip(reg_operands)
        .for_each(|((asm_type, asm_arg), reg)| {
            assemble!(instructions {
                mov asm_type, asm_arg, reg;
            });
        });

    double_args
        .into_iter()
        .zip(double_registers)
        .for_each(|(arg, reg)| {
            assemble!(instructions {
                movsd arg, reg;
            });
        });

    let stack_args_count = stack_args.len();
    for (t, stack_arg) in stack_args.into_iter().rev() {
        if stack_arg.is_reg() || stack_arg.is_imm() || t.is_double() || t.is_quadword() {
            assemble!(instructions {
                push stack_arg;
            });
        } else {
            assemble!(instructions {
                mov t, stack_arg, %rax;
                push %rax;
            });
        }
    }

    assemble!(instructions {
        call name;
    });

    let bytes_to_remove = 8 * stack_args_count + stack_padding;

    if bytes_to_remove != 0 {
        assemble!(instructions {
            addq #bytes_to_remove, %rsp;
        });
    }

    let dst_type = get_asm_type(&dst);
    let asm_dst = Operand::from(dst);
    if dst_type.is_double() {
        assemble!(instructions {
            movsd %xmm0, asm_dst;
        });
    } else {
        assemble!(instructions {
            mov dst_type, %rax, asm_dst;
        });
    }
}

fn tunary_to_asm(val1: TValue, val2: TValue, op: TUnaryOp, instructions: &mut AsmInstructions) {
    let src_type = get_asm_type(&val1);
    let src = Operand::from(val1);
    let dst = Operand::from(val2);
    let op = AsmUnaryOp::from(op);
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

    if src_type.is_double() {
        assemble!(instructions {
            xorsd %xmm0, %xmm0;
            comisd src, %xmm0;
            mov dst_type, #0, dst;
            sete dst;
        });
    } else {
        assemble!(instructions {
            cmp src_type, #0, src;
            mov dst_type, #0, dst;
            sete dst;
        });
    }
}

fn treturn_to_asm(val: TValue, instructions: &mut AsmInstructions) {
    let rtype = get_asm_type(&val);
    let src = Operand::from(val);
    let ret_reg = if rtype.is_double() {
        Operand::Reg(Register::XMM0)
    } else {
        Operand::Reg(Register::AX)
    };
    assemble!(instructions {
        mov rtype, src, ret_reg;
        ret;
    });
}

fn tjz_to_asm(val: TValue, target: String, instructions: &mut AsmInstructions) {
    let ctype = get_asm_type(&val);
    let src = Operand::from(val);
    let is_double = ctype.is_double();

    let zero = if is_double {
        assemble!(instructions {
            xorsd %xmm0, %xmm0;
        });
        Operand::Reg(Register::XMM0)
    } else {
        Operand::Imm(0)
    };

    assemble! (instructions {
        cmp ctype, src, zero;
        je target;
    });
}

fn tjnz_to_asm(val: TValue, target: String, instructions: &mut AsmInstructions) {
    let ctype = get_asm_type(&val);
    let src = Operand::from(val);
    let is_double = ctype.is_double();

    let zero = if is_double {
        assemble!(instructions {
            xorsd %xmm0, %xmm0;
        });
        Operand::Reg(Register::XMM0)
    } else {
        Operand::Imm(0)
    };

    assemble!(instructions {
        cmp ctype, zero, src;
        jne target;
    });
}

fn tcopy_to_asm(src: TValue, dst: TValue, instructions: &mut Vec<AsmInstruction>) {
    let src_type = get_asm_type(&src);
    let src = Operand::from(src);
    let dst = Operand::from(dst);

    assemble!(instructions {
        mov src_type, src, dst;
    });
}

fn set_up_parameters(params: Vec<TValue>, instructions: &mut AsmInstructions) {
    let (int_reg_params, double_reg_params, stack_params) = classify_parameters(params);

    let double_regs = [
        Register::XMM0,
        Register::XMM1,
        Register::XMM2,
        Register::XMM3,
        Register::XMM4,
        Register::XMM5,
        Register::XMM6,
        Register::XMM7,
    ]
    .into_iter()
    .map(Operand::Reg);
    let reg_src = [
        Register::DI,
        Register::SI,
        Register::DX,
        Register::CX,
        Register::R8,
        Register::R9,
    ]
    .into_iter()
    .map(Operand::Reg);

    int_reg_params
        .into_iter()
        .zip(reg_src)
        .for_each(|((param_type, param), reg)| {
            assemble!(instructions {
                mov param_type, reg, param;
            });
        });

    double_reg_params
        .into_iter()
        .zip(double_regs)
        .for_each(|(param, reg)| {
            assemble!(instructions {
                movsd reg, param;
            });
        });

    let stack_src = successors(Some(16), |n| Some(n + 8)).map(Operand::Stack);
    stack_params
        .into_iter()
        .zip(stack_src)
        .for_each(|((param_type, param), stack_offset)| {
            assemble!(instructions {
                mov param_type, stack_offset, param;
            });
        });
}

fn gen_fundef(f: TFunction) -> AsmFunction {
    let TFunction {
        name,
        params,
        body,
        global,
    } = f;

    let mut instructions = AsmInstructions::new();
    let params = params.into_iter().map(TValue::Var).collect();
    set_up_parameters(params, &mut instructions);

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
