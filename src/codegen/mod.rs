use crate::semantic_analysis::SYM_TABLE;
use crate::tacky::*;

use std::collections::HashMap;
use std::iter::successors;

#[derive(Debug)]
pub struct AsmAst {
    pub functions: Vec<AsmTopLevelItem>,
}

pub type AsmInstructions = Vec<AsmInstruction>;
pub type Identifier = String;

#[derive(Debug)]
pub enum AsmTopLevelItem {
    Fun(AsmFunction),
    StaticVar(AsmStaticVar),
}

#[derive(Debug)]
pub struct AsmStaticVar {
    pub name: Identifier,
    pub global: bool,
    pub init: i64,
}

#[derive(Debug)]
pub struct AsmFunction {
    pub name: String,
    pub body: AsmInstructions,
    pub global: bool,
}

#[derive(Debug)]
pub enum AsmInstruction {
    Call(Identifier),
    Push(Operand),
    AllocateStack(u64),
    DeallocateStack(u64),
    Mov(Operand, Operand),
    Unary(UnaryOp, Operand),
    Binary(BinaryOp, Operand, Operand),
    Cmp(Operand, Operand),
    Jmp(Identifier),
    JmpCC(Condition, Identifier),
    SetCC(Condition, Operand),
    Label(Identifier),
    Idiv(Operand),
    Cdq,
    Ret,
}

#[derive(Copy, Clone, Debug)]
pub enum Condition {
    E,
    NE,
    G,
    GE,
    L,
    LE,
}

#[derive(Copy, Clone, Debug)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Clone, Debug)]
pub enum Operand {
    Imm(u64),
    Reg(Register),
    Pseudo(Identifier),
    Stack(i64),
    Data(Identifier),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Imul,
    And,
    Xor,
    Or,
    Shl,
    Shr,
}

#[derive(Copy, Clone, Debug)]
pub enum Register {
    Ax,
    Dx,
    Cx,
    Di,
    Si,
    R8,
    R9,
    R10,
    R11,
}

type StackAllocMap = HashMap<Identifier, i64>;

struct StackAllocator {
    offset: i64,
    map: StackAllocMap,
}

impl AsmInstruction {
    fn mem_operands(&self) -> bool {
        match self {
            Self::Binary(BinaryOp::Imul, _, _) => false,
            Self::Mov(src, dst) | Self::Cmp(src, dst) | Self::Binary(_, src, dst) => {
                src.is_mem() && dst.is_mem()
            }
            _ => false,
        }
    }

    fn is_cmp_sndimm(&self) -> bool {
        matches!(self, Self::Cmp(_, Operand::Imm(_)))
    }

    fn is_mul_sndmem(&self) -> bool {
        matches!(
            self,
            Self::Binary(BinaryOp::Imul, _, Operand::Stack(_) | Operand::Data(_))
        )
    }

    fn is_idiv_constant(&self) -> bool {
        matches!(self, Self::Idiv(Operand::Imm(_)))
    }
}

impl Operand {
    fn is_mem(&self) -> bool {
        matches!(self, Self::Stack(_) | Self::Data(_))
    }

    pub fn is_reg(&self) -> bool {
        matches!(self, Self::Reg(_))
    }
}

impl From<TValue> for Operand {
    fn from(value: TValue) -> Self {
        match value {
            TValue::Constant(u) => Self::Imm(u),
            TValue::Var(id) => Self::Pseudo(id.clone()),
        }
    }
}

impl From<TUnaryOp> for UnaryOp {
    fn from(value: TUnaryOp) -> Self {
        match value {
            TUnaryOp::Complement => UnaryOp::Not,
            TUnaryOp::Negate => UnaryOp::Neg,
            TUnaryOp::LogicalNot => unreachable!(),
        }
    }
}

impl StackAllocator {
    fn new_with_reserve(count: i64) -> Self {
        Self {
            offset: count,
            map: StackAllocMap::new(),
        }
    }

    fn allocate_if_pseudo(&mut self, operand: Operand) -> Operand {
        match operand {
            Operand::Pseudo(name) => self.allocate(name),
            _ => operand,
        }
    }

    fn allocate(&mut self, name: Identifier) -> Operand {
        if let Some(entry) = self.map.get(&name) {
            return Operand::Stack(*entry);
        }

        if SYM_TABLE.is_sym_static(&name) {
            return Operand::Data(name);
        }

        self.offset += 4;
        self.map.insert(name, -self.offset);
        Operand::Stack(-self.offset)
    }

    #[allow(clippy::cast_sign_loss)]
    fn get_prologue(&self) -> AsmInstruction {
        let stack_size = self.offset + (16 - (self.offset % 16));
        AsmInstruction::AllocateStack(stack_size as u64)
    }
}

impl From<TBinaryOp> for BinaryOp {
    fn from(value: TBinaryOp) -> Self {
        match value {
            TBinaryOp::Add => Self::Add,
            TBinaryOp::Multiply => Self::Imul,
            TBinaryOp::Substract => Self::Sub,
            TBinaryOp::BitwiseAnd => Self::And,
            TBinaryOp::BitwiseOr => Self::Or,
            TBinaryOp::BitwiseXor => Self::Xor,
            TBinaryOp::ShiftLeft => Self::Shl,
            TBinaryOp::ShiftRight => Self::Shr,
            _ => unimplemented!(),
        }
    }
}

fn tbinary_to_asm(instructions: &mut AsmInstructions, tinstr: TInstruction) {
    if let TInstruction::Binary(op, val1, val2, val3) = tinstr {
        let src1 = Operand::from(val1);
        let src2 = Operand::from(val2);
        let dst = Operand::from(val3);
        let is_div = op.is_div();
        let is_rem = op.is_rem();

        if op.is_shift() {
            let cx = Operand::Reg(Register::Cx);
            let op = BinaryOp::from(op);
            let mov = AsmInstruction::Mov(src1, dst.clone());
            let mov2 = AsmInstruction::Mov(src2, cx.clone());
            let operation = AsmInstruction::Binary(op, cx, dst);

            instructions.push(mov);
            instructions.push(mov2);
            instructions.push(operation);
        } else if is_div || is_rem {
            let ax = Operand::Reg(Register::Ax);
            let dx = Operand::Reg(Register::Dx);
            let mov1 = AsmInstruction::Mov(src1, ax.clone());
            let cdq = AsmInstruction::Cdq;
            let idiv = AsmInstruction::Idiv(src2);
            let last = if is_rem { dx } else { ax };
            let mov2 = AsmInstruction::Mov(last, dst);

            instructions.push(mov1);
            instructions.push(cdq);
            instructions.push(idiv);
            instructions.push(mov2);
        } else {
            let op = BinaryOp::from(op);
            let mov = AsmInstruction::Mov(src1, dst.clone());
            let operation = AsmInstruction::Binary(op, src2, dst);

            instructions.push(mov);
            instructions.push(operation);
        }
    }
}

//TODO: Change to TryFrom<_>
impl From<TBinaryOp> for Condition {
    fn from(value: TBinaryOp) -> Self {
        match value {
            TBinaryOp::IsGreaterThan => Self::G,

            TBinaryOp::IsGreaterOrEqual => Self::GE,
            TBinaryOp::IsEqual => Self::E,
            TBinaryOp::IsNotEqual => Self::NE,
            TBinaryOp::IsLessThan => Self::L,
            TBinaryOp::IsLessOrEqual => Self::LE,
            _ => unreachable!(),
        }
    }
}

fn tcomp_to_asm(instructions: &mut AsmInstructions, instr: TInstruction) {
    match instr {
        TInstruction::Binary(op, src1, src2, dst) => {
            let src1 = Operand::from(src1);
            let src2 = Operand::from(src2);
            let dst = Operand::from(dst);
            let cmp = AsmInstruction::Cmp(src2, src1);
            let mov = AsmInstruction::Mov(Operand::Imm(0), dst.clone());
            let setcc = AsmInstruction::SetCC(Condition::from(op), dst);
            instructions.push(cmp);
            instructions.push(mov);
            instructions.push(setcc);
        }
        _ => unreachable!(),
    }
}

fn tacky_to_asm(body: TInstructions, instructions: &mut AsmInstructions) {
    for inst in body {
        match inst {
            TInstruction::Return(val) => {
                treturn_to_asm(val, instructions);
            }
            TInstruction::Unary(TUnaryOp::LogicalNot, src, dst) => {
                tlogical_not_to_asm(src, dst, instructions);
            }
            TInstruction::Unary(op, val1, val2) => {
                tunary_to_asm(val1, val2, op, instructions);
            }
            TInstruction::Binary(op, _, _, _) => {
                if op.is_comp() {
                    tcomp_to_asm(instructions, inst);
                } else {
                    tbinary_to_asm(instructions, inst);
                }
            }
            TInstruction::JumpIfZero(val, target) => {
                tjz_to_asm(val, target, instructions);
            }
            TInstruction::JumpIfNotZero(val, target) => {
                tjnz_to_asm(val, target, instructions);
            }
            TInstruction::Copy(src, dst) => {
                tcopy_to_asm(src, dst, instructions);
            }
            TInstruction::Label(id) => {
                instructions.push(AsmInstruction::Label(id));
            }
            TInstruction::Jump(target) => {
                instructions.push(AsmInstruction::Jmp(target));
            }
            TInstruction::FunCall { name, args, dst } => {
                tfun_call_to_asm(name, args, dst, instructions);
            }
        }
    }
}

fn tfun_call_to_asm(
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
        let allocate_stack = AsmInstruction::AllocateStack(stack_padding as u64);
        instructions.push(allocate_stack);
    }

    let mov_reg_args = args
        .iter()
        .take(6)
        .cloned()
        .map(Operand::from)
        .zip(reg_operands)
        .map(|(operand, reg)| AsmInstruction::Mov(operand, reg));
    instructions.extend(mov_reg_args);

    let stack_args = args.into_iter().skip(6).rev().map(Operand::from);

    for stack_arg in stack_args {
        if matches!(stack_arg, Operand::Reg(_) | Operand::Imm(_)) {
            let push = AsmInstruction::Push(stack_arg);
            instructions.push(push);
        } else {
            let ax = Operand::Reg(Register::Ax);
            let save_to_ax = AsmInstruction::Mov(stack_arg, ax.clone());
            let push_ax = AsmInstruction::Push(ax);
            instructions.push(save_to_ax);
            instructions.push(push_ax);
        }
    }
    let call = AsmInstruction::Call(name);
    instructions.push(call);

    let bytes_to_remove = 8 * stack_args_count + stack_padding;

    if bytes_to_remove != 0 {
        let dealloc = AsmInstruction::DeallocateStack(bytes_to_remove as u64);
        instructions.push(dealloc);
    }
    let asm_dst = Operand::from(dst);
    let ax = Operand::Reg(Register::Ax);
    let mov = AsmInstruction::Mov(ax, asm_dst);
    instructions.push(mov);
}

fn tunary_to_asm(val1: TValue, val2: TValue, op: TUnaryOp, instructions: &mut AsmInstructions) {
    let src = Operand::from(val1);
    let dst = Operand::from(val2);
    let op = UnaryOp::from(op);
    let mov = AsmInstruction::Mov(src, dst.clone());
    let unary = AsmInstruction::Unary(op, dst);

    instructions.push(mov);
    instructions.push(unary);
}

fn tlogical_not_to_asm(src: TValue, dst: TValue, instructions: &mut AsmInstructions) {
    let src = Operand::from(src);
    let dst = Operand::from(dst);
    let cmp = AsmInstruction::Cmp(Operand::Imm(0), src);
    let mov = AsmInstruction::Mov(Operand::Imm(0), dst.clone());
    let setcc = AsmInstruction::SetCC(Condition::E, dst);

    instructions.push(cmp);
    instructions.push(mov);
    instructions.push(setcc);
}

fn treturn_to_asm(val: TValue, instructions: &mut AsmInstructions) {
    let src = Operand::from(val);
    let dst = Operand::Reg(Register::Ax);
    let mov = AsmInstruction::Mov(src, dst);
    let ret = AsmInstruction::Ret;

    instructions.push(mov);
    instructions.push(ret);
}

fn tjz_to_asm(val: TValue, target: String, instructions: &mut AsmInstructions) {
    let src = Operand::from(val);
    let cmp = AsmInstruction::Cmp(Operand::Imm(0), src);
    let jmp = AsmInstruction::JmpCC(Condition::E, target);

    instructions.push(cmp);
    instructions.push(jmp);
}

fn tjnz_to_asm(val: TValue, target: String, instructions: &mut AsmInstructions) {
    let src = Operand::from(val);
    let cmp = AsmInstruction::Cmp(Operand::Imm(0), src);
    let jmp = AsmInstruction::JmpCC(Condition::NE, target);

    instructions.push(cmp);
    instructions.push(jmp);
}

fn tcopy_to_asm(src: TValue, dst: TValue, instructions: &mut Vec<AsmInstruction>) {
    let src = Operand::from(src);
    let dst = Operand::from(dst);
    let mov = AsmInstruction::Mov(src, dst);

    instructions.push(mov);
}

fn allocate_stack(instructions: &mut AsmInstructions, nparams: usize) {
    let mut sa = StackAllocator::new_with_reserve(8 * nparams as i64);
    for inst in instructions.iter_mut() {
        match inst {
            AsmInstruction::SetCC(_, operand)
            | AsmInstruction::Push(operand)
            | AsmInstruction::Unary(_, operand)
            | AsmInstruction::Idiv(operand) => {
                let allocated = sa.allocate_if_pseudo(operand.clone());
                *operand = allocated;
            }
            AsmInstruction::Cmp(src, dst) => {
                let src = sa.allocate_if_pseudo(src.clone());
                let dst = sa.allocate_if_pseudo(dst.clone());
                *inst = AsmInstruction::Cmp(src, dst);
            }
            AsmInstruction::Mov(src, dst) => {
                let src = sa.allocate_if_pseudo(src.clone());
                let dst = sa.allocate_if_pseudo(dst.clone());
                *inst = AsmInstruction::Mov(src, dst);
            }
            AsmInstruction::Binary(op, operand1, operand2) => {
                let operand1 = sa.allocate_if_pseudo(operand1.clone());
                let operand2 = sa.allocate_if_pseudo(operand2.clone());
                *inst = AsmInstruction::Binary(*op, operand1, operand2);
            }
            _ => (),
        }
    }

    let prologue = sa.get_prologue();
    instructions.insert(0, prologue);
}

fn fix_imul(instruction: AsmInstruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if instruction.is_mul_sndmem() {
        let AsmInstruction::Binary(op, src, dst) = instruction else {
            unreachable!()
        };
        let temp_reg = Register::R11;
        let mov1 = AsmInstruction::Mov(dst.clone(), Operand::Reg(temp_reg));
        let imul = AsmInstruction::Binary(op, src, Operand::Reg(temp_reg));
        let mov2 = AsmInstruction::Mov(Operand::Reg(temp_reg), dst);
        result.push(mov1);
        result.push(imul);
        result.push(mov2);
    }
    result
}

fn fix_idiv(instruction: AsmInstruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if instruction.is_idiv_constant() {
        let AsmInstruction::Idiv(operand) = instruction else {
            unreachable!()
        };
        let temp_reg = Register::R10;
        let mov1 = AsmInstruction::Mov(operand, Operand::Reg(temp_reg));
        let idiv = AsmInstruction::Idiv(Operand::Reg(temp_reg));
        result.push(mov1);
        result.push(idiv);
    }
    result
}

fn fix_two_memoperands(instruction: &AsmInstruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if instruction.mem_operands() {
        let (AsmInstruction::Mov(src, dst)
        | AsmInstruction::Binary(_, src, dst)
        | AsmInstruction::Cmp(src, dst)) = instruction
        else {
            unreachable!()
        };
        let temp_reg = Register::R10;
        let mov1 = AsmInstruction::Mov(src.clone(), Operand::Reg(temp_reg));
        let snd = match instruction {
            AsmInstruction::Binary(op, _, _) => {
                AsmInstruction::Binary(*op, Operand::Reg(temp_reg), dst.clone())
            }
            AsmInstruction::Mov(_, _) => AsmInstruction::Mov(Operand::Reg(temp_reg), dst.clone()),
            AsmInstruction::Cmp(_, _) => AsmInstruction::Cmp(Operand::Reg(temp_reg), dst.clone()),
            _ => unreachable!(),
        };
        result.push(mov1);
        result.push(snd);
    }
    result
}

fn fix_cmp_sndimm(instr: AsmInstruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if let AsmInstruction::Cmp(src, dst) = instr {
        let temp_reg = Register::R11;
        let mov = AsmInstruction::Mov(dst, Operand::Reg(temp_reg));
        let cmp = AsmInstruction::Cmp(src, Operand::Reg(temp_reg));
        result.push(mov);
        result.push(cmp);
    }
    result
}

fn fix_instructions(instructions: &mut AsmInstructions) {
    let indexes: Vec<_> = instructions
        .iter()
        .enumerate()
        .filter(|(_, i)| {
            i.mem_operands() || i.is_mul_sndmem() || i.is_idiv_constant() || i.is_cmp_sndimm()
        })
        .map(|(i, _)| i)
        .collect();

    let mut count = 0;
    for i in indexes {
        let instr = instructions.remove(i + count);

        let fixed = if instr.mem_operands() {
            fix_two_memoperands(&instr)
        } else if instr.is_mul_sndmem() {
            fix_imul(instr)
        } else if instr.is_idiv_constant() {
            fix_idiv(instr)
        } else if instr.is_cmp_sndimm() {
            fix_cmp_sndimm(instr)
        } else {
            unreachable!()
        };

        for instr in fixed {
            instructions.insert(i + count, instr);
            count += 1;
        }
        count -= 1;
    }
}

fn gen_fundef(f: TFunction) -> AsmFunction {
    let TFunction {
        name,
        params,
        body,
        global,
    } = f;

    let nparams = params.len();
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
        .map(|(src, dst)| AsmInstruction::Mov(src, dst))
        .collect::<AsmInstructions>();

    tacky_to_asm(body, &mut instructions);
    allocate_stack(&mut instructions, nparams);
    fix_instructions(&mut instructions);

    AsmFunction {
        name,
        body: instructions,
        global,
    }
}

#[inline]
fn gen_staticvar(staticvar: StaticVariable) -> AsmStaticVar {
    let StaticVariable { name, global, init } = staticvar;
    AsmStaticVar { name, global, init }
}

fn gen_toplevel_item(item: TopLevelItem) -> AsmTopLevelItem {
    match item {
        TopLevelItem::Fun(tfun) => AsmTopLevelItem::Fun(gen_fundef(tfun)),
        TopLevelItem::Var(staticvar) => AsmTopLevelItem::StaticVar(gen_staticvar(staticvar)),
    }
}

pub fn codegen(ast: TAst) -> AsmAst {
    let TAst { toplevel_items } = ast;
    let functions = toplevel_items.into_iter().map(gen_toplevel_item).collect();
    AsmAst { functions }
}
