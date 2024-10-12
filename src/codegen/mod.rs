use crate::tacky::*;

use std::collections::HashMap;

#[derive(Debug)]
pub enum AsmAst {
    Program(Function),
}

pub type AsmInstructions = Vec<Instruction>;
pub type Identifier = String;

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub body: AsmInstructions,
}

#[derive(Debug)]
pub enum Instruction {
    AllocateStack(u64),
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
    Stack(u64),
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

type StackAllocMap = HashMap<Identifier, u64>;

struct StackAllocator {
    offset: u64,
    map: StackAllocMap,
}

impl Instruction {
    fn mem_operands(&self) -> bool {
        match self {
            Self::Mov(src, dst) => src.is_mem() && dst.is_mem(),
            Self::Cmp(src, dst) => src.is_mem() && dst.is_mem(),
            Self::Binary(op, src, dst) => (*op != BinaryOp::Imul) && src.is_mem() && dst.is_mem(),
            _ => false,
        }
    }

    fn is_cmp_sndimm(&self) -> bool {
        match self {
            Self::Cmp(_, dst) => dst.is_imm(),
            _ => false,
        }
    }

    fn is_mul_sndmem(&self) -> bool {
        match self {
            Instruction::Binary(op, _, operand) => *op == BinaryOp::Imul && operand.is_mem(),
            _ => false,
        }
    }

    fn is_idiv_constant(&self) -> bool {
        match self {
            Instruction::Idiv(c) => c.is_imm(),
            _ => false,
        }
    }
}

impl Operand {
    fn is_mem(&self) -> bool {
        matches!(self, Self::Stack(_))
    }

    pub fn is_reg(&self) -> bool {
        matches!(self, Self::Reg(_))
    }

    fn is_imm(&self) -> bool {
        matches!(self, Self::Imm(_))
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Register {
    Ax,
    Dx,
    Cx,
    R10,
    R11,
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
            _ => unreachable!(),
        }
    }
}

impl StackAllocator {
    fn new() -> Self {
        Self {
            offset: 0,
            map: StackAllocMap::new(),
        }
    }

    fn allocate_if_pseudo(&mut self, operand: Operand) -> Operand {
        match operand {
            Operand::Pseudo(name) => {
                let offset = self.allocate(&name);
                Operand::Stack(offset)
            }
            _ => operand,
        }
    }

    fn allocate(&mut self, name: &Identifier) -> u64 {
        if self.map.contains_key(name) {
            return *self.map.get(name).unwrap();
        }
        self.offset += 4;
        self.map.insert(name.clone(), self.offset);
        self.offset
    }

    fn get_prologue(&self) -> Instruction {
        Instruction::AllocateStack(self.offset)
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
            let mov = Instruction::Mov(src1, dst.clone());
            let mov2 = Instruction::Mov(src2, cx.clone());
            let operation = Instruction::Binary(op, cx, dst);
            instructions.push(mov);
            instructions.push(mov2);
            instructions.push(operation);
        } else if is_div || is_rem {
            let ax = Operand::Reg(Register::Ax);
            let dx = Operand::Reg(Register::Dx);
            let mov1 = Instruction::Mov(src1, ax.clone());
            let cdq = Instruction::Cdq;
            let idiv = Instruction::Idiv(src2);
            let last = if is_rem { dx } else { ax };
            let mov2 = Instruction::Mov(last, dst);

            instructions.push(mov1);
            instructions.push(cdq);
            instructions.push(idiv);
            instructions.push(mov2);
        } else {
            let op = BinaryOp::from(op);
            let mov = Instruction::Mov(src1, dst.clone());
            let operation = Instruction::Binary(op, src2, dst);

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
            let cmp = Instruction::Cmp(src2, src1);
            let mov = Instruction::Mov(Operand::Imm(0), dst.clone());
            let setcc = Instruction::SetCC(Condition::from(op), dst);
            instructions.push(cmp);
            instructions.push(mov);
            instructions.push(setcc);
        }
        _ => unreachable!(),
    }
}

fn tacky_to_asm(body: TInstructions) -> AsmInstructions {
    let mut instructions = AsmInstructions::new();
    for inst in body.into_iter() {
        match inst {
            TInstruction::Return(val) => {
                let src = Operand::from(val);
                let dst = Operand::Reg(Register::Ax);
                let mov = Instruction::Mov(src, dst);
                let ret = Instruction::Ret;
                instructions.push(mov);
                instructions.push(ret);
            }
            TInstruction::Unary(TUnaryOp::LogicalNot, src, dst) => {
                let src = Operand::from(src);
                let dst = Operand::from(dst);
                let cmp = Instruction::Cmp(Operand::Imm(0), src);
                let mov = Instruction::Mov(Operand::Imm(0), dst.clone());
                let setcc = Instruction::SetCC(Condition::E, dst);
                instructions.push(cmp);
                instructions.push(mov);
                instructions.push(setcc);
            }
            TInstruction::Unary(op, val1, val2) => {
                let src = Operand::from(val1);
                let dst = Operand::from(val2);
                let op = UnaryOp::from(op);
                let mov = Instruction::Mov(src, dst.clone());
                let unary = Instruction::Unary(op, dst);
                instructions.push(mov);
                instructions.push(unary);
            }
            TInstruction::Binary(op, _, _, _) => {
                if op.is_comp() {
                    tcomp_to_asm(&mut instructions, inst);
                } else {
                    tbinary_to_asm(&mut instructions, inst);
                }
            }
            TInstruction::JumpIfZero(val, target) => {
                let src = Operand::from(val);
                let cmp = Instruction::Cmp(Operand::Imm(0), src);
                let jmp = Instruction::JmpCC(Condition::E, target);
                instructions.push(cmp);
                instructions.push(jmp);
            }
            TInstruction::JumpIfNotZero(val, target) => {
                let src = Operand::from(val);
                let cmp = Instruction::Cmp(Operand::Imm(0), src);
                let jmp = Instruction::JmpCC(Condition::NE, target);
                instructions.push(cmp);
                instructions.push(jmp);
            }
            TInstruction::Copy(src, dst) => {
                let src = Operand::from(src);
                let dst = Operand::from(dst);
                let mov = Instruction::Mov(src, dst);
                instructions.push(mov);
            }
            TInstruction::Label(id) => {
                instructions.push(Instruction::Label(id));
            }
            TInstruction::Jump(target) => {
                instructions.push(Instruction::Jmp(target));
            }
        }
    }
    instructions
}

fn allocate_stack(instructions: &mut AsmInstructions) {
    let mut sa = StackAllocator::new();
    for inst in instructions.iter_mut() {
        match inst {
            Instruction::SetCC(cond, operand) => {
                let operand = sa.allocate_if_pseudo(operand.clone());
                *inst = Instruction::SetCC(*cond, operand);
            }
            Instruction::Cmp(src, dst) => {
                let src = sa.allocate_if_pseudo(src.clone());
                let dst = sa.allocate_if_pseudo(dst.clone());
                *inst = Instruction::Cmp(src, dst);
            }
            Instruction::Unary(op, operand) => {
                let operand = sa.allocate_if_pseudo(operand.clone());
                *inst = Instruction::Unary(*op, operand);
            }
            Instruction::Mov(src, dst) => {
                let src = sa.allocate_if_pseudo(src.clone());
                let dst = sa.allocate_if_pseudo(dst.clone());
                *inst = Instruction::Mov(src, dst);
            }
            Instruction::Binary(op, operand1, operand2) => {
                let operand1 = sa.allocate_if_pseudo(operand1.clone());
                let operand2 = sa.allocate_if_pseudo(operand2.clone());
                *inst = Instruction::Binary(*op, operand1, operand2);
            }
            Instruction::Idiv(operand) => {
                let operand = sa.allocate_if_pseudo(operand.clone());
                *inst = Instruction::Idiv(operand);
            }
            _ => (),
        }
    }
    let prologue = sa.get_prologue();
    instructions.insert(0, prologue);
}

fn fix_imul(instruction: Instruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if instruction.is_mul_sndmem() {
        let (op, src, dst) = match instruction {
            Instruction::Binary(op, src, dst) => (op, src, dst),
            _ => unreachable!(),
        };
        let temp_reg = Register::R11;
        let mov1 = Instruction::Mov(dst.clone(), Operand::Reg(temp_reg));
        let imul = Instruction::Binary(op, src, Operand::Reg(temp_reg));
        let mov2 = Instruction::Mov(Operand::Reg(temp_reg), dst);
        result.push(mov1);
        result.push(imul);
        result.push(mov2);
    }
    result
}

fn fix_idiv(instruction: Instruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if instruction.is_idiv_constant() {
        let operand = match instruction {
            Instruction::Idiv(operand) => operand,
            _ => unreachable!(),
        };
        let temp_reg = Register::R10;
        let mov1 = Instruction::Mov(operand, Operand::Reg(temp_reg));
        let idiv = Instruction::Idiv(Operand::Reg(temp_reg));
        result.push(mov1);
        result.push(idiv);
    }
    result
}

fn fix_two_memoperands(instruction: Instruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if instruction.mem_operands() {
        let (src, dst) = match instruction {
            Instruction::Mov(ref o1, ref o2) => (o1, o2),
            Instruction::Binary(_, ref o1, ref o2) => (o1, o2),
            Instruction::Cmp(ref o1, ref o2) => (o1, o2),
            _ => unreachable!(),
        };
        let temp_reg = Register::R10;
        let mov1 = Instruction::Mov(src.clone(), Operand::Reg(temp_reg));
        let snd = match instruction {
            Instruction::Binary(op, _, _) => {
                Instruction::Binary(op, Operand::Reg(temp_reg), dst.clone())
            }
            Instruction::Mov(_, _) => Instruction::Mov(Operand::Reg(temp_reg), dst.clone()),
            Instruction::Cmp(_, _) => Instruction::Cmp(Operand::Reg(temp_reg), dst.clone()),
            _ => unreachable!(),
        };
        result.push(mov1);
        result.push(snd);
    }
    result
}

fn fix_cmp_sndimm(instr: Instruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if let Instruction::Cmp(src, dst) = instr {
        let temp_reg = Register::R11;
        let mov = Instruction::Mov(dst, Operand::Reg(temp_reg));
        let cmp = Instruction::Cmp(src, Operand::Reg(temp_reg));
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
            fix_two_memoperands(instr)
        } else if instr.is_mul_sndmem() {
            fix_imul(instr)
        } else if instr.is_idiv_constant() {
            fix_idiv(instr)
        } else if instr.is_cmp_sndimm() {
            fix_cmp_sndimm(instr)
        } else {
            unreachable!()
        };

        for instr in fixed.into_iter() {
            instructions.insert(i + count, instr);
            count += 1;
        }
        count -= 1;
    }
}

fn gen_body(body: TInstructions) -> AsmInstructions {
    let mut instructions = tacky_to_asm(body);
    allocate_stack(&mut instructions);
    fix_instructions(&mut instructions);
    instructions
}

fn gen_fundef(f: TFunction) -> Function {
    match f {
        TFunction::FunDef(name, body) => Function {
            name,
            body: gen_body(body),
        },
    }
}

pub fn codegen(ast: TAst) -> AsmAst {
    match ast {
        TAst::Program(f) => AsmAst::Program(gen_fundef(f)),
    }
}
