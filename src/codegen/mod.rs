use crate::ast::{AstConst, Type};
use crate::semantic_analysis::{StaticInit, SYM_TABLE};
use crate::tacky::*;
use std::collections::HashMap;
use std::iter::successors;
use std::sync::OnceLock;

static ASM_SYM_TABLE: GlobalAsmSymTable = GlobalAsmSymTable::new();
pub struct GlobalAsmSymTable {
    inner: OnceLock<AsmSymTable>,
}

impl GlobalAsmSymTable {
    const fn new() -> Self {
        Self {
            inner: OnceLock::new(),
        }
    }

    fn init(&self) {
        let names = SYM_TABLE.get_keys();
        let mut result = HashMap::with_capacity(names.len());
        for name in names {
            let entry = SYM_TABLE.get_symbol(&name).unwrap();
            let is_static = entry.attrs.is_static();
            let asm_entry = match entry.sym_type {
                Type::Int => AsmSymTabEntry::Obj {
                    asm_type: AsmType::Longword,
                    is_static,
                },
                Type::Long => AsmSymTabEntry::Obj {
                    asm_type: AsmType::Quadword,
                    is_static,
                },
                Type::Fun { .. } => {
                    let is_defined = entry.attrs.is_fun_defined();
                    AsmSymTabEntry::Fun { is_defined }
                }
            };
            result.insert(name, asm_entry);
        }
        self.inner.set(result).expect("Should not be initialized");
    }

    pub fn get_sym(&self, name: &str) -> Option<AsmSymTabEntry> {
        self.inner
            .get()
            .expect("Should be initialized")
            .get(name)
            .cloned()
    }

    pub fn is_sym_static(&self, name: &str) -> bool {
        self.inner
            .get()
            .expect("Should be initialized")
            .get(name)
            .filter(|sym| sym.is_static())
            .is_some()
    }

    pub fn get_type(&self, name: &str) -> Option<AsmType> {
        self.inner
            .get()
            .expect("Should be initialized")
            .get(name)
            .and_then(AsmSymTabEntry::get_type)
    }
}

type AsmSymTable = HashMap<String, AsmSymTabEntry>;

#[derive(Debug, Clone)]
pub enum AsmSymTabEntry {
    Obj { asm_type: AsmType, is_static: bool },
    Fun { is_defined: bool },
}

impl AsmSymTabEntry {
    pub fn is_static(&self) -> bool {
        match self {
            Self::Obj { is_static, .. } => *is_static,
            _ => false,
        }
    }

    pub fn get_type(&self) -> Option<AsmType> {
        match self {
            Self::Obj { asm_type, .. } => Some(*asm_type),
            Self::Fun { .. } => None,
        }
    }

    pub fn get_size(&self) -> Option<i64> {
        match self {
            Self::Obj { asm_type, .. } => match asm_type {
                AsmType::Longword => Some(4),
                AsmType::Quadword => Some(8),
            },
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct AsmAst {
    pub functions: Vec<AsmTopLevelItem>,
}

pub type AsmInstructions = Vec<AsmInstruction>;
pub type Identifier = String;

#[derive(Debug, Clone, Copy)]
pub enum AsmType {
    Longword,
    Quadword,
}

#[derive(Debug)]
pub enum AsmTopLevelItem {
    Fun(AsmFunction),
    StaticVar(AsmStaticVar),
}

#[derive(Debug)]
pub struct AsmStaticVar {
    pub name: Identifier,
    pub global: bool,
    pub init: StaticInit,
    pub alignment: i32,
}

#[derive(Debug)]
pub struct AsmFunction {
    pub name: String,
    pub body: AsmInstructions,
    pub global: bool,
}

#[derive(Debug, Clone)]
pub enum AsmInstruction {
    Call(Identifier),
    Push(Operand),
    Mov(AsmType, Operand, Operand),
    Movsx(Operand, Operand),
    Unary(AsmType, UnaryOp, Operand),
    Binary(AsmType, BinaryOp, Operand, Operand),
    Cmp(AsmType, Operand, Operand),
    Jmp(Identifier),
    JmpCC(Condition, Identifier),
    SetCC(Condition, Operand),
    Label(Identifier),
    Idiv(AsmType, Operand),
    Cdq(AsmType),
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
    Imm(i64),
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
    Sp,
}

type StackAllocMap = HashMap<Identifier, i64>;

struct StackAllocator {
    offset: i64,
    map: StackAllocMap,
}

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

impl AsmInstruction {
    fn mem_operands(&self) -> bool {
        match self {
            Self::Binary(_, BinaryOp::Imul, _, _) => false,
            Self::Mov(_, src, dst) | Self::Cmp(_, src, dst) | Self::Binary(_, _, src, dst) => {
                src.is_mem() && dst.is_mem()
            }
            _ => false,
        }
    }

    fn is_truncate_imm_toobig(&self) -> bool {
        matches!(self, Self::Mov(AsmType::Longword, Operand::Imm(i), _) if *i > i32::MAX as i64 || *i < i32::MIN as i64)
    }

    fn is_movsx_invalid(&self) -> bool {
        matches!(self, Self::Movsx(src, dst) if src.is_imm() || dst.is_mem())
    }

    fn is_cmp_sndimm(&self) -> bool {
        matches!(self, Self::Cmp(_, _, Operand::Imm(_)))
    }

    fn is_mul_sndmem(&self) -> bool {
        matches!(
            self,
            Self::Binary(_, BinaryOp::Imul, _, Operand::Stack(_) | Operand::Data(_))
        )
    }

    fn is_idiv_constant(&self) -> bool {
        matches!(self, Self::Idiv(_, Operand::Imm(_)))
    }

    fn is_mov_immtoobig(&self) -> bool {
        matches!(self,
            Self::Mov(AsmType::Quadword, Operand::Imm(src), _) if *src > i32::MAX as i64 || *src < i32::MIN as i64)
    }

    fn is_imm_toobig(&self) -> bool {
        let cmp = |i| i > i32::MAX as i64 || i < i32::MIN as i64;
        match self {
            Self::Binary(
                AsmType::Quadword,
                BinaryOp::Add
                | BinaryOp::Sub
                | BinaryOp::Imul
                | BinaryOp::Or
                | BinaryOp::Xor
                | BinaryOp::And,
                Operand::Imm(i),
                _,
            )
            | Self::Cmp(AsmType::Quadword, Operand::Imm(i), _)
            | Self::Push(Operand::Imm(i)) => cmp(*i),
            _ => false,
        }
    }
}

impl Operand {
    pub fn is_mem(&self) -> bool {
        matches!(self, Self::Stack(_) | Self::Data(_))
    }

    pub fn is_reg(&self) -> bool {
        matches!(self, Self::Reg(_))
    }

    pub fn is_imm(&self) -> bool {
        matches!(self, Self::Imm(_))
    }
}

impl From<AstConst> for Operand {
    fn from(value: AstConst) -> Self {
        match value {
            AstConst::Int(i) => Self::Imm(i as i64),
            AstConst::Long(i) => Self::Imm(i),
        }
    }
}
impl From<TValue> for Operand {
    fn from(value: TValue) -> Self {
        match value {
            TValue::Constant(u) => Self::from(u),
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
    fn new() -> Self {
        Self {
            map: StackAllocMap::new(),
            offset: 0,
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

        let entry = ASM_SYM_TABLE.get_sym(&name).expect("Should have it");
        if entry.is_static() {
            return Operand::Data(name);
        }

        let size = entry.get_size().expect("Should not be function");
        self.offset += size;
        self.map.insert(name, -self.offset);
        let result = Operand::Stack(-self.offset);
        self.offset += 8 - size;
        result
    }

    #[allow(clippy::cast_sign_loss)]
    fn get_prologue(&self) -> AsmInstruction {
        let stack_size = self.offset + (16 - (self.offset % 16));
        let sp = Operand::Reg(Register::Sp);
        AsmInstruction::Binary(
            AsmType::Quadword,
            BinaryOp::Sub,
            Operand::Imm(stack_size),
            sp,
        )
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
            TI::Truncate(src, dst) => truncate_to_asm(src, dst, instructions),
            TI::SignExtend(src, dst) => sign_extend_to_asm(src, dst, instructions),
            TI::Return(val) => {
                treturn_to_asm(val, instructions);
            }
            TI::Unary(TUnaryOp::LogicalNot, src, dst) => {
                tlogical_not_to_asm(src, dst, instructions);
            }
            TI::Unary(op, val1, val2) => {
                tunary_to_asm(val1, val2, op, instructions);
            }
            TI::Binary(op, v1, v2, v3) if op.is_relational() => {
                trelational_to_asm(op, v1, v2, v3, instructions);
            }
            TI::Binary(op, v1, v2, v3) if op.is_shift() => {
                tshift_to_asm(op, v1, v2, v3, instructions);
            }
            TI::Binary(op, v1, v2, v3) if op.is_divrem() => {
                tdivrem_to_asm(op, v1, v2, v3, instructions);
            }
            TI::Binary(op, v1, v2, v3) => {
                tbinary_to_asm(op, v1, v2, v3, instructions);
            }
            TI::JumpIfZero(val, target) => {
                tjz_to_asm(val, target, instructions);
            }
            TI::JumpIfNotZero(val, target) => {
                tjnz_to_asm(val, target, instructions);
            }
            TI::Copy(src, dst) => {
                tcopy_to_asm(src, dst, instructions);
            }
            TI::Label(id) => {
                instructions.push(AsmInstruction::Label(id));
            }
            TI::Jump(target) => {
                instructions.push(AsmInstruction::Jmp(target));
            }
            TI::FunCall { name, args, dst } => {
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

fn allocate_stack(instructions: &mut AsmInstructions) {
    let mut sa = StackAllocator::new();
    for inst in instructions.iter_mut() {
        match inst {
            AsmInstruction::SetCC(_, operand)
            | AsmInstruction::Push(operand)
            | AsmInstruction::Unary(_, _, operand)
            | AsmInstruction::Idiv(_, operand) => {
                *operand = sa.allocate_if_pseudo(operand.clone());
            }
            AsmInstruction::Cmp(_, src, dst)
            | AsmInstruction::Mov(_, src, dst)
            | AsmInstruction::Binary(_, _, src, dst)
            | AsmInstruction::Movsx(src, dst) => {
                *src = sa.allocate_if_pseudo(src.clone());
                *dst = sa.allocate_if_pseudo(dst.clone());
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
        let AsmInstruction::Binary(t, op, src, dst) = instruction else {
            unreachable!()
        };
        let temp_reg = Register::R11;
        let mov1 = AsmInstruction::Mov(t, dst.clone(), Operand::Reg(temp_reg));
        let imul = AsmInstruction::Binary(t, op, src, Operand::Reg(temp_reg));
        let mov2 = AsmInstruction::Mov(t, Operand::Reg(temp_reg), dst);
        result.push(mov1);
        result.push(imul);
        result.push(mov2);
    }
    result
}

fn fix_idiv(instruction: AsmInstruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if instruction.is_idiv_constant() {
        let AsmInstruction::Idiv(t, operand) = instruction else {
            unreachable!()
        };
        let temp_reg = Register::R10;
        let mov1 = AsmInstruction::Mov(t, operand, Operand::Reg(temp_reg));
        let idiv = AsmInstruction::Idiv(t, Operand::Reg(temp_reg));
        result.push(mov1);
        result.push(idiv);
    }
    result
}

fn fix_two_memoperands(instruction: AsmInstruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if instruction.mem_operands() {
        let (AsmInstruction::Mov(t, src, dst)
        | AsmInstruction::Binary(t, _, src, dst)
        | AsmInstruction::Cmp(t, src, dst)) = instruction.clone()
        else {
            unreachable!()
        };
        let temp_reg = Register::R10;
        let mov1 = AsmInstruction::Mov(t, src.clone(), Operand::Reg(temp_reg));
        let snd = match instruction {
            AsmInstruction::Binary(t, op, _, _) => {
                AsmInstruction::Binary(t, op, Operand::Reg(temp_reg), dst.clone())
            }
            AsmInstruction::Mov(t, _, _) => {
                AsmInstruction::Mov(t, Operand::Reg(temp_reg), dst.clone())
            }
            AsmInstruction::Cmp(t, _, _) => {
                AsmInstruction::Cmp(t, Operand::Reg(temp_reg), dst.clone())
            }
            _ => unreachable!(),
        };
        result.push(mov1);
        result.push(snd);
    }
    result
}

fn fix_cmp_sndimm(instr: AsmInstruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if let AsmInstruction::Cmp(t, src, dst) = instr {
        let temp_reg = Register::R11;
        let mov = AsmInstruction::Mov(t, dst, Operand::Reg(temp_reg));
        let cmp = AsmInstruction::Cmp(t, src, Operand::Reg(temp_reg));
        result.push(mov);
        result.push(cmp);
    }
    result
}

fn fix_movsx(instr: AsmInstruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if let AsmInstruction::Movsx(src, dst) = instr {
        let r10 = Operand::Reg(Register::R10);
        let r11 = Operand::Reg(Register::R11);
        let mov1 = AsmInstruction::Mov(AsmType::Longword, src, r10.clone());
        let movsx = AsmInstruction::Movsx(r10, r11.clone());
        let mov2 = AsmInstruction::Mov(AsmType::Quadword, r11, dst);
        result.push(mov1);
        result.push(movsx);
        result.push(mov2);
    }
    result
}

fn fix_mov_imm(instr: AsmInstruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if let AsmInstruction::Mov(AsmType::Quadword, src, dst) = instr {
        let r10 = Operand::Reg(Register::R10);
        let mov_to_r10 = AsmInstruction::Mov(AsmType::Quadword, src, r10.clone());
        let mov_from_r10 = AsmInstruction::Mov(AsmType::Quadword, r10, dst);
        result.push(mov_to_r10);
        result.push(mov_from_r10);
    }
    result
}

fn fix_imm_toobig(mut instr: AsmInstruction) -> AsmInstructions {
    use AsmInstruction as I;
    use AsmType as AT;
    let mut result = AsmInstructions::new();
    if let I::Binary(AT::Quadword, _, src, _) | I::Push(src) | I::Cmp(AT::Quadword, src, _) =
        &mut instr
    {
        let r10 = Operand::Reg(Register::R10);
        let save_to_r10 = I::Mov(AT::Quadword, src.clone(), r10.clone());
        *src = r10;
        result.push(save_to_r10);
        result.push(instr);
    }
    result
}

fn fix_with_fixer(
    instructions: &mut AsmInstructions,
    predicate: fn(&AsmInstruction) -> bool,
    fixer: fn(AsmInstruction) -> Vec<AsmInstruction>,
) {
    let indexes: Vec<_> = instructions
        .iter()
        .enumerate()
        .filter(|(_, i)| predicate(i))
        .map(|(i, _)| i)
        .collect();
    let mut count = 0;
    for i in indexes {
        let instr = instructions.remove(i + count);
        let fixed = fixer(instr);
        for instr in fixed {
            instructions.insert(i + count, instr);
            count += 1;
        }
        count -= 1;
    }
}

fn fix_truncate(mut instr: AsmInstruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if let AsmInstruction::Mov(AsmType::Longword, Operand::Imm(src), _) = &mut instr {
        let v = *src as i32;
        *src = v as i64;
    }
    result.push(instr);
    result
}

fn fix_instructions(instructions: &mut AsmInstructions) {
    use AsmInstruction as I;
    fix_with_fixer(instructions, I::is_mul_sndmem, fix_imul);
    fix_with_fixer(instructions, I::is_idiv_constant, fix_idiv);
    fix_with_fixer(instructions, I::is_movsx_invalid, fix_movsx);
    fix_with_fixer(instructions, I::mem_operands, fix_two_memoperands);
    fix_with_fixer(instructions, I::is_cmp_sndimm, fix_cmp_sndimm);
    fix_with_fixer(instructions, I::is_mov_immtoobig, fix_mov_imm);
    fix_with_fixer(instructions, I::is_imm_toobig, fix_imm_toobig);
    fix_with_fixer(instructions, I::is_truncate_imm_toobig, fix_truncate);
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

#[inline]
fn gen_staticvar(staticvar: StaticVariable) -> AsmStaticVar {
    let StaticVariable {
        name,
        global,
        init,
        var_type: _,
    } = staticvar;

    let alignment = match init {
        StaticInit::Int(_) => 4,
        StaticInit::Long(_) => 8,
    };
    AsmStaticVar {
        name,
        global,
        init,
        alignment,
    }
}

fn gen_toplevel_item(item: TopLevelItem) -> AsmTopLevelItem {
    match item {
        TopLevelItem::Fun(tfun) => AsmTopLevelItem::Fun(gen_fundef(tfun)),
        TopLevelItem::Var(staticvar) => AsmTopLevelItem::StaticVar(gen_staticvar(staticvar)),
    }
}

pub fn codegen(ast: TAst) -> AsmAst {
    let TAst { toplevel_items } = ast;
    ASM_SYM_TABLE.init();
    let functions = toplevel_items
        .into_iter()
        .map(gen_toplevel_item)
        .collect::<Vec<_>>();

    AsmAst { functions }
}
