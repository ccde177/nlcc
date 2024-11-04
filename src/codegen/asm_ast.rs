use crate::ast::{AstConst, Identifier};
use crate::semantic_analysis::StaticInit;
use crate::tacky::{StaticVariable, TBinaryOp, TUnaryOp, TValue};

#[derive(Debug)]
pub struct AsmAst {
    pub functions: Vec<AsmTopLevelItem>,
}

pub type AsmInstructions = Vec<AsmInstruction>;

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

impl AsmInstruction {
    pub fn mem_operands(&self) -> bool {
        match self {
            Self::Binary(_, BinaryOp::Imul, _, _) => false,
            Self::Mov(_, src, dst) | Self::Cmp(_, src, dst) | Self::Binary(_, _, src, dst) => {
                src.is_mem() && dst.is_mem()
            }
            _ => false,
        }
    }

    pub fn is_truncate_imm_toobig(&self) -> bool {
        matches!(self, Self::Mov(AsmType::Longword, Operand::Imm(i), _) if *i > i64::from(i32::MAX) || *i < i64::from(i32::MIN))
    }

    pub fn is_movsx_invalid(&self) -> bool {
        matches!(self, Self::Movsx(src, dst) if src.is_imm() || dst.is_mem())
    }

    pub fn is_cmp_sndimm(&self) -> bool {
        matches!(self, Self::Cmp(_, _, Operand::Imm(_)))
    }

    pub fn is_mul_sndmem(&self) -> bool {
        matches!(
            self,
            Self::Binary(_, BinaryOp::Imul, _, Operand::Stack(_) | Operand::Data(_))
        )
    }

    pub fn is_idiv_constant(&self) -> bool {
        matches!(self, Self::Idiv(_, Operand::Imm(_)))
    }

    pub fn is_mov_immtoobig(&self) -> bool {
        matches!(self,
            Self::Mov(AsmType::Quadword, Operand::Imm(src), _) if *src > i64::from(i32::MAX) || *src < i64::from(i32::MIN))
    }

    pub fn is_imm_toobig(&self) -> bool {
        let cmp = |i| i > i64::from(i32::MAX) || i < i64::from(i32::MIN);
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
            AstConst::Int(i) => Self::Imm(i64::from(i)),
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

impl From<StaticVariable> for AsmStaticVar {
    fn from(value: StaticVariable) -> Self {
        let StaticVariable {
            name,
            global,
            init,
            var_type: _,
        } = value;

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
}

impl From<StaticVariable> for AsmTopLevelItem {
    fn from(value: StaticVariable) -> Self {
        Self::StaticVar(value.into())
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
