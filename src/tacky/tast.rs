use crate::ast::{AstBinaryOp, AstConst, AstUnaryOp, Identifier, Type};
use crate::semantic_analysis::{StaticInit, SYM_TABLE};

#[derive(Clone, Debug)]
pub struct TAst {
    pub toplevel_items: Vec<TopLevelItem>,
}

pub type TInstructions = Vec<TInstruction>;

#[derive(Debug, Clone)]
pub struct TFunction {
    pub name: Identifier,
    pub params: Vec<Identifier>,
    pub body: TInstructions,
    pub global: bool,
}

#[derive(Debug, Clone)]
pub struct StaticVariable {
    pub name: Identifier,
    pub global: bool,
    pub init: StaticInit,
    pub var_type: Type,
}

#[derive(Debug, Clone)]
pub enum TopLevelItem {
    Fun(TFunction),
    Var(StaticVariable),
}

#[derive(Clone, Debug)]
pub enum TInstruction {
    Truncate(TValue, TValue),   //(src, dst)
    SignExtend(TValue, TValue), // (src,dst)
    ZeroExtend(TValue, TValue),
    Return(TValue),
    Unary(TUnaryOp, TValue, TValue),
    Binary(TBinaryOp, TValue, TValue, TValue),
    Copy(TValue, TValue),
    Jump(Identifier),
    JumpIfZero(TValue, Identifier),
    JumpIfNotZero(TValue, Identifier),
    Label(Identifier),
    FunCall {
        name: Identifier,
        args: Vec<TValue>,
        dst: TValue,
    },
}

#[derive(Copy, Clone, Debug)]
pub enum TBinaryOp {
    Add,
    Substract,
    Multiply,
    Divide,
    Reminder,
    IsEqual,
    IsNotEqual,
    IsLessThan,
    IsLessOrEqual,
    IsGreaterThan,
    IsGreaterOrEqual,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
}

#[derive(Clone, Debug)]
pub enum TValue {
    Constant(AstConst),
    Var(Identifier),
}

impl TValue {
    pub fn get_type(&self) -> Type {
        match self {
            Self::Constant(c) => c.get_type(),
            Self::Var(name) => SYM_TABLE.get_type(name).expect("Should be in symbol table"),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum TUnaryOp {
    Complement,
    Negate,
    LogicalNot,
}

impl TBinaryOp {
    pub fn is_shift(self) -> bool {
        matches!(self, TBinaryOp::ShiftLeft | TBinaryOp::ShiftRight)
    }
    pub fn is_divrem(self) -> bool {
        matches!(self, TBinaryOp::Divide | TBinaryOp::Reminder)
    }

    pub fn is_rem(self) -> bool {
        matches!(self, TBinaryOp::Reminder)
    }

    pub fn is_relational(self) -> bool {
        matches!(
            self,
            Self::IsEqual
                | Self::IsNotEqual
                | Self::IsLessThan
                | Self::IsLessOrEqual
                | Self::IsGreaterThan
                | Self::IsGreaterOrEqual
        )
    }
}

impl From<AstUnaryOp> for TUnaryOp {
    fn from(value: AstUnaryOp) -> Self {
        match value {
            AstUnaryOp::Complement => TUnaryOp::Complement,
            AstUnaryOp::Negate => TUnaryOp::Negate,
            AstUnaryOp::LogicalNot => TUnaryOp::LogicalNot,
            _ => unimplemented!(),
        }
    }
}

impl From<AstBinaryOp> for TBinaryOp {
    fn from(value: AstBinaryOp) -> Self {
        match value {
            AstBinaryOp::Add => Self::Add,
            AstBinaryOp::Substract => Self::Substract,
            AstBinaryOp::Mod => Self::Reminder,
            AstBinaryOp::Multiply => Self::Multiply,
            AstBinaryOp::Div => Self::Divide,
            AstBinaryOp::IsEqual => Self::IsEqual,
            AstBinaryOp::IsNotEqual => Self::IsNotEqual,
            AstBinaryOp::LessThan => Self::IsLessThan,
            AstBinaryOp::LessOrEqual => Self::IsLessOrEqual,
            AstBinaryOp::GreaterThan => Self::IsGreaterThan,
            AstBinaryOp::GreaterOrEqual => Self::IsGreaterOrEqual,
            AstBinaryOp::BitwiseAnd => Self::BitwiseAnd,
            AstBinaryOp::BitwiseOr => Self::BitwiseOr,
            AstBinaryOp::BitwiseXor => Self::BitwiseXor,
            AstBinaryOp::ShiftLeft => Self::ShiftLeft,
            AstBinaryOp::ShiftRight => Self::ShiftRight,
            _ => unimplemented!(),
        }
    }
}
