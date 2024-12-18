use super::gen::CONST_TABLE;
use crate::ast::{AstConst, Identifier};
use crate::semantic_analysis::StaticInit;
use crate::tacky::{StaticVariable, TBinaryOp, TUnaryOp, TValue};

#[derive(Debug)]
pub struct AsmAst {
    pub asm_toplevel_items: Vec<AsmTopLevelItem>,
}

pub type AsmInstructions = Vec<AsmInstruction>;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum AsmType {
    Longword,
    Quadword,
    Double,
}

impl AsmType {
    #[inline]
    pub fn is_double(&self) -> bool {
        matches!(self, Self::Double)
    }

    #[inline]
    pub fn is_quadword(&self) -> bool {
        matches!(self, Self::Quadword)
    }

    #[inline]
    pub fn is_longword(&self) -> bool {
        matches!(self, Self::Longword)
    }
}

#[derive(Debug)]
pub enum AsmTopLevelItem {
    Fun(AsmFunction),
    StaticVar(AsmStaticVar),
    StaticConst(AsmStaticConst),
}

#[derive(Debug)]
pub struct AsmStaticConst {
    pub name: Identifier,
    pub alignment: usize,
    pub init: StaticInit,
}

#[derive(Debug)]
pub struct AsmStaticVar {
    pub name: Identifier,
    pub global: bool,
    pub init: StaticInit,
    pub alignment: usize,
}

#[derive(Debug)]
pub struct AsmFunction {
    pub name: String,
    pub body: AsmInstructions,
    pub global: bool,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AsmInstruction {
    Cvttsd2si(AsmType, Operand, Operand),
    Cvtsi2sd(AsmType, Operand, Operand),
    Call(Identifier),
    Push(Operand),
    CmovCC(AsmType, Condition, Operand, Operand),
    Mov(AsmType, Operand, Operand),
    Movsx(Operand, Operand),
    MovZX(Operand, Operand),
    Unary(AsmType, AsmUnaryOp, Operand),
    Binary(AsmType, AsmBinaryOp, Operand, Operand),
    Cmp(AsmType, Operand, Operand),
    Jmp(Identifier),
    JmpCC(Condition, Identifier),
    SetCC(Condition, Operand),
    Label(Identifier),
    Div(AsmType, Operand),
    Idiv(AsmType, Operand),
    Cdq(AsmType),
    Ret,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Condition {
    E,
    NE,
    //Signed:
    G,
    GE,
    L,
    LE,
    //Unsigned:
    A,
    AE,
    B,
    BE,
    //Parity
    P,
    NP,
}

impl Condition {
    pub fn to_unsigned(self) -> Self {
        match self {
            Self::LE => Self::BE,
            Self::L => Self::B,
            Self::GE => Self::AE,
            Self::G => Self::A,
            _ => self,
        }
    }

    pub fn to_signed(self) -> Self {
        match self {
            Self::BE => Self::LE,
            Self::B => Self::L,
            Self::AE => Self::GE,
            Self::A => Self::G,
            _ => self,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum AsmUnaryOp {
    Neg,
    Not,
    //Unsigned shift right only once
    Shr,
    //(Un)signed shift left only once
    Sal,
    //Signed shift right only once
    Sar,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Operand {
    Imm(i128),
    Reg(Register),
    Pseudo(Identifier),
    Stack(i64),
    Data(Identifier),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum AsmBinaryOp {
    Add,
    Sub,
    Imul,
    And,
    DivDouble,
    Xor,
    Or,
    //(Un)signed left CL times
    Sal,
    //Signed right CL times
    Sar,
    //Unsigned right CL times
    Shr,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Register {
    AX,
    DX,
    CX,
    DI,
    SI,
    R8,
    R9,
    R10,
    R11,
    SP,
    XMM0,
    XMM1,
    XMM2,
    XMM3,
    XMM4,
    XMM5,
    XMM6,
    XMM7,
    XMM14,
    XMM15,
}

macro_rules! reg_constructor {
    ($name:ident, $variant:ident) => {
        pub fn $name() -> Self {
            Self::$variant
        }
    };
}

impl Register {
    reg_constructor!(xmm0, XMM0);
    reg_constructor!(xmm1, XMM1);
    reg_constructor!(xmm2, XMM2);
    reg_constructor!(xmm3, XMM3);
    reg_constructor!(xmm4, XMM4);
    reg_constructor!(xmm5, XMM5);
    reg_constructor!(xmm6, XMM6);
    reg_constructor!(xmm7, XMM7);
    reg_constructor!(xmm14, XMM14);
    reg_constructor!(xmm15, XMM15);

    reg_constructor!(sp, SP);
    reg_constructor!(rsp, SP);
    reg_constructor!(esp, SP);

    reg_constructor!(ax, AX);
    reg_constructor!(rax, AX);
    reg_constructor!(eax, AX);

    reg_constructor!(si, SI);
    reg_constructor!(rsi, SI);
    reg_constructor!(esi, SI);

    reg_constructor!(di, DI);
    reg_constructor!(edi, DI);
    reg_constructor!(rdi, DI);

    reg_constructor!(cx, CX);
    reg_constructor!(rcx, CX);
    reg_constructor!(ecx, CX);

    reg_constructor!(dx, DX);
    reg_constructor!(rdx, DX);
    reg_constructor!(edx, DX);

    reg_constructor!(r11, R11);
    reg_constructor!(r10, R10);
}

macro_rules! i_typed_constr {
    ($name:ident | q -> $variant:ident ($($arg:ident),+)) => {
        pub fn $name($($arg: Operand),+) -> AsmInstruction {
            AsmInstruction::$variant(AsmType::Quadword, $($arg),+)
        }
    };
    ($name:ident | l -> $variant:ident ($($arg:ident),+)) => {
        pub fn $name($($arg: Operand),+) -> AsmInstruction {
            AsmInstruction::$variant(AsmType::Longword, $($arg),+)
        }
    };
    ($name:ident | d -> $variant:ident ($($arg:ident),+)) => {
        pub fn $name($($arg: Operand),+) -> AsmInstruction {
            AsmInstruction::$variant(AsmType::Double, $($arg),+)
        }
    };

    ($name:ident | q -> #binary:$op:ident) => {
        pub fn $name(src: Operand, dst: Operand) -> AsmInstruction {
            AsmInstruction::Binary(AsmType::Quadword, AsmBinaryOp::$op, src, dst)
        }
    };

    ($name:ident | l -> #binary:$op:ident) => {
        pub fn $name(src: Operand, dst: Operand) -> AsmInstruction {
            AsmInstruction::Binary(AsmType::Longword, AsmBinaryOp::$op, src, dst)
        }
    };

    ($name:ident | d -> #binary: $op:ident) => {
        pub fn $name(src: Operand, dst: Operand) -> AsmInstruction {
            AsmInstruction::Binary(AsmType::Double, AsmBinaryOp::$op, src, dst)
        }
    };

    ($name:ident | q -> #unary:$op:ident) => {
        pub fn $name(src: Operand) -> AsmInstruction {
            AsmInstruction::Unary(AsmType::Quadword, AsmUnaryOp::$op, src)
        }
    };

    ($name:ident | l -> #unary:$op:ident) => {
        pub fn $name(src: Operand) -> AsmInstruction {
            AsmInstruction::Unary(AsmType::Longword, AsmUnaryOp::$op, src)
        }
    };

    ($name:ident | d -> #unary:$op:ident) => {
        pub fn $name(src: Operand) -> AsmInstruction {
            AsmInstruction::Unary(AsmType::Double, AsmUnary::$op, src)
        }
    };
}
macro_rules! i_constr {
    ($name:ident -> $variant:ident($($arg:ident: $type:ty),+)) => {
        pub fn $name($($arg: $type),+) -> AsmInstruction {
            AsmInstruction::$variant($($arg,)+)
        }
    };
    ($name:ident -> #binary: $op:ident) => {
        pub fn $name(asm_type: AsmType, src: Operand, dst: Operand) -> AsmInstruction {
            AsmInstruction::Binary(asm_type, AsmBinaryOp::$op, src, dst)
        }
    };

    ($name:ident -> #unary: $op:ident) => {
        pub fn $name(asm_type: AsmType, src: Operand) -> AsmInstruction {
            AsmInstruction::Unary(asm_type, AsmBinaryOp::$op, src)
        }
    };
}

macro_rules! i_cond_constr {
    ($name:ident | e -> $variant:ident($($args:ident:$type:ty),*)) => {
        pub fn $name($($args: $type),*) -> AsmInstruction {
            AsmInstruction::$variant(Condition::E $(,$args)*)
        }
    };
    ($name:ident | ne -> $variant:ident($($args:ident:$type:ty),*)) => {
        pub fn $name($($args: $type),*) -> AsmInstruction {
            AsmInstruction::$variant(Condition::NE $(,$args)*)
        }
    };
    ($name:ident | np -> $variant:ident($($args:ident:$type:ty),*)) => {
        pub fn $name($($args: $type),*) -> AsmInstruction {
            AsmInstruction::$variant(Condition::NP $(,$args)*)
        }
    };
    ($name:ident | p -> $variant:ident($($args:ident:$type:ty),*)) => {
        pub fn $name($($args: $type),*) -> AsmInstruction {
            AsmInstruction::$variant(Condition::P $(,$args)*)
        }
    };
    ($name:ident | ae -> $variant:ident($($args:ident:$type:ty),*)) => {
        pub fn $name($($args: $type),*) -> AsmInstruction {
            AsmInstruction::$variant(Condition::AE $(,$args)*)
        }
    };
    ($name:ident | l -> $variant:ident($($args:ident:$type:ty),*)) => {
        pub fn $name($($args: $type),*) -> AsmInstruction {
            AsmInstruction::$variant(Condition::L $(,$args)*)
        }
    };
}

impl AsmInstruction {
    i_cond_constr!(sete | e -> SetCC(dst: Operand));
    i_cond_constr!(setne | ne -> SetCC(dst: Operand));
    i_cond_constr!(setp | p -> SetCC(dst: Operand));
    i_cond_constr!(setnp | np -> SetCC(dst: Operand));
    i_constr!(setcc -> SetCC(condition: Condition, dst: Operand));

    i_cond_constr!(je | e -> JmpCC(dst: String));
    i_cond_constr!(jne | ne -> JmpCC(dst: String));
    i_cond_constr!(jp | p -> JmpCC(dst: String));
    i_cond_constr!(jnp | np -> JmpCC(dst: String));
    i_cond_constr!(jae | ae -> JmpCC(dst: String));
    i_cond_constr!(jl | l -> JmpCC(dst: String));
    i_constr!(jmp -> Jmp(label: String));

    i_typed_constr!(comisd | d -> Cmp(src, dst));
    i_typed_constr!(cmpq | q -> Cmp(src, dst));
    i_typed_constr!(cmpl | l -> Cmp(src, dst));
    i_constr!(cmp -> Cmp(t: AsmType, src: Operand, dst: Operand));

    i_typed_constr!(movq | q -> Mov(src, dst));
    i_typed_constr!(movl | l -> Mov(src, dst));
    i_typed_constr!(movsd | d -> Mov(src, dst));
    i_constr!(mov -> Mov(t: AsmType, src: Operand, dst: Operand));
    i_constr!(movzx -> MovZX(src: Operand, dst: Operand));
    i_constr!(movsx -> Movsx(src: Operand, dst: Operand));

    i_typed_constr!(subq | q -> #binary:Sub);
    i_typed_constr!(subl | l -> #binary:Sub);
    i_typed_constr!(subsd | d -> #binary:Sub);
    i_constr!(sub -> #binary:Sub);

    i_typed_constr!(addq | q -> #binary:Add);
    i_typed_constr!(addl | l -> #binary:Add);
    i_typed_constr!(addsd | d -> #binary:Add);

    i_typed_constr!(xorq | q -> #binary:Xor);
    i_typed_constr!(xorl | l -> #binary:Xor);
    i_typed_constr!(xorsd | d -> #binary:Xor);

    i_typed_constr!(orq | q -> #binary:Or);
    i_typed_constr!(orl | l -> #binary:Or);
    i_constr!(or -> #binary:Or);

    i_typed_constr!(shrq | q -> #unary:Shr);

    i_typed_constr!(andq | q -> #binary:And);
    i_typed_constr!(andl | l -> #binary:And);
    i_constr!(and -> #binary:And);

    i_typed_constr!(cvttsd2siq | q -> Cvttsd2si(src, dst));
    i_typed_constr!(cvttsd2sil | l -> Cvttsd2si(src, dst));
    i_constr!(cvttsd2si -> Cvttsd2si(t: AsmType, src: Operand, dst: Operand));

    i_constr!(cvtsi2sd -> Cvtsi2sd(asm_type: AsmType, src: Operand, dst: Operand));
    i_typed_constr!(cvtsi2sdq | q -> Cvtsi2sd(src, dst));

    i_constr!(label -> Label(name: String));
    i_constr!(push -> Push(src: Operand));

    i_constr!(idiv -> Idiv(asm_type: AsmType, src: Operand));
    i_constr!(div -> Div(asm_type: AsmType, src: Operand));
    i_typed_constr!(divsd | d -> #binary:DivDouble);

    i_constr!(imul -> #binary:Imul);

    i_constr!(call -> Call(name: String));

    i_constr!(cmovcc -> CmovCC(asm_type: AsmType, condition: Condition, src: Operand, dst: Operand));

    pub fn cmovnel(src: Operand, dst: Operand) -> AsmInstruction {
        AsmInstruction::CmovCC(AsmType::Longword, Condition::NE, src, dst)
    }

    pub fn ret() -> AsmInstruction {
        AsmInstruction::Ret
    }

    #[inline]
    pub fn is_zero_extend(&self) -> bool {
        matches!(self, Self::MovZX(_, _))
    }

    pub fn mem_operands(&self) -> bool {
        match self {
            Self::Binary(_, AsmBinaryOp::Imul, _, _) => false,
            Self::Mov(_, src, dst) => src.is_mem() && dst.is_mem(),
            Self::Cmp(t, src, dst) | Self::Binary(t, _, src, dst) if !t.is_double() => {
                src.is_mem() && dst.is_mem()
            }
            _ => false,
        }
    }

    pub fn is_truncate_imm_toobig(&self) -> bool {
        matches!(self, Self::Mov(AsmType::Longword, Operand::Imm(i), _) if *i > i128::from(i32::MAX) || *i < i128::from(i32::MIN))
    }

    pub fn is_movsx_invalid(&self) -> bool {
        matches!(self, Self::Movsx(src, dst) if src.is_imm() || dst.is_mem())
    }

    pub fn is_cmp_sndimm(&self) -> bool {
        matches!(self, Self::Cmp(t, _, Operand::Imm(_)) if !t.is_double())
    }

    pub fn is_mul_sndmem(&self) -> bool {
        matches!(
            self,
            Self::Binary(
                t,
                AsmBinaryOp::Imul,
                _,
                Operand::Stack(_) | Operand::Data(_)
            ) if !t.is_double()
        )
    }

    pub fn binary_double_dst_not_reg(&self) -> bool {
        use AsmBinaryOp as O;
        matches!(self, Self::Binary(AsmType::Double, O::Add | O::Sub | O::Imul | O::DivDouble | O::Xor, _, dst) if !dst.is_reg())
    }

    pub fn is_idiv_constant(&self) -> bool {
        matches!(self, Self::Idiv(_, Operand::Imm(_)))
    }

    pub fn is_div_constant(&self) -> bool {
        matches!(self, Self::Div(_, Operand::Imm(_)))
    }

    pub fn is_mov_immtoobig(&self) -> bool {
        matches!(self,
            Self::Mov(AsmType::Quadword, Operand::Imm(src), _) if *src > i128::from(i32::MAX) || *src < i128::from(i32::MIN))
    }

    pub fn cvttsd2si_dst_not_reg(&self) -> bool {
        matches!(self, Self::Cvttsd2si(_, _, r) if !r.is_reg())
    }

    pub fn comisd_dst_not_reg(&self) -> bool {
        matches!(self, Self::Cmp(AsmType::Double, _, dst) if !dst.is_reg())
    }

    pub fn cvtsi2sd_needs_fix(&self) -> bool {
        matches!(self, Self::Cvtsi2sd(_, src, dst) if src.is_imm() || !dst.is_reg())
    }

    pub fn is_imm_toobig(&self) -> bool {
        let cmp = |i| i > i128::from(i32::MAX) || i < i128::from(i32::MIN);
        match self {
            Self::Binary(
                AsmType::Quadword,
                AsmBinaryOp::Add
                | AsmBinaryOp::Sub
                | AsmBinaryOp::Imul
                | AsmBinaryOp::Or
                | AsmBinaryOp::Xor
                | AsmBinaryOp::And,
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
            AstConst::Int(i) => Self::Imm(i128::from(i)),
            AstConst::Long(i) => Self::Imm(i128::from(i)),
            AstConst::UInt(u) => Self::Imm(i128::from(u)),
            AstConst::ULong(u) => Self::Imm(i128::from(u)),
            AstConst::Double(_) => Self::Data(CONST_TABLE.get_const_name(&value)),
        }
    }
}

impl From<TValue> for Operand {
    fn from(value: TValue) -> Self {
        match value {
            TValue::Constant(u) => Self::from(u),
            TValue::Var(id) => Self::Pseudo(id),
        }
    }
}

impl From<TUnaryOp> for AsmUnaryOp {
    fn from(value: TUnaryOp) -> Self {
        match value {
            TUnaryOp::Complement => AsmUnaryOp::Not,
            TUnaryOp::Negate => AsmUnaryOp::Neg,
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
            StaticInit::Int(_) | StaticInit::UInt(_) => 4,
            StaticInit::Long(_) | StaticInit::ULong(_) => 8,
            StaticInit::Double(_) => 16,
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

impl From<TBinaryOp> for AsmBinaryOp {
    fn from(value: TBinaryOp) -> Self {
        match value {
            TBinaryOp::Add => Self::Add,
            TBinaryOp::Multiply => Self::Imul,
            TBinaryOp::Substract => Self::Sub,
            TBinaryOp::BitwiseAnd => Self::And,
            TBinaryOp::BitwiseOr => Self::Or,
            TBinaryOp::BitwiseXor => Self::Xor,
            TBinaryOp::ShiftLeft => Self::Sal,
            TBinaryOp::ShiftRight => Self::Sar,
            _ => unimplemented!(),
        }
    }
}
