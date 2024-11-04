use std::ops::Deref;

pub type Identifier = String;

#[derive(Debug, Clone)]
pub struct Ast {
    pub declarations: Vec<Declaration>,
}

pub type AstBlockItems = Vec<AstBlockItem>;

#[derive(Clone, Debug)]
pub struct AstBlock {
    pub items: AstBlockItems,
}

#[derive(Debug, Default, Clone, Eq, PartialEq)]
pub enum Type {
    #[default]
    Int,
    Long,
    Fun {
        ptypes: Vec<Type>,
        return_type: Box<Type>,
    },
}

#[derive(Copy, Clone, Debug)]
pub enum StorageClass {
    Static,
    Extern,
    Auto,
}

#[derive(Clone, Debug)]
pub enum AstBlockItem {
    S(Statement),
    D(Declaration),
}

#[derive(Clone, Debug)]
pub enum Declaration {
    Var(VarDec),
    Fun(FunDec),
}

#[derive(Clone, Debug)]
pub struct FunDec {
    pub name: Identifier,
    pub params: Vec<Identifier>,
    pub body: Option<AstBlock>,
    pub storage_class: StorageClass,
    pub fun_type: Type,
}

#[derive(Debug, Clone)]
pub struct VarDec {
    pub name: Identifier,
    pub init: Option<Exp>,
    pub storage_class: StorageClass,
    pub var_type: Type,
}

pub type Cases = Vec<(Option<AstConst>, Identifier)>;

#[derive(Debug, Clone)]
pub struct DoWhile {
    pub body: Box<Statement>,
    pub condition: Exp,
    pub label: Identifier,
}

#[derive(Debug, Clone)]
pub struct While {
    pub condition: Exp,
    pub body: Box<Statement>,
    pub label: Identifier,
}

#[derive(Debug, Clone)]
pub struct For {
    pub init: AstForInit,
    pub condition: Option<Exp>,
    pub post: Option<Exp>,
    pub body: Box<Statement>,
    pub label: Identifier,
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Exp,
    pub then: Box<Statement>,
    pub els: Option<Box<Statement>>,
}

#[derive(Debug, Clone)]
pub struct Switch {
    pub ctrl_exp: Exp,
    pub body: Box<Statement>,
    pub cases: Cases,
    pub label: Identifier,
}

#[derive(Debug, Clone)]
pub struct CasedStatement {
    pub exp: Exp,
    pub body: Box<Statement>,
    pub label: Identifier,
}

#[derive(Debug, Clone)]
pub struct DCasedStatement {
    pub body: Box<Statement>,
    pub label: Identifier,
}

#[derive(Debug, Clone)]
pub enum Statement {
    While(While),
    DoWhile(DoWhile),
    For(For),
    If(If),
    Switch(Switch),
    Cased(CasedStatement),
    DCased(DCasedStatement),
    Labeled(Identifier, Box<Statement>),
    Continue(Identifier),
    Compound(AstBlock),
    Break(Identifier),
    Goto(Identifier),
    Return(Exp),
    Exp(Exp),
    Null,
}

#[derive(Debug, Clone)]
pub enum AstForInit {
    InitDecl(VarDec),
    InitExp(Option<Exp>),
}

#[derive(Debug, Clone)]
pub struct ConditionalExp {
    pub condition: Box<Exp>,
    pub then: Box<Exp>,
    pub els: Box<Exp>,
}

impl From<Exp> for UntypedExp {
    fn from(value: Exp) -> Self {
        match value {
            Exp::Untyped(ue) | Exp::Typed(_, ue) => ue,
        }
    }
}

impl From<UntypedExp> for Exp {
    fn from(value: UntypedExp) -> Self {
        Self::Untyped(value)
    }
}

#[derive(Debug, Clone)]
pub enum Exp {
    Typed(Type, UntypedExp),
    Untyped(UntypedExp),
}

// Reminder: &Exp will act like &UntypedExp
impl Deref for Exp {
    type Target = UntypedExp;
    fn deref(&self) -> &Self::Target {
        let (Self::Typed(_, e) | Self::Untyped(e)) = self;
        e
    }
}

impl Exp {
    pub fn get_type(&self) -> Option<Type> {
        match self {
            Self::Typed(t, _) | Self::Untyped(UntypedExp::Cast(t, _)) => Some(t.clone()),
            Self::Untyped(_) => None,
        }
    }

    pub fn set_type(self, t: Type) -> Self {
        let (Self::Typed(_, e) | Self::Untyped(e)) = self;
        Self::Typed(t, e)
    }

    pub fn conditional(c: ConditionalExp) -> Self {
        Self::Untyped(UntypedExp::Conditional(c))
    }

    pub fn cast(t: Type, e: Box<Exp>) -> Self {
        Self::Untyped(UntypedExp::Cast(t, e))
    }

    pub fn binary(op: AstBinaryOp, src: Box<Exp>, dst: Box<Exp>) -> Self {
        Self::Untyped(UntypedExp::Binary(op, src, dst))
    }

    pub fn unary(op: AstUnaryOp, e: Box<Exp>) -> Self {
        Self::Untyped(UntypedExp::Unary(op, e))
    }

    pub fn assignment(dst: Box<Exp>, src: Box<Exp>) -> Self {
        Self::Untyped(UntypedExp::Assignment(dst, src))
    }

    pub fn call(name: Identifier, args: Vec<Exp>) -> Self {
        Self::Untyped(UntypedExp::Call(name, args))
    }

    pub fn var(name: Identifier) -> Self {
        Self::Untyped(UntypedExp::Var(name))
    }

    pub fn constant(cs: AstConst) -> Self {
        Self::Untyped(UntypedExp::Constant(cs))
    }

    pub fn constant_from(i: i64) -> Self {
        let cs = if i < i64::from(i32::MAX) {
            AstConst::Int(i as i32)
        } else {
            AstConst::Long(i)
        };

        Self::Untyped(UntypedExp::Constant(cs))
    }
}

#[derive(Debug, Clone)]
pub enum UntypedExp {
    Conditional(ConditionalExp),
    Cast(Type, Box<Exp>),
    Binary(AstBinaryOp, Box<Exp>, Box<Exp>),
    Unary(AstUnaryOp, Box<Exp>),
    Assignment(Box<Exp>, Box<Exp>),
    Call(Identifier, Vec<Exp>),
    Var(Identifier),
    Constant(AstConst),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum AstConst {
    Int(i32),
    Long(i64),
}

impl AstConst {
    pub fn is_negative(&self) -> bool {
        match self {
            Self::Int(i) => *i < 0,
            Self::Long(l) => *l < 0,
        }
    }

    pub fn abs(&self) -> Self {
        match self {
            Self::Int(i) => Self::Int(i32::abs(*i)),
            Self::Long(l) => Self::Long(i64::abs(*l)),
        }
    }

    #[allow(clippy::cast_possible_truncation)]
    pub fn new(t: &Type, v: i64) -> Option<Self> {
        match t {
            Type::Int => Some(AstConst::Int(v as i32)),
            Type::Long => Some(AstConst::Long(v)),
            Type::Fun { .. } => None,
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            Self::Int(_) => Type::Int,
            Self::Long(_) => Type::Long,
        }
    }

    fn get_value_i64(&self) -> i64 {
        match self {
            Self::Int(i) => i64::from(*i),
            Self::Long(i) => *i,
        }
    }

    #[allow(clippy::cast_possible_truncation)]
    pub fn convert_to(&self, t: &Type) -> Self {
        let self_type = self.get_type();
        let value = self.get_value_i64();
        match t {
            t if t == &self_type => *self,
            Type::Int => AstConst::Int(value as i32),
            Type::Long => AstConst::Long(value),
            Type::Fun { .. } => *self,
        }
    }
}

impl std::fmt::Display for AstConst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstConst::Int(i) => write!(f, "{i}"),
            AstConst::Long(i) => write!(f, "{i}"),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum AstBinaryOp {
    Add,
    Multiply,
    Div,
    Mod,
    Substract,
    LogicalAnd,
    LogicalOr,
    IsEqual,
    IsNotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
}

#[derive(Debug, Clone, Copy)]
pub enum AstUnaryOp {
    Complement,
    Negate,
    LogicalNot,
    PostfixDecrement,
    PrefixDecrement,
    PostfixIncrement,
    PrefixIncrement,
}

impl AstUnaryOp {
    pub fn is_prefix_incdec(&self) -> bool {
        matches!(self, Self::PrefixIncrement | Self::PrefixDecrement)
    }
    pub fn is_postfix_incdec(&self) -> bool {
        matches!(self, Self::PostfixIncrement | Self::PostfixDecrement)
    }
}

impl AstBinaryOp {
    pub fn is_shift(&self) -> bool {
        matches!(self, Self::ShiftLeft | Self::ShiftRight)
    }
    pub fn is_eq(&self) -> bool {
        matches!(
            self,
            Self::IsEqual
                | Self::IsNotEqual
                | Self::LessThan
                | Self::GreaterThan
                | Self::LessOrEqual
                | Self::GreaterOrEqual
        )
    }
}

impl Type {
    #[inline]
    pub fn is_function(&self) -> bool {
        matches!(self, Type::Fun { .. })
    }

    pub fn get_ptypes(&self) -> Option<&Vec<Type>> {
        match self {
            Self::Fun { ptypes, .. } => Some(ptypes),
            _ => None,
        }
    }

    pub fn get_rtype(&self) -> Option<&Type> {
        match self {
            Self::Fun { return_type, .. } => Some(return_type),
            _ => None,
        }
    }

    pub fn get_common(t1: &Self, t2: &Self) -> Self {
        if t1 == t2 {
            t1.clone()
        } else {
            Self::Long
        }
    }
}

impl UntypedExp {
    pub fn is_var(&self) -> bool {
        matches!(self, Self::Var(_))
    }
}

impl StorageClass {
    pub fn is_static(&self) -> bool {
        matches!(self, Self::Static)
    }

    pub fn is_extern(&self) -> bool {
        matches!(self, Self::Extern)
    }

    pub fn is_auto(&self) -> bool {
        matches!(self, Self::Auto)
    }
}
