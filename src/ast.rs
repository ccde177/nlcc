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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
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

pub type Cases = Vec<(Option<u64>, Identifier)>;

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

#[derive(Debug, Clone)]
pub enum Exp {
    Conditional(ConditionalExp),
    Cast(Type, Box<Exp>),
    Binary(AstBinaryOp, Box<Exp>, Box<Exp>),
    Unary(AstUnaryOp, Box<Exp>),
    Assignment(Box<Exp>, Box<Exp>),
    Call(Identifier, Vec<Exp>),
    Var(Identifier),
    Constant(AstConst),
}

#[derive(Debug, Clone, Copy)]
pub enum AstConst {
    Int(i32),
    Long(i64),
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

impl Exp {
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
