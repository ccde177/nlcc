pub type Identifier = String;

#[derive(Debug, Clone)]
pub struct Ast {
    pub functions: Vec<FunDec>,
}

pub type AstBlockItems = Vec<AstBlockItem>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AstBlock {
    pub items: AstBlockItems,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AstBlockItem {
    S(Statement),
    D(Declaration),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Declaration {
    Var(VarDec),
    Fun(FunDec),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunDec {
    pub name: Identifier,
    pub params: Vec<Identifier>,
    pub body: Option<AstBlock>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VarDec {
    pub name: Identifier,
    pub init: Option<Exp>,
}

pub type Cases = Vec<(Option<u64>, Identifier)>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DoWhile {
    pub body: Box<Statement>,
    pub condition: Exp,
    pub label: Identifier,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct While {
    pub condition: Exp,
    pub body: Box<Statement>,
    pub label: Identifier,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct For {
    pub init: AstForInit,
    pub condition: Option<Exp>,
    pub post: Option<Exp>,
    pub body: Box<Statement>,
    pub label: Identifier,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct If {
    pub condition: Exp,
    pub then: Box<Statement>,
    pub els: Option<Box<Statement>>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Switch {
    pub ctrl_exp: Exp,
    pub body: Box<Statement>,
    pub cases: Cases,
    pub label: Identifier,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CasedStatement {
    pub exp: Exp,
    pub body: Box<Statement>,
    pub label: Identifier,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DCasedStatement {
    pub body: Box<Statement>,
    pub label: Identifier,
}

#[derive(Debug, Clone, Eq, PartialEq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstForInit {
    InitDecl(VarDec),
    InitExp(Option<Exp>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConditionalExp {
    pub condition: Box<Exp>,
    pub then: Box<Exp>,
    pub els: Box<Exp>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Exp {
    Conditional(ConditionalExp),
    Binary(AstBinaryOp, Box<Exp>, Box<Exp>),
    Unary(AstUnaryOp, Box<Exp>),
    Assignment(Box<Exp>, Box<Exp>),
    Call(Identifier, Vec<Exp>),
    Var(Identifier),
    Constant(u64),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
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

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
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

    pub fn get_const(&self) -> Option<u64> {
        match self {
            Exp::Constant(u) => Some(*u),
            _ => None,
        }
    }
}
