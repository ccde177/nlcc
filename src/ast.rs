pub type Identifier = String;

#[derive(Debug, Clone)]
pub enum Ast {
    FunDef(AstFunction),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AstFunction {
    pub name: Identifier,
    pub body: AstBlock,
}

pub type AstBlockItems = Vec<AstBlockItem>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AstBlock {
    pub items: AstBlockItems,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AstBlockItem {
    S(AstStatement),
    D(AstDeclaration),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AstDeclaration {
    pub name: Identifier,
    pub init: Option<AstExp>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AstStatement {
    While {
        condition: AstExp,
        body: Box<AstStatement>,
        label: Identifier,
    },
    DoWhile {
        condition: AstExp,
        body: Box<AstStatement>,
        label: Identifier,
    },
    For {
        init: AstForInit,
        condition: Option<AstExp>,
        post: Option<AstExp>,
        body: Box<AstStatement>,
        label: Identifier,
    },
    If {
        condition: AstExp,
        then: Box<AstStatement>,
        els: Option<Box<AstStatement>>,
    },
    Switch {
        ctrl_exp: AstExp,
        body: Box<AstStatement>,
        cases: Vec<(Option<u64>, Identifier)>,
        label: Identifier,
    },
    Case {
        exp: AstExp,
        statement: Box<AstStatement>,
        label: Identifier,
    },
    DefaultCase {
        statement: Box<AstStatement>,
        label: Identifier,
    },
    LabeledStatement(Identifier, Box<AstStatement>),
    Continue(Identifier),
    Compound(AstBlock),
    Break(Identifier),
    Goto(Identifier),
    Return(AstExp),
    Exp(AstExp),
    Null,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstForInit {
    InitDecl(AstDeclaration),
    InitExp(Option<AstExp>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstExp {
    Conditional {
        condition: Box<AstExp>,
        then: Box<AstExp>,
        els: Box<AstExp>,
    },
    Binary(AstBinaryOp, Box<AstExp>, Box<AstExp>),
    Unary(AstUnaryOp, Box<AstExp>),
    Assignment(Box<AstExp>, Box<AstExp>),
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AstUnaryOp {
    Complement,
    Negate,
    LogicalNot,
    PostfixDecrement,
    PrefixDecrement,
    PostfixIncrement,
    PrefixIncrement,
}

impl AstExp {
    pub fn is_var(&self) -> bool {
        matches!(self, Self::Var(_))
    }

    pub fn is_const(&self) -> bool {
        matches!(self, AstExp::Constant(_))
    }

    pub fn get_const(&self) -> Option<u64> {
        match self {
            AstExp::Constant(u) => Some(*u),
            _ => None,
        }
    }
}
