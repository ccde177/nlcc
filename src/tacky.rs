use crate::parser as ast;
type Identifier = String;
pub type Instructions = Vec<Instruction>;

pub enum TackyAst {
    Program(FunDef),
}

pub enum FunDef {
    Function(Identifier, Instructions),
}

pub enum Instruction {
    Return(Value),
    Unary(UnaryOp, Value, Value),
    Binary(BinaryOp, Value, Value, Value),
}

pub enum BinaryOp {
    Add,
    Substract,
    Multiply,
    Divide,
    Reminder,
}

#[derive(Clone)]
pub enum Value {
    Constant(u64),
    Var(Identifier),
}

pub enum UnaryOp {
    Complement,
    Negate,
}

struct NameGenerator {
    count: u64,
}

impl NameGenerator {
    fn new() -> Self {
        Self { count: 0 }
    }

    fn get_name(&mut self) -> String {
        let c = self.count;
        self.count += 1;
        format!("tmp.{c}")
    }
}

impl From<ast::UnaryOperator> for UnaryOp {
    fn from(value: ast::UnaryOperator) -> Self {
        match value {
            ast::UnaryOperator::Complement => UnaryOp::Complement,
            ast::UnaryOperator::Negate => UnaryOp::Negate,
        }
    }
}

impl From<ast::BinaryOp> for BinaryOp {
    fn from(value: ast::BinaryOp) -> Self {
	match value {
	    ast::BinaryOp::Add => Self::Add,
	    ast::BinaryOp::Substract => Self::Substract,
	    ast::BinaryOp::Mod => Self::Reminder,
	    ast::BinaryOp::Multiply => Self::Multiply,
	    ast::BinaryOp::Div => Self::Divide,
	}
    }
}

fn emit_instruction(
    instructions: &mut Instructions,
    e: ast::Expression,
    ng: &mut NameGenerator,
) -> Value {
    match e {
        ast::Expression::Constant(u) => Value::Constant(u),
        ast::Expression::Unary(op, exp) => {
            let tacky_op = UnaryOp::from(op);
            let src = emit_instruction(instructions, exp.as_ref().clone(), ng);
            let dst_name = ng.get_name();
            let dst = Value::Var(dst_name);
            let tacky_instruction = Instruction::Unary(tacky_op, src, dst.clone());
            instructions.push(tacky_instruction);
            dst
        }
        ast::Expression::Binary(op, exp1, exp2) => {
	    let v1 = emit_instruction(instructions, exp1.as_ref().clone(), ng);
	    let v2 = emit_instruction(instructions, exp2.as_ref().clone(), ng);
	    let dst_name = ng.get_name();
	    let dst = Value::Var(dst_name);
	    let tacky_op = BinaryOp::from(op);
	    let tacky_instruction = Instruction::Binary(tacky_op, v1, v2, dst.clone());
	    instructions.push(tacky_instruction);
	    dst
	}
    }
}

fn emit_statement(statement: ast::Statement) -> Instructions {
    let mut instructions = Instructions::new();
    let mut ng = NameGenerator::new();
    match statement {
        ast::Statement::Return(e) => {
            let value = emit_instruction(&mut instructions, e, &mut ng);
            instructions.push(Instruction::Return(value));
            instructions
        }
    }
}

fn emit_function(f: ast::Function) -> FunDef {
    match f {
        ast::Function { name, body } => FunDef::Function(name.clone(), emit_statement(body)),
    }
}

pub fn emit_tacky(input: ast::Program) -> TackyAst {
    match input {
        ast::Program::FunDef(f) => TackyAst::Program(emit_function(f)),
    }
}
