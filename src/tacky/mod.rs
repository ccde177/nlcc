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
    Copy(Value, Value),
    Jump(Identifier),
    JumpIfZero(Value, Identifier),
    JumpIfNotZero(Value, Identifier),
    Label(Identifier),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BinaryOp {
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
}

#[derive(Clone)]
pub enum Value {
    Constant(u64),
    Var(Identifier),
}

pub enum UnaryOp {
    Complement,
    Negate,
    LogicalNot,
}

impl BinaryOp {
    pub fn is_div(&self) -> bool {
        *self == BinaryOp::Divide
    }

    pub fn is_rem(&self) -> bool {
        *self == BinaryOp::Reminder
    }

    pub fn is_comp(&self) -> bool {
        match self {
            Self::IsEqual
                | Self::IsNotEqual
                | Self::IsLessThan
                | Self::IsLessOrEqual
                | Self::IsGreaterThan
                | Self::IsGreaterOrEqual => true,
            _ => false
        }
    }
}

struct NameGenerator {
    name_count: u64,
    label_count: u64,
}

impl NameGenerator {
    fn new() -> Self {
        Self {
            name_count: 0,
            label_count: 0,
        }
    }

    fn get_name(&mut self) -> String {
        let c = self.name_count;
        self.name_count += 1;
        format!("tmp.{c}")
    }

    fn get_label(&mut self) -> String {
        let c = self.label_count;
        self.label_count += 1;
        format!("lable_{c}")
    }
}

impl From<ast::UnaryOperator> for UnaryOp {
    fn from(value: ast::UnaryOperator) -> Self {
        match value {
            ast::UnaryOperator::Complement => UnaryOp::Complement,
            ast::UnaryOperator::Negate => UnaryOp::Negate,
            ast::UnaryOperator::LogicalNot => UnaryOp::LogicalNot,
            _ => unimplemented!(),
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
            ast::BinaryOp::IsEqual => Self::IsEqual,
            ast::BinaryOp::IsNotEqual => Self::IsNotEqual,
            ast::BinaryOp::LessThan => Self::IsLessThan,
            ast::BinaryOp::LessOrEqual => Self::IsLessOrEqual,
            ast::BinaryOp::GreaterThan => Self::IsGreaterThan,
            ast::BinaryOp::GreaterOrEqual => Self::IsGreaterOrEqual,
            _ => unimplemented!(),
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
        ast::Expression::Binary(ast::BinaryOp::LogicalAnd, src, dst) => {
            let false_label = ng.get_label(); 
            let label_end = ng.get_label();
            let result = Value::Var(ng.get_name());
            let copy0 = Instruction::Copy(Value::Constant(0), result.clone());
            let copy1 = Instruction::Copy(Value::Constant(1), result.clone());
            let jumpend = Instruction::Jump(label_end.clone());
           
            let v1 = emit_instruction(instructions, src.as_ref().clone(), ng);
            let jz1 = Instruction::JumpIfZero(v1.clone(), false_label.clone());
            instructions.push(jz1);
            
            let v2 = emit_instruction(instructions, dst.as_ref().clone(), ng);
            let jz2 = Instruction::JumpIfZero(v2.clone(), false_label.clone());
            instructions.push(jz2);
            
            instructions.push(copy1);
            instructions.push(jumpend);
            instructions.push(Instruction::Label(false_label));
            instructions.push(copy0);
            instructions.push(Instruction::Label(label_end));
                        
            result
        }
        ast::Expression::Binary(ast::BinaryOp::LogicalOr, src, dst) => {
            let true_label = ng.get_label(); 
            let label_end = ng.get_label();
            let result = Value::Var(ng.get_name());
            let copy0 = Instruction::Copy(Value::Constant(0), result.clone());
            let copy1 = Instruction::Copy(Value::Constant(1), result.clone());
            let jumpend = Instruction::Jump(label_end.clone());
           
            let v1 = emit_instruction(instructions, src.as_ref().clone(), ng);
            let jnz1 = Instruction::JumpIfNotZero(v1.clone(), true_label.clone());
            instructions.push(jnz1);
            
            let v2 = emit_instruction(instructions, dst.as_ref().clone(), ng);
            let jiz2 = Instruction::JumpIfNotZero(v2.clone(), true_label.clone());
            instructions.push(jiz2);
            
            instructions.push(copy0);
            instructions.push(jumpend);
            instructions.push(Instruction::Label(true_label));
            instructions.push(copy1);
            instructions.push(Instruction::Label(label_end));
                        
            result
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
