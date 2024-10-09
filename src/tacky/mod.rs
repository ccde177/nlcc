#[cfg(test)]
mod tacky_tests;

use crate::parser::*;

pub type Identifier = String;
pub type TInstructions = Vec<TInstruction>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TAst {
    Program(TFunction),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TFunction {
    FunDef(Identifier, TInstructions),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TInstruction {
    Return(TValue),
    Unary(TUnaryOp, TValue, TValue),
    Binary(TBinaryOp, TValue, TValue, TValue),
    Copy(TValue, TValue),
    Jump(Identifier),
    JumpIfZero(TValue, Identifier),
    JumpIfNotZero(TValue, Identifier),
    Label(Identifier),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
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
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TValue {
    Constant(u64),
    Var(Identifier),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TUnaryOp {
    Complement,
    Negate,
    LogicalNot,
}

impl TBinaryOp {
    pub fn is_div(&self) -> bool {
        *self == TBinaryOp::Divide
    }

    pub fn is_rem(&self) -> bool {
        *self == TBinaryOp::Reminder
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
        format!("label_{c}")
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
            _ => unimplemented!(),
        }
    }
}

fn emit_instruction(
    instructions: &mut TInstructions,
    e: AstExp,
    ng: &mut NameGenerator,
) -> TValue {
    match e {
        AstExp::Constant(u) => TValue::Constant(u),
        AstExp::Unary(op, exp) => {
            let tacky_op = TUnaryOp::from(op);
            let src = emit_instruction(instructions, exp.as_ref().clone(), ng);
            let dst_name = ng.get_name();
            let dst = TValue::Var(dst_name);
            let tacky_instruction = TInstruction::Unary(tacky_op, src, dst.clone());
            instructions.push(tacky_instruction);
            dst
        }
        AstExp::Binary(AstBinaryOp::LogicalAnd, src, dst) => {
            let false_label = ng.get_label(); 
            let label_end = ng.get_label();
            let result = TValue::Var(ng.get_name());
            let copy0 = TInstruction::Copy(TValue::Constant(0), result.clone());
            let copy1 = TInstruction::Copy(TValue::Constant(1), result.clone());
            let jumpend = TInstruction::Jump(label_end.clone());
           
            let v1 = emit_instruction(instructions, src.as_ref().clone(), ng);
            let jz1 = TInstruction::JumpIfZero(v1.clone(), false_label.clone());
            instructions.push(jz1);
            
            let v2 = emit_instruction(instructions, dst.as_ref().clone(), ng);
            let jz2 = TInstruction::JumpIfZero(v2.clone(), false_label.clone());
            instructions.push(jz2);
            
            instructions.push(copy1);
            instructions.push(jumpend);
            instructions.push(TInstruction::Label(false_label));
            instructions.push(copy0);
            instructions.push(TInstruction::Label(label_end));
                        
            result
        }
        AstExp::Binary(AstBinaryOp::LogicalOr, src, dst) => {
            let true_label = ng.get_label(); 
            let label_end = ng.get_label();
            let result = TValue::Var(ng.get_name());
            let copy0 = TInstruction::Copy(TValue::Constant(0), result.clone());
            let copy1 = TInstruction::Copy(TValue::Constant(1), result.clone());
            let jumpend = TInstruction::Jump(label_end.clone());
           
            let v1 = emit_instruction(instructions, src.as_ref().clone(), ng);
            let jnz1 = TInstruction::JumpIfNotZero(v1.clone(), true_label.clone());
            instructions.push(jnz1);
            
            let v2 = emit_instruction(instructions, dst.as_ref().clone(), ng);
            let jiz2 = TInstruction::JumpIfNotZero(v2.clone(), true_label.clone());
            instructions.push(jiz2);
            
            instructions.push(copy0);
            instructions.push(jumpend);
            instructions.push(TInstruction::Label(true_label));
            instructions.push(copy1);
            instructions.push(TInstruction::Label(label_end));
                        
            result
        }
        AstExp::Binary(op, exp1, exp2) => {
            let v1 = emit_instruction(instructions, exp1.as_ref().clone(), ng);
            let v2 = emit_instruction(instructions, exp2.as_ref().clone(), ng);
            let dst_name = ng.get_name();
            let dst = TValue::Var(dst_name);
            let tacky_op = TBinaryOp::from(op);
            let tacky_instruction = TInstruction::Binary(tacky_op, v1, v2, dst.clone());
            instructions.push(tacky_instruction);
            dst
        }
    }
}

fn emit_statement(statement: AstStatement) -> TInstructions {
    let mut instructions = TInstructions::new();
    let mut ng = NameGenerator::new();
    match statement {
        AstStatement::Return(e) => {
            let value = emit_instruction(&mut instructions, e, &mut ng);
            instructions.push(TInstruction::Return(value));
            instructions
        }
    }
}

fn emit_function(f: AstFunction) -> TFunction {
    match f {
        AstFunction { name, body } => TFunction::FunDef(name.clone(), emit_statement(body)),
    }
}

pub fn emit_tacky(input: Ast) -> TAst {
    match input {
        Ast::FunDef(f) => TAst::Program(emit_function(f)),
    }
}