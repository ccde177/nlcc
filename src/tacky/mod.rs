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
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
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
    pub fn is_shift(&self) -> bool {
        matches!(self, TBinaryOp::ShiftLeft | TBinaryOp::ShiftRight)
    }
    pub fn is_div(&self) -> bool {
        matches!(self, TBinaryOp::Divide)
    }

    pub fn is_rem(&self) -> bool {
        matches!(self, TBinaryOp::Reminder)
    }

    pub fn is_comp(&self) -> bool {
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
            AstBinaryOp::BitwiseAnd => Self::BitwiseAnd,
            AstBinaryOp::BitwiseOr => Self::BitwiseOr,
            AstBinaryOp::BitwiseXor => Self::BitwiseXor,
            AstBinaryOp::ShiftLeft => Self::ShiftLeft,
            AstBinaryOp::ShiftRight => Self::ShiftRight,
            _ => unimplemented!(),
        }
    }
}

fn emit_expression(instructions: &mut TInstructions, e: AstExp, ng: &mut NameGenerator) -> TValue {
    match e {
        AstExp::Constant(u) => TValue::Constant(u),
        AstExp::Unary(op @ (AstUnaryOp::PostfixIncrement | AstUnaryOp::PostfixDecrement), exp) => {
            let one = TValue::Constant(1);
            let op = if matches!(op, AstUnaryOp::PostfixIncrement) {
                TBinaryOp::Add
            } else {
                TBinaryOp::Substract
            };
            let original = emit_expression(instructions, *exp, ng);
            let new_var = TValue::Var(ng.get_name());
            let copy = TInstruction::Copy(original.clone(), new_var.clone());
            let modify = TInstruction::Binary(op, new_var.clone(), one, original);
            instructions.push(copy);
            instructions.push(modify);
            new_var
        }
        AstExp::Unary(op @ (AstUnaryOp::PrefixIncrement | AstUnaryOp::PrefixDecrement), exp) => {
            let one = TValue::Constant(1);
            let op = if matches!(op, AstUnaryOp::PrefixIncrement) {
                TBinaryOp::Add
            } else {
                TBinaryOp::Substract
            };
            let src = emit_expression(instructions, *exp, ng);
            let dst = TValue::Var(ng.get_name());
            let modify = TInstruction::Binary(op, src.clone(), one, dst.clone());
            let copy = TInstruction::Copy(dst.clone(), src.clone());
            instructions.push(modify);
            instructions.push(copy);
            src
        }
        AstExp::Unary(op, exp) => {
            let tacky_op = TUnaryOp::from(op);
            let src = emit_expression(instructions, *exp, ng);
            let dst = TValue::Var(ng.get_name());
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

            let v1 = emit_expression(instructions, src.as_ref().clone(), ng);
            let jz1 = TInstruction::JumpIfZero(v1.clone(), false_label.clone());
            instructions.push(jz1);

            let v2 = emit_expression(instructions, dst.as_ref().clone(), ng);
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

            let v1 = emit_expression(instructions, src.as_ref().clone(), ng);
            let jnz1 = TInstruction::JumpIfNotZero(v1.clone(), true_label.clone());
            instructions.push(jnz1);

            let v2 = emit_expression(instructions, *dst, ng);
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
            let v1 = emit_expression(instructions, *exp1, ng);
            let v2 = emit_expression(instructions, *exp2, ng);
            let dst_name = ng.get_name();
            let dst = TValue::Var(dst_name);
            let tacky_op = TBinaryOp::from(op);
            let tacky_instruction = TInstruction::Binary(tacky_op, v1, v2, dst.clone());
            instructions.push(tacky_instruction);
            dst
        }
        AstExp::Assignment(var, rhs) => {
            let name = match *var {
                AstExp::Var(name) => name,
                _ => unreachable!(),
            };
            let rhs = emit_expression(instructions, *rhs, ng);
            let var = TValue::Var(name);
            let copy = TInstruction::Copy(rhs, var.clone());
            instructions.push(copy);
            var
        }
        AstExp::Var(name) => TValue::Var(name.clone()),
        AstExp::Conditional {
            condition,
            then,
            els,
        } => {
            let c = emit_expression(instructions, *condition, ng);
            let e2 = ng.get_label();
            let jz = TInstruction::JumpIfZero(c, e2.clone());
            instructions.push(jz);
            let v1 = emit_expression(instructions, *then, ng);
            let result = TValue::Var(ng.get_name());
            let copy = TInstruction::Copy(v1, result.clone());
            instructions.push(copy);
            let end = ng.get_label();
            let jmp_end = TInstruction::Jump(end.clone());
            instructions.push(jmp_end);
            let e2_label = TInstruction::Label(e2);
            instructions.push(e2_label);
            let v2 = emit_expression(instructions, *els, ng);
            let copy = TInstruction::Copy(v2, result.clone());
            instructions.push(copy);
            let end_label = TInstruction::Label(end);
            instructions.push(end_label);
            result
        }
    }
}

fn emit_forinit(forinit: AstForInit, instructions: &mut TInstructions, ng: &mut NameGenerator) {
    match forinit {
        AstForInit::InitDecl(dec) => emit_declaration(dec, instructions, ng),
        AstForInit::InitExp(Some(exp)) => {
            let _ = emit_expression(instructions, exp, ng);
        }
        _ => (),
    }
}

fn emit_statement(
    statement: AstStatement,
    instructions: &mut TInstructions,
    ng: &mut NameGenerator,
) {
    match statement {
        AstStatement::DoWhile {
            condition,
            body,
            label,
        } => {
            let continue_label = format!("continue_{label}");
            let break_label = format!("break_{label}");
            let start_label = format!("start_{label}");

            let start = TInstruction::Label(start_label.clone());
            instructions.push(start);

            emit_statement(*body, instructions, ng);

            let cont = TInstruction::Label(continue_label.clone());
            instructions.push(cont);

            let v = emit_expression(instructions, condition, ng);
            let jnz = TInstruction::JumpIfNotZero(v, start_label);
            instructions.push(jnz);

            let brk = TInstruction::Label(break_label);
            instructions.push(brk);
        }
        AstStatement::For {
            init,
            condition,
            post,
            body,
            label,
        } => {
            let continue_label = format!("continue_{label}");
            let break_label = format!("break_{label}");
            let start_label = format!("start_{label}");

            emit_forinit(init, instructions, ng);

            let start = TInstruction::Label(start_label.clone());
            instructions.push(start);

            if let Some(exp) = condition {
                let v = emit_expression(instructions, exp, ng);
                let jz = TInstruction::JumpIfZero(v, break_label.clone());
                instructions.push(jz);
            }

            emit_statement(*body, instructions, ng);

            let cont = TInstruction::Label(continue_label);
            instructions.push(cont);

            if let Some(exp) = post {
                let _ = emit_expression(instructions, exp, ng);
            }

            let jmp = TInstruction::Jump(start_label);
            instructions.push(jmp);

            let brk = TInstruction::Label(break_label);
            instructions.push(brk);
        }
        AstStatement::While {
            condition,
            body,
            label,
        } => {
            let continue_label = format!("continue_{label}");
            let break_label = format!("break_{label}");

            let cont = TInstruction::Label(continue_label.clone());
            instructions.push(cont);

            let v = emit_expression(instructions, condition, ng);
            let jz = TInstruction::JumpIfZero(v, break_label.clone());
            instructions.push(jz);

            emit_statement(*body, instructions, ng);

            let jump = TInstruction::Jump(continue_label);
            instructions.push(jump);

            let brk = TInstruction::Label(break_label);
            instructions.push(brk);
        }
        AstStatement::Break(label) => {
            let break_label = format!("break_{label}");
            let jump = TInstruction::Jump(break_label);
            instructions.push(jump);
        }
        AstStatement::Continue(label) => {
            let continue_label = format!("continue_{label}");
            let jump = TInstruction::Jump(continue_label);
            instructions.push(jump);
        }
        AstStatement::If {
            condition,
            then,
            els,
        } => {
            let c = emit_expression(instructions, condition, ng);
            let end_or_else = ng.get_label();
            let jz = TInstruction::JumpIfZero(c, end_or_else.clone());
            instructions.push(jz);
            emit_statement(*then, instructions, ng);
            let end_or_else = TInstruction::Label(end_or_else);
            if els.is_some() {
                let els_label = end_or_else;
                let end = ng.get_label();
                let jump_end = TInstruction::Jump(end.clone());
                instructions.push(jump_end);
                instructions.push(els_label);
                emit_statement(*els.unwrap(), instructions, ng);
                let end_label = TInstruction::Label(end);
                instructions.push(end_label);
            } else {
                instructions.push(end_or_else);
            }
        }
        AstStatement::Compound(block) => {
            let AstBlock { items } = block;
            let mut block_items = emit_block_items(items, ng);
            instructions.append(&mut block_items);
        }
        AstStatement::Goto(label) => {
            let jump = TInstruction::Jump(label);
            instructions.push(jump);
        }
        AstStatement::LabeledStatement(name, statement) => {
            let label = TInstruction::Label(name);
            instructions.push(label);
            emit_statement(*statement, instructions, ng);
        }
        AstStatement::Return(e) => {
            let value = emit_expression(instructions, e, ng);
            instructions.push(TInstruction::Return(value));
        }
        AstStatement::Exp(e) => {
            emit_expression(instructions, e, ng);
        }
        AstStatement::Null => (),
    }
}

fn emit_declaration(d: AstDeclaration, instructions: &mut TInstructions, ng: &mut NameGenerator) {
    if let Some(init) = d.init {
        let rhs = emit_expression(instructions, init, ng);
        let var = TValue::Var(d.name.clone());
        let copy = TInstruction::Copy(rhs, var.clone());
        instructions.push(copy);
    }
}

fn emit_block_items(blockitems: AstBlockItems, ng: &mut NameGenerator) -> TInstructions {
    let mut instructions = TInstructions::new();
    for block in blockitems.into_iter() {
        match block {
            AstBlockItem::S(s) => emit_statement(s, &mut instructions, ng),
            AstBlockItem::D(d) => emit_declaration(d, &mut instructions, ng),
        }
    }
    instructions
}

fn emit_function(f: AstFunction) -> TFunction {
    let AstBlock { items } = f.body;
    let mut ng = NameGenerator::new();
    let mut body = emit_block_items(items, &mut ng);

    body.push(TInstruction::Return(TValue::Constant(0)));

    TFunction::FunDef(f.name, body)
}

pub fn emit_tacky(input: Ast) -> TAst {
    let Ast::FunDef(f) = input;
    let tfunction = emit_function(f);

    TAst::Program(tfunction)
}
