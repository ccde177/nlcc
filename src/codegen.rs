use crate::tacky;

use std::collections::HashMap;
use std::fmt;

#[derive(Debug)]
pub enum Program {
    Program(Function),
}

type Identifier = String;
type Instructions = Vec<Instruction>;

#[derive(Debug)]
pub struct Function {
    name: String,
    body: Instructions,
}

#[derive(Debug)]
pub enum Instruction {
    AllocateStack(u64),
    Mov(Operand, Operand),
    Unary(UnaryOp, Operand),
    Binary(BinaryOp, Operand, Operand),
    Idiv(Operand),
    Cdq,
    Ret,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Imul,
}

impl Instruction {
    fn stack_operands(&self) -> bool {
        match self {
            Self::Mov(src, dst) => src.is_stack() && dst.is_stack(),
	    Self::Binary(op, src, dst) => {
		(*op == BinaryOp::Add || *op == BinaryOp::Sub)
		    && src.is_stack() && dst.is_stack()
	    }
            _ => false,
        }
    }

    fn is_mul_sndmem(&self) -> bool {
	match self {
	    Instruction::Binary(op, _, operand) => {
		*op == BinaryOp::Imul && operand.is_stack()
	    }
	    _ => false
	}
    }

    fn is_idiv_constant(&self) -> bool {
	match self {
	    Instruction::Idiv(c) => c.is_imm(),
	    _ => false,
	}
    }
}

#[derive(Copy, Clone, Debug)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Clone, Debug)]
pub enum Operand {
    Imm(u64),
    Reg(Register),
    Pseudo(Identifier),
    Stack(u64),
}

impl Operand {
    fn is_stack(&self) -> bool {
        match self {
            Self::Stack(_) => true,
            _ => false,
        }
    }
    fn is_imm(&self) -> bool {
	match self {
	    Self::Imm(_) => true,
	    _ => false,
	}
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Register {
    Ax,
    Dx,
    R10,
    R11,
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Ax => write!(f, "%eax"),
	    Self::Dx => write!(f, "%edx"),
            Self::R10 => write!(f, "%r10d"),
	    Self::R11 => write!(f, "%r11d"),
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Imm(i) => write!(f, "${i}"),
            Self::Reg(r) => write!(f, "{r}"),
            Self::Stack(i) => write!(f, "-{i}(%rbp)"),
            _ => unimplemented!(),
        }
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Neg => write!(f, "negl"),
            Self::Not => write!(f, "notl"),
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
	match self {
	    BinaryOp::Add => write!(f, "addl"),
	    BinaryOp::Sub => write!(f, "subl"),
	    BinaryOp::Imul => write!(f, "imull"),
	}
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::AllocateStack(i) => write!(f, "subq ${i}, %rsp"),
            Self::Unary(op, operand) => write!(f, "{op} {operand}"),
            Self::Mov(o1, o2) => write!(f, "movl {o1}, {o2}"),
            Self::Ret => write!(f, "movq %rbp, %rsp\n\tpopq %rbp\n\tret"),
	    Self::Idiv(op) => write!(f, "idivl {op}"),
	    Self::Cdq => write!(f, "cdq"),
	    Self::Binary(op, src, dst) => write!(f, "{op} {src}, {dst}"),
            _ => unimplemented!(),
        }
    }
}

#[cfg(target_os = "linux")]
impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "\t.globl {}", self.name)?;
        writeln!(f, "{}:", self.name)?;
        //Prologue:
        writeln!(f, "\tpushq %rbp")?;
        writeln!(f, "\tmovq %rsp, %rbp")?;
        for instruction in self.body.iter() {
            writeln!(f, "\t{instruction}")?;
        }
        writeln!(f, ".section .note.GNU-stak,\"\",@progbits")?;
        Ok(())
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Program(fun) => write!(f, "{fun}"),
        }
    }
}

fn val_to_operand(val: &tacky::Value) -> Operand {
    match val {
        tacky::Value::Constant(u) => Operand::Imm(*u),
        tacky::Value::Var(identifier) => Operand::Pseudo(identifier.clone()),
    }
}

fn unary_from_tacky(op: &tacky::UnaryOp) -> UnaryOp {
    match op {
        tacky::UnaryOp::Complement => UnaryOp::Not,
        tacky::UnaryOp::Negate => UnaryOp::Neg,
    }
}

type StackAllocMap = HashMap<Identifier, u64>;

struct StackAllocator {
    offset: u64,
    map: StackAllocMap,
}

impl StackAllocator {
    fn new() -> Self {
        Self {
            offset: 0,
            map: StackAllocMap::new(),
        }
    }

    fn allocate_if_pseudo(&mut self, operand: Operand) -> Operand {
        match operand {
            Operand::Pseudo(name) => {
                let offset = self.allocate(&name);
                Operand::Stack(offset)
            }
            _ => operand,
        }
    }

    fn allocate(&mut self, name: &Identifier) -> u64 {
        if self.map.contains_key(name) {
            return *self.map.get(name).unwrap();
        }
        self.offset += 4;
        self.map.insert(name.clone(), self.offset);
	self.offset
    }

    fn get_prologue(&self) -> Instruction {
        Instruction::AllocateStack(self.offset)
    }
}

impl From<tacky::BinaryOp> for BinaryOp {
    fn from(value: tacky::BinaryOp) -> Self {
	match value {
	    tacky::BinaryOp::Add => BinaryOp::Add,
	    tacky::BinaryOp::Multiply => BinaryOp::Imul,
	    tacky::BinaryOp::Substract => BinaryOp::Sub,
	    _ => unimplemented!()
	}
    }
}

fn tbinary_to_asm(instructions: &mut Instructions, tinstr: tacky::Instruction) {
    if let tacky::Instruction::Binary(op, val1, val2, val3) = tinstr {
	let src1 = val_to_operand(&val1);
	let src2 = val_to_operand(&val2);
	let dst = val_to_operand(&val3);

	let is_div = op.is_div();
	let is_rem = op.is_rem();
	
	if is_div || is_rem {
	    let ax = Operand::Reg(Register::Ax);
	    let dx = Operand::Reg(Register::Dx);
	    let mov1 = Instruction::Mov(src1, ax.clone());
	    let cdq = Instruction::Cdq;
	    let idiv = Instruction::Idiv(src2);
	    let last = if is_rem { dx } else { ax };
	    let mov2 = Instruction::Mov(last, dst);

	    instructions.push(mov1);
	    instructions.push(cdq);
	    instructions.push(idiv);
	    instructions.push(mov2);
	    
	} else {
	    let op = BinaryOp::from(op);
	    let mov = Instruction::Mov(src1, dst.clone());
	    let operation = Instruction::Binary(op, src2, dst);
	    
	    instructions.push(mov);
	    instructions.push(operation);
	}
    }
}

fn tacky_to_asm(body: tacky::Instructions) -> Instructions {
    let mut instructions = Instructions::new();
    for inst in body.into_iter() {
        match inst {
            tacky::Instruction::Return(val) => {
                let src = val_to_operand(&val);
                let dst = Operand::Reg(Register::Ax);
                let mov = Instruction::Mov(src, dst);
                let ret = Instruction::Ret;
                instructions.push(mov);
                instructions.push(ret);
            }
            tacky::Instruction::Unary(op, val1, val2) => {
                let src = val_to_operand(&val1);
                let dst = val_to_operand(&val2);
                let op = unary_from_tacky(&op);
                let mov = Instruction::Mov(src, dst.clone());
                let unary = Instruction::Unary(op, dst);
                instructions.push(mov);
                instructions.push(unary);
            }
	    tacky::Instruction::Binary(_,_,_,_) => {
		tbinary_to_asm(&mut instructions, inst);
	    }
            _ => unimplemented!(),
        }
    }
    instructions
}

fn allocate_stack(instructions: &mut Instructions) {
    let mut stack_allocator = StackAllocator::new();
    for inst in instructions.iter_mut() {
        match inst {
            Instruction::Unary(op, operand) => {
                let operand = stack_allocator.allocate_if_pseudo(operand.clone());
                *inst = Instruction::Unary(*op, operand);
            }
            Instruction::Mov(src, dst) => {
                let src = stack_allocator.allocate_if_pseudo(src.clone());
                let dst = stack_allocator.allocate_if_pseudo(dst.clone());
                *inst = Instruction::Mov(src, dst);
            }
	    Instruction::Binary(op, operand1, operand2) => {
		let operand1 = stack_allocator.allocate_if_pseudo(operand1.clone());
		let operand2 = stack_allocator.allocate_if_pseudo(operand2.clone());
		*inst = Instruction::Binary(*op, operand1, operand2);
	    }
	    Instruction::Idiv(operand) => {
		let operand = stack_allocator.allocate_if_pseudo(operand.clone());
		*inst = Instruction::Idiv(operand);
	    }
            _ => (),
        }
    }
    let prologue = stack_allocator.get_prologue();
    instructions.insert(0, prologue);
}

fn fix_imul(instructions: &mut Instructions) {
    let indexes: Vec<_> = instructions
        .iter()
        .enumerate()
        .filter(|(_, i)| i.is_mul_sndmem())
        .map(|(i, _)| i)
        .collect();
    let mut count = 0;
    for i in indexes {
	let instruction = instructions.remove(i + count);
	let (op, src, dst) = match instruction {
	    Instruction::Binary(op, src, dst) => (op, src, dst),
	    _ => unreachable!()
	};
	let temp_reg = Register::R11;
	let mov1 = Instruction::Mov(dst.clone(), Operand::Reg(temp_reg));
	let imul = Instruction::Binary(op, src, Operand::Reg(temp_reg));
	let mov2 = Instruction::Mov(Operand::Reg(temp_reg), dst);
	
	instructions.insert(i + count, mov1);
	count += 1;
	instructions.insert(i + count, imul);
	count += 1;
	instructions.insert(i + count, mov2);
    }
}

fn fix_idiv(instructions: &mut Instructions) {
    let indexes:Vec<_> = instructions
	.iter()
	.enumerate()
        .filter(|(_, i)| i.is_idiv_constant())
        .map(|(i, _)| i)
        .collect();

    let mut count = 0;
    for i in indexes {
	let instruction = instructions.remove(i + count);
	let operand = match instruction {
	    Instruction::Idiv(operand) => operand,
	    _ => unreachable!(),
	};
	let temp_reg = Register::R10;
	let mov1 = Instruction::Mov(operand, Operand::Reg(temp_reg));
	let idiv = Instruction::Idiv(Operand::Reg(temp_reg));
	instructions.insert(i + count, mov1);
	count += 1;
	instructions.insert(i + count, idiv);
    }
}

fn replace_two_stack_operands(instructions: &mut Instructions) {
    //* Fix instructions where src and dst are Stack(n) *//
    let indexes: Vec<_> = instructions
        .iter()
        .enumerate()
        .filter(|(_, i)| i.stack_operands())
        .map(|(i, _)| i)
        .collect();
    let mut count = 0;
    for i in indexes {
        let instruction = instructions.remove(i + count);
        let (src, dst) = match instruction {
            Instruction::Mov(ref o1, ref o2) => (o1, o2),
	    Instruction::Binary(_, ref o1, ref o2) => (o1, o2),
            _ => unreachable!(),
        };
        let temp_reg = Register::R10;
        let mov1 = Instruction::Mov(src.clone(), Operand::Reg(temp_reg));
	let snd = if let Instruction::Binary(op, _, _) = instruction {
	    Instruction::Binary(op, Operand::Reg(temp_reg), dst.clone())
	} else {
	    Instruction::Mov(Operand::Reg(temp_reg), dst.clone())
	};

        instructions.insert(i + count, mov1);
        count += 1;
        instructions.insert(i + count, snd);
    }
}

fn gen_body(body: tacky::Instructions) -> Instructions {
    let mut instructions = tacky_to_asm(body);
    allocate_stack(&mut instructions);
    replace_two_stack_operands(&mut instructions);
    fix_idiv(&mut instructions);
    fix_imul(&mut instructions);
    instructions
}

fn gen_fundef(f: tacky::FunDef) -> Function {
    match f {
        tacky::FunDef::Function(name, body) => Function {
            name,
            body: gen_body(body),
        },
    }
}

pub fn codegen(ast: tacky::TackyAst) -> Program {
    match ast {
        tacky::TackyAst::Program(f) => Program::Program(gen_fundef(f)),
    }
}
