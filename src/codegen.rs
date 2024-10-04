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
    Ret
}

impl Instruction {
    fn stack_operands(&self) -> bool {
	match self {
	    Self::Mov(src, dst) => {
		src.is_stack() && dst.is_stack()
	    }
	    _ => false
	}
    }
}

#[derive(Copy, Clone, Debug)]
pub enum UnaryOp {
    Neg,
    Not
}

#[derive(Clone, Debug)]
pub enum Operand {
    Imm(u64),
    Register(Reg),
    Pseudo(Identifier),
    Stack(u64)
}

impl Operand {
    fn is_stack(&self) -> bool {
	match self {
	    Self::Stack(_) => true,
	    _ => false
	}
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Reg {
    Ax,
    R10
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
	match self {
	    Self::Ax => write!(f, "%eax"),
	    Self::R10 => write!(f, "%r10d"),
	}
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Imm(i) => write!(f, "${i}"),
            Self::Register(r) => write!(f, "{r}"),
	    Self::Stack(i) => write!(f, "{i}(%rbp)"),
	    _ => unreachable!()
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

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
	    Self::AllocateStack(i) => write!(f, "subq ${i}, %rsp"),
	    Self::Unary(op, operand) => write!(f, "{op} {operand}"),
            Self::Mov(o1, o2) => write!(f, "movl {o1}, {o2}"),
            Self::Ret => write!(f, "movq %rbp, %rsp\n\tpopq %rbp\n\tret"),
	    
	    _ => unimplemented!()
        }
    }
}

#[cfg(target_os = "macos")]
impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "\t.globl _{}", self.name)?;
        writeln!(f, "_{}:", self.name)?;
        for instruction in self.body.iter() {
            writeln!(f, "\t{instruction}")?;
        }
        Ok(())
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
        tacky::Value::Var(identifier) => Operand::Pseudo(identifier.clone())
    }
}

fn unary_from_tacky(op: &tacky::UnaryOp) -> UnaryOp {
    match op {
        tacky::UnaryOp::Complement => UnaryOp::Not,
        tacky::UnaryOp::Negate => UnaryOp::Neg
    }
}

type StackAllocMap = HashMap<Identifier, u64>;

struct StackAllocator {
    offset: u64,
    map: StackAllocMap
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
	    _ => operand
	}
    }
    
    fn allocate(&mut self, name: &Identifier) -> u64 {
	if self.map.contains_key(name) {
	    return *self.map.get(name).unwrap();
	}
	self.offset += 4;
	self.map.insert(name.clone(), self.offset - 4);
	self.offset - 4
    }

    fn get_prologue(&self) -> Instruction {
	Instruction::AllocateStack(self.offset)
    }
}

fn gen_body(body: tacky::Instructions) -> Instructions {
    let mut instructions = Instructions::new();
    for inst in body.iter() {
        match inst {
            tacky::Instruction::Return(val) => {
                let src = val_to_operand(val);
                let dst = Operand::Register(Reg::Ax);
                let mov = Instruction::Mov(src, dst);
                let ret = Instruction::Ret;
                instructions.push(mov);
                instructions.push(ret);
                
            }
            tacky::Instruction::Unary(op, val1, val2) => {
                let src = val_to_operand(val1);
                let dst = val_to_operand(val2);
                let op = unary_from_tacky(op);
                let mov = Instruction::Mov(src, dst.clone());
                let unary = Instruction::Unary(op, dst);
                instructions.push(mov);
                instructions.push(unary);
            }
        }
    }       
    
    //* STACK ALLOCATION *//
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
	    _ => ()
	}
    }

    //* Fix instructions where src and dst are Stack(n) *//
    let indexes: Vec<_> = instructions
	.iter()
	.enumerate()
	.filter(|(_, i)| i.stack_operands())
	.map(|(i,_)| i).collect();
    let mut count = 0;
    for i in indexes {
	let instruction = instructions.remove(i + count);
	let (src, dst) = match instruction {
	    Instruction::Mov(o1, o2) => (o1, o2),
	    _ => unreachable!()
	};
	let temp_reg = Reg::R10;
	let mov1 = Instruction::Mov(src, Operand::Register(temp_reg));
	let mov2 = Instruction::Mov(Operand::Register(temp_reg), dst);
	
	instructions.insert(i + count, mov1);
	count += 1;
	instructions.insert(i + count, mov2);
    }
    let prologue = stack_allocator.get_prologue();
    instructions.insert(0, prologue);
    instructions
}

fn gen_fundef(f: tacky::FunDef) -> Function {
    match f {
        tacky::FunDef::Function(name, body) => Function {
            name,
            body: gen_body(body),
        }
    }
}

pub fn codegen(ast: tacky::TackyAst) -> Program {
    match ast {
        tacky::TackyAst::Program(f) => Program::Program(gen_fundef(f)),
    }
}
