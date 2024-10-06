use crate::tacky;

use std::collections::HashMap;

#[derive(Debug)]
pub enum Program {
    Program(Function),
}

type Identifier = String;
type Instructions = Vec<Instruction>;

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub body: Instructions,
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
    fn mem_operands(&self) -> bool {
        match self {
            Self::Mov(src, dst) => src.is_mem() && dst.is_mem(),
	    Self::Binary(op, src, dst) => {
		(*op == BinaryOp::Add || *op == BinaryOp::Sub)
		    && src.is_mem() && dst.is_mem()
	    }
            _ => false,
        }
    }

    fn is_mul_sndmem(&self) -> bool {
	match self {
	    Instruction::Binary(op, _, operand) => {
		*op == BinaryOp::Imul && operand.is_mem()
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
    fn is_mem(&self) -> bool {
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
    let mut sa = StackAllocator::new();
    for inst in instructions.iter_mut() {
        match inst {
            Instruction::Unary(op, operand) => {
                let operand = sa.allocate_if_pseudo(operand.clone());
                *inst = Instruction::Unary(*op, operand);
            }
            Instruction::Mov(src, dst) => {
                let src = sa.allocate_if_pseudo(src.clone());
                let dst = sa.allocate_if_pseudo(dst.clone());
                *inst = Instruction::Mov(src, dst);
            }
	    Instruction::Binary(op, operand1, operand2) => {
		let operand1 = sa.allocate_if_pseudo(operand1.clone());
		let operand2 = sa.allocate_if_pseudo(operand2.clone());
		*inst = Instruction::Binary(*op, operand1, operand2);
	    }
	    Instruction::Idiv(operand) => {
		let operand = sa.allocate_if_pseudo(operand.clone());
		*inst = Instruction::Idiv(operand);
	    }
            _ => (),
        }
    }
    let prologue = sa.get_prologue();
    instructions.insert(0, prologue);
}

fn fix_imul(instruction: Instruction) -> Instructions {
    let mut result = Instructions::new();
    if instruction.is_mul_sndmem() {
       	let (op, src, dst) = match instruction {
	    Instruction::Binary(op, src, dst) => (op, src, dst),
	    _ => unreachable!()
	};
	let temp_reg = Register::R11;
	let mov1 = Instruction::Mov(dst.clone(), Operand::Reg(temp_reg));
	let imul = Instruction::Binary(op, src, Operand::Reg(temp_reg));
	let mov2 = Instruction::Mov(Operand::Reg(temp_reg), dst);
        result.push(mov1);
        result.push(imul);
        result.push(mov2);
    }
    result
}

fn fix_idiv(instruction: Instruction) -> Instructions {
    let mut result = Instructions::new();
    if instruction.is_idiv_constant() {
       	let operand = match instruction {
	    Instruction::Idiv(operand) => operand,
	    _ => unreachable!(),
	};
	let temp_reg = Register::R10;
	let mov1 = Instruction::Mov(operand, Operand::Reg(temp_reg));
	let idiv = Instruction::Idiv(Operand::Reg(temp_reg));
        result.push(mov1);
        result.push(idiv);
    }
    result
}

fn fix_two_memoperands(instruction: Instruction) -> Instructions {
    let mut result = Instructions::new();
    if instruction.mem_operands() {
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
        result.push(mov1);
        result.push(snd);
    }
    result
}

fn fix_instructions(instructions: &mut Instructions) {
    let indexes: Vec<_> = instructions
        .iter()
        .enumerate()
        .filter(|(_, i)| i.mem_operands() || i.is_mul_sndmem() || i.is_idiv_constant())
        .map(|(i, _)| i)
        .collect();

    let mut count = 0;
    for i in indexes {
        let instr = instructions.remove(i + count);
        
        let fixed = if instr.mem_operands() {
            fix_two_memoperands(instr)
        } else if instr.is_mul_sndmem() {
            fix_imul(instr)
        } else if instr.is_idiv_constant() {
            fix_idiv(instr)
        } else {
            unreachable!()
        };
        
        for instr in fixed.into_iter() {
            instructions.insert(i + count, instr);
            count += 1;
        }
        count -= 1;
    }
}

fn gen_body(body: tacky::Instructions) -> Instructions {
    let mut instructions = tacky_to_asm(body);
    allocate_stack(&mut instructions);
    fix_instructions(&mut instructions);
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
