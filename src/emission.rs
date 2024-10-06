use crate::codegen::*;

use std::fmt;

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
