use crate::codegen::*;

use std::fmt;

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Ax => write!(f, "%eax"),
            Self::Cx => write!(f, "%ecx"),
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
            BinaryOp::And => write!(f, "andl"),
            BinaryOp::Or => write!(f, "orl"),
            BinaryOp::Xor => write!(f, "xorl"),
            BinaryOp::Shl => write!(f, "sall"),
            BinaryOp::Shr => write!(f, "sarl"),
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

impl fmt::Display for AsmAst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Program(fun) => write!(f, "{fun}"),
        }
    }
}

impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::E => write!(f, "e"),
            Self::NE => write!(f, "ne"),
            Self::L => write!(f, "l"),
            Self::LE => write!(f, "le"),
            Self::G => write!(f, "g"),
            Self::GE => write!(f, "ge"),
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
            Self::Binary(BinaryOp::Shl, Operand::Reg(Register::Cx), dst) => {
                write!(f, "sall %cl, {dst}")
            }
            Self::Binary(BinaryOp::Shr, Operand::Reg(Register::Cx), dst) => {
                write!(f, "sarl %cl, {dst}")
            }
            Self::Binary(op, src, dst) => write!(f, "{op} {src}, {dst}"),
            Self::Cmp(src, dst) => write!(f, "cmpl {src}, {dst}"),
            Self::Jmp(label) => write!(f, "jmp .L{label}"),
            Self::JmpCC(condition, label) => write!(f, "j{condition} .L{label}"),
            Self::Label(label) => write!(f, ".L{label}:"),
            Self::SetCC(cond_code, operand) => {
                if operand.is_reg() {
                    let reg = match operand {
                        Operand::Reg(r) => r,
                        _ => unreachable!(),
                    };
                    let reg_str = match reg {
                        Register::Ax => "al",
                        Register::Dx => "dl",
                        Register::R10 => "r10b",
                        Register::R11 => "r11b",
                        Register::Cx => "cx",
                    };
                    write!(f, "set{cond_code} {reg_str}")
                } else {
                    write!(f, "set{cond_code} {operand}")
                }
            }
        }
    }
}
