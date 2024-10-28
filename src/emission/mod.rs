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
            Self::R8 => write!(f, "%r8d"),
            Self::R9 => write!(f, "%r9d"),
            Self::Si => write!(f, "%esi"),
            Self::Di => write!(f, "%edi"),
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Imm(i) => write!(f, "${i}"),
            Self::Reg(r) => write!(f, "{r}"),
            Self::Stack(i) => write!(f, "{i}(%rbp)"),
            Self::Data(name) => write!(f, "{name}(%rip)"),
            Self::Pseudo(_) => unreachable!(),
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
impl fmt::Display for AsmFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.global {
            writeln!(f, "\t.globl {}", self.name)?;
        }
        writeln!(f, ".text")?;
        writeln!(f, "{}:", self.name)?;
        //Prologue:
        writeln!(f, "\tpushq %rbp")?;
        writeln!(f, "\tmovq %rsp, %rbp")?;
        for instruction in &self.body {
            writeln!(f, "\t{instruction}")?;
        }

        Ok(())
    }
}

impl fmt::Display for AsmStaticVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let AsmStaticVar { name, global, init } = self;
        let (section, init) = if *init == 0 {
            let bss = String::from(".bss");
            let zero = String::from(".zero 4");
            (bss, zero)
        } else {
            let data = String::from(".data");
            let init = format!(".long {init}");
            (data, init)
        };
        if *global {
            writeln!(f, "\t.globl {name}")?;
        };
        writeln!(f, "\t{section}")?;
        writeln!(f, "\t.align 4")?;
        writeln!(f, "{name}:")?;
        writeln!(f, "\t{init}")
    }
}

impl fmt::Display for AsmTopLevelItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Fun(fun) => write!(f, "{fun}"),
            Self::StaticVar(staticvar) => write!(f, "{staticvar}"),
        }
    }
}

impl fmt::Display for AsmAst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for asm_toplevel_item in &self.functions {
            write!(f, "{asm_toplevel_item}")?;
        }
        writeln!(f, ".section .note.GNU-stak,\"\",@progbits")
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

fn reg_to_8(reg: &Operand) -> String {
    if let Operand::Reg(reg) = reg {
        match reg {
            Register::Ax => "%rax",
            Register::Cx => "%rcx",
            Register::Dx => "%rdx",
            Register::Di => "%rdi",
            Register::Si => "%rsi",
            Register::R8 => "%r8",
            Register::R9 => "%r9",
            Register::R10 => "%r10",
            Register::R11 => "%r11",
        }
        .into()
    } else {
        reg.to_string()
    }
}

impl fmt::Display for AsmInstruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::DeallocateStack(u) => write!(f, "addq ${u}, %rsp"),
            Self::Push(op) => write!(f, "pushq {}", reg_to_8(op)),
            Self::Call(name) => write!(f, "call {name}"),
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
                    let Operand::Reg(reg) = operand else {
                        unreachable!()
                    };
                    let reg_str = match reg {
                        Register::Ax => "al",
                        Register::Dx => "dl",
                        Register::R8 => "r8b",
                        Register::R10 => "r10b",
                        Register::R11 => "r11b",
                        Register::Cx => "cl",
                        Register::Di => "dil",
                        Register::Si => "sil",
                        Register::R9 => "r9b",
                    };
                    write!(f, "set{cond_code} {reg_str}")
                } else {
                    write!(f, "set{cond_code} {operand}")
                }
            }
        }
    }
}
