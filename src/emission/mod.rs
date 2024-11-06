use crate::codegen::*;
use crate::semantic_analysis::StaticInit;

use std::fmt;

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Neg => write!(f, "neg"),
            Self::Not => write!(f, "not"),
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOp::Shr => write!(f, "shr"),
            BinaryOp::Add => write!(f, "add"),
            BinaryOp::Sub => write!(f, "sub"),
            BinaryOp::Imul => write!(f, "imul"),
            BinaryOp::And => write!(f, "and"),
            BinaryOp::Or => write!(f, "or"),
            BinaryOp::Xor => write!(f, "xor"),
            BinaryOp::Sal => write!(f, "sal"),
            BinaryOp::Sar => write!(f, "sar"),
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

impl fmt::Display for StaticInit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{i}"),
            Self::Long(i) => write!(f, "{i}"),
            Self::UInt(u) => write!(f, "{u}"),
            Self::ULong(u) => write!(f, "{u}"),
        }
    }
}

impl fmt::Display for AsmStaticVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let AsmStaticVar {
            name,
            global,
            init,
            alignment,
        } = self;
        let (section, init) = if init.is_zero() {
            let bss = String::from(".bss");
            let zero = format!(".zero {alignment}");
            (bss, zero)
        } else {
            let data = String::from(".data");
            let prefix = if init.is_long() { ".quad" } else { ".long" };
            let init = format!("{prefix} {init}");
            (data, init)
        };
        if *global {
            writeln!(f, "\t.globl {name}")?;
        };
        writeln!(f, "\t{section}")?;
        writeln!(f, "\t.align {alignment}")?;
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
        for asm_toplevel_item in &self.asm_toplevel_items {
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
            Self::A => write!(f, "a"),
            Self::AE => write!(f, "ae"),
            Self::B => write!(f, "b"),
            Self::BE => write!(f, "be"),
        }
    }
}

impl fmt::Display for AsmType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Longword => write!(f, "l"),
            Self::Quadword => write!(f, "q"),
        }
    }
}

fn display_quadword_reg(r: Register) -> String {
    match r {
        Register::Ax => "%rax",
        Register::Cx => "%rcx",
        Register::Dx => "%rdx",
        Register::Di => "%rdi",
        Register::Si => "%rsi",
        Register::R8 => "%r8",
        Register::R9 => "%r9",
        Register::R10 => "%r10",
        Register::R11 => "%r11",
        Register::Sp => "%rsp",
    }
    .into()
}
fn display_longword_reg(r: Register) -> String {
    match r {
        Register::Ax => "%eax",
        Register::Cx => "%ecx",
        Register::Dx => "%edx",
        Register::R10 => "%r10d",
        Register::R11 => "%r11d",
        Register::R8 => "%r8d",
        Register::R9 => "%r9d",
        Register::Si => "%esi",
        Register::Di => "%edi",
        Register::Sp => "%rsp",
    }
    .into()
}

fn display_reg(r: Register, t: AsmType) -> String {
    match t {
        AsmType::Longword => display_longword_reg(r),
        AsmType::Quadword => display_quadword_reg(r),
    }
}

fn display_operand(op: &Operand, t: AsmType) -> String {
    match op {
        Operand::Reg(r) => display_reg(*r, t),
        Operand::Imm(i) => format!("${i}"),
        Operand::Stack(i) => format!("{i}(%rbp)"),
        Operand::Data(name) => format!("{name}(%rip)"),
        Operand::Pseudo(name) => panic!("pseudo operand {name} after codegen"),
    }
}

impl fmt::Display for AsmInstruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Push(op) => write!(f, "pushq {}", display_operand(op, AsmType::Quadword)),
            Self::Call(name) => write!(f, "call {name}"),
            Self::Unary(t, op, operand) => {
                let operand_str = display_operand(operand, *t);
                write!(f, "{op}{t} {operand_str}")
            }
            Self::Mov(t, o1, o2) => {
                let o1_str = display_operand(o1, *t);
                let o2_str = display_operand(o2, *t);
                write!(f, "mov{t} {o1_str}, {o2_str}")
            }
            Self::Ret => write!(f, "movq %rbp, %rsp\n\tpopq %rbp\n\tret"),
            Self::Idiv(t, op) => {
                let op_str = display_operand(op, *t);
                write!(f, "idiv{t} {op_str}")
            }
            Self::Div(t, op) => {
                let op_str = display_operand(op, *t);
                write!(f, "div{t} {op_str}")
            }
            Self::Cdq(t) => {
                if matches!(t, AsmType::Longword) {
                    write!(f, "cdq")
                } else {
                    write!(f, "cqo")
                }
            }
            Self::Binary(t, BinaryOp::Sal, Operand::Reg(Register::Cx), dst) => {
                let dst_str = display_operand(dst, *t);
                write!(f, "sal{t} %cl, {dst_str}")
            }
            Self::Binary(t, BinaryOp::Sar, Operand::Reg(Register::Cx), dst) => {
                let dst_str = display_operand(dst, *t);
                write!(f, "sar{t} %cl, {dst_str}")
            }
            Self::Binary(t, op, src, dst) => {
                let src_str = display_operand(src, *t);
                let dst_str = display_operand(dst, *t);
                write!(f, "{op}{t} {src_str}, {dst_str}")
            }
            Self::Cmp(t, src, dst) => {
                let src_str = display_operand(src, *t);
                let dst_str = display_operand(dst, *t);
                write!(f, "cmp{t} {src_str}, {dst_str}")
            }
            Self::Jmp(label) => write!(f, "jmp .L{label}"),
            Self::JmpCC(condition, label) => write!(f, "j{condition} .L{label}"),
            Self::Label(label) => write!(f, ".L{label}:"),
            Self::Movsx(src, dst) => {
                let src_str = display_operand(src, AsmType::Longword);
                let dst_str = display_operand(dst, AsmType::Quadword);
                write!(f, "movslq {src_str}, {dst_str}")
            }
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
                        Register::Sp => "sp",
                    };
                    write!(f, "set{cond_code} {reg_str}")
                } else {
                    let operand_str = display_operand(operand, AsmType::Longword);
                    write!(f, "set{cond_code} {operand_str}")
                }
            }
            Self::MovZX(_, _) => {
                panic!("Should be replaced during fix-up stage");
            }
        }
    }
}
