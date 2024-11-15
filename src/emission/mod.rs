//! Code emission using [Display]
//!
//! Dummy module containing implementation of [Display] for [AsmAst](crate::codegen) which allows to use [to_string] method or as a formatting arg in any formatting context(e.g. [format!], [write!], [println!], ..)
//!
//! [Display]: https://doc.rust-lang.org/std/fmt/trait.Display.html
//! [to_string]: https://doc.rust-lang.org/std/string/trait.ToString.html#tymethod.to_string
//! [format!]: https://doc.rust-lang.org/std/macro.format.html
//! [write!]: https://doc.rust-lang.org/std/macro.write.html
//! [println!]: https://doc.rust-lang.org/std/macro.println.html

use crate::codegen::*;
use crate::semantic_analysis::StaticInit;

use std::fmt;

impl fmt::Display for AsmUnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Neg => write!(f, "neg"),
            Self::Not => write!(f, "not"),
            Self::Sal => write!(f, "sal"),
            Self::Shr => write!(f, "shr"),
            Self::Sar => write!(f, "sar"),
        }
    }
}

impl fmt::Display for AsmBinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Shr => write!(f, "shr"),
            Self::Add => write!(f, "add"),
            Self::Sub => write!(f, "sub"),
            Self::Imul => write!(f, "imul"),
            Self::And => write!(f, "and"),
            Self::Or => write!(f, "or"),
            Self::Xor => write!(f, "xor"),
            Self::Sal => write!(f, "sal"),
            Self::Sar => write!(f, "sar"),
            Self::DivDouble => write!(f, "div"),
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
            Self::Double(fl) => write!(f, "{}", fl.to_bits()),
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
        let (section, init) = if init.is_zero() && !init.is_double() {
            let bss = String::from(".bss");
            let zero = format!(".zero {alignment}");
            (bss, zero)
        } else {
            let data = String::from(".data");
            let prefix = if init.is_long() || init.is_double() {
                ".quad"
            } else {
                ".long"
            };
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

impl fmt::Display for AsmStaticConst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            name,
            alignment,
            init,
        } = self;
        writeln!(f, "\t.section .rodata")?;
        writeln!(f, "\t.align {alignment}")?;
        writeln!(f, ".L{name}:")?;
        writeln!(f, "\t.quad {init}")
    }
}

impl fmt::Display for AsmTopLevelItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Fun(fun) => write!(f, "{fun}"),
            Self::StaticVar(staticvar) => write!(f, "{staticvar}"),
            Self::StaticConst(static_const) => write!(f, "{static_const}"),
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
            Self::P => write!(f, "p"),
            Self::NP => write!(f, "np"),
        }
    }
}

impl fmt::Display for AsmType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Longword => write!(f, "l"),
            Self::Quadword => write!(f, "q"),
            Self::Double => write!(f, "sd"),
        }
    }
}

fn display_quadword_reg(r: Register) -> &'static str {
    match r {
        Register::AX => "%rax",
        Register::CX => "%rcx",
        Register::DX => "%rdx",
        Register::DI => "%rdi",
        Register::SI => "%rsi",
        Register::R8 => "%r8",
        Register::R9 => "%r9",
        Register::R10 => "%r10",
        Register::R11 => "%r11",
        Register::SP => "%rsp",
        _ => display_xmm(r),
    }
}
fn display_longword_reg(r: Register) -> &'static str {
    match r {
        Register::AX => "%eax",
        Register::CX => "%ecx",
        Register::DX => "%edx",
        Register::R10 => "%r10d",
        Register::R11 => "%r11d",
        Register::R8 => "%r8d",
        Register::R9 => "%r9d",
        Register::SI => "%esi",
        Register::DI => "%edi",
        Register::SP => "%esp",
        _ => display_xmm(r),
    }
    .into()
}

fn display_xmm(r: Register) -> &'static str {
    match r {
        Register::XMM0 => "%xmm0",
        Register::XMM1 => "%xmm1",
        Register::XMM2 => "%xmm2",
        Register::XMM3 => "%xmm3",
        Register::XMM4 => "%xmm4",
        Register::XMM5 => "%xmm5",
        Register::XMM6 => "%xmm6",
        Register::XMM7 => "%xmm7",
        Register::XMM14 => "%xmm14",
        Register::XMM15 => "%xmm15",
        _ => {
            panic!("internal error: attempt to display non floating register {r:?} in floating operation")
        }
    }
}

fn display_reg(r: Register, t: AsmType) -> &'static str {
    match t {
        AsmType::Longword => display_longword_reg(r),
        AsmType::Quadword => display_quadword_reg(r),
        AsmType::Double => display_xmm(r),
    }
}

fn display_operand(op: &Operand, t: AsmType) -> String {
    match op {
        Operand::Data(name) => {
            let prefix = ASM_SYM_TABLE
                .get_sym(name)
                .filter(AsmSymTabEntry::is_const)
                .is_some()
                .then_some(".L")
                .unwrap_or("");
            format!("{prefix}{name}(%rip)")
        }
        Operand::Reg(r) => display_reg(*r, t).to_string(),
        Operand::Imm(i) => format!("${i}"),
        Operand::Stack(i) => format!("{i}(%rbp)"),
        Operand::Pseudo(name) => panic!("pseudo operand {name} after codegen"),
    }
}

impl fmt::Display for AsmInstruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::CmovCC(t, c, src, dst) => {
                let src_str = display_operand(src, *t);
                let dst_str = display_operand(dst, *t);
                write!(f, "cmov{c}{t} {src_str}, {dst_str}")
            }
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
            Self::Binary(AsmType::Double, AsmBinaryOp::Imul, src, dst) => {
                let src_str = display_operand(src, AsmType::Double);
                let dst_str = display_operand(dst, AsmType::Double);

                write!(f, "mulsd {src_str}, {dst_str}")
            }
            Self::Binary(AsmType::Double, AsmBinaryOp::Xor, src, dst) => {
                let src_str = display_operand(src, AsmType::Double);
                let dst_str = display_operand(dst, AsmType::Double);
                write!(f, "xorpd {src_str}, {dst_str}")
            }
            Self::Binary(t, AsmBinaryOp::Sal, Operand::Reg(Register::CX), dst) => {
                let dst_str = display_operand(dst, *t);
                write!(f, "sal{t} %cl, {dst_str}")
            }
            Self::Binary(t, AsmBinaryOp::Sar, Operand::Reg(Register::CX), dst) => {
                let dst_str = display_operand(dst, *t);
                write!(f, "sar{t} %cl, {dst_str}")
            }
            Self::Binary(t, op, src, dst) => {
                let src_str = display_operand(src, *t);
                let dst_str = display_operand(dst, *t);
                write!(f, "{op}{t} {src_str}, {dst_str}")
            }
            Self::Cmp(AsmType::Double, src, dst) => {
                let src_str = display_operand(src, AsmType::Double);
                let dst_str = display_operand(dst, AsmType::Double);
                write!(f, "comisd {src_str}, {dst_str}")
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
                        Register::AX => "%al",
                        Register::DX => "%dl",
                        Register::R8 => "%r8b",
                        Register::R10 => "%r10b",
                        Register::R11 => "%r11b",
                        Register::CX => "%cl",
                        Register::DI => "%dil",
                        Register::SI => "%sil",
                        Register::R9 => "%r9b",
                        Register::SP => "%sp",
                        _ => display_xmm(*reg),
                    };
                    write!(f, "set{cond_code} {reg_str}")
                } else {
                    let operand_str = display_operand(operand, AsmType::Longword);
                    write!(f, "set{cond_code} {operand_str}")
                }
            }
            Self::MovZX(_, _) => {
                panic!("internal error: movzx in code emission");
            }
            Self::Cvtsi2sd(t, src, dst) => {
                let src_str = display_operand(src, *t);
                let dst_str = display_operand(dst, *t);
                write!(f, "cvtsi2sd{t} {src_str}, {dst_str}")
            }
            Self::Cvttsd2si(t, src, dst) => {
                let src_str = display_operand(src, *t);
                let dst_str = display_operand(dst, *t);
                write!(f, "cvttsd2si{t} {src_str}, {dst_str}")
            }
        }
    }
}
