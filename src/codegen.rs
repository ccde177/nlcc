use crate::parser;
use std::fmt;

pub enum Program {
    Program(Function)
}

type Instructions = Vec<Instruction>;

pub struct Function {
    name: String,
    body: Instructions
}

pub enum Instruction {
    Mov(Operand, Operand),
    Ret
}

pub enum Operand {
    Imm(u64),
    Register
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Imm(i) => write!(f, "${i}"),
            Self::Register => write!(f, "%eax")
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Mov(o1, o2) => write!(f, "movl {o1}, {o2}"),
            Self::Ret => write!(f, "ret")
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
            Self::Program(fun) => write!(f, "{fun}")
        }
    }
}

fn gen_exp(exp: parser::Expression) -> Operand {
    match exp {
        parser::Expression::Constant(i) => Operand::Imm(i)
    }
}

fn gen_body(body: parser::Statement) -> Instructions {
    match body {
        parser::Statement::Return(exp) => vec![
            Instruction::Mov(gen_exp(exp), Operand::Register),
            Instruction::Ret
        ]
    }
}
fn gen_fundef(f: parser::Function) -> Function {
    Function {
        name: f.name, 
        body: gen_body(f.body)
    }
}

pub fn codegen(ast: parser::Program) -> Program {
    match ast {
        parser::Program::FunDef(f) => Program::Program(gen_fundef(f))
    }
}
