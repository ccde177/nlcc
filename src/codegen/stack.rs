use crate::ast::Identifier;
use crate::codegen::ASM_SYM_TABLE;
use crate::codegen::{AsmBinaryOp, AsmInstruction, AsmInstructions, AsmType, Operand, Register};
use std::collections::HashMap;

pub fn allocate_stack(instructions: &mut AsmInstructions) {
    use AsmInstruction as I;
    let mut sa = StackAllocator::new();
    for inst in instructions.iter_mut() {
        match inst {
            I::SetCC(_, operand)
            | I::Push(operand)
            | I::Unary(_, _, operand)
            | I::Idiv(_, operand)
            | I::Div(_, operand) => {
                *operand = sa.allocate_if_pseudo(operand.clone());
            }
            I::Cvtsi2sd(_, src, dst)
            | I::Cvttsd2si(_, src, dst)
            | I::Cmp(_, src, dst)
            | I::MovZX(src, dst)
            | I::Mov(_, src, dst)
            | I::Binary(_, _, src, dst)
            | I::Movsx(src, dst) => {
                *src = sa.allocate_if_pseudo(src.clone());
                *dst = sa.allocate_if_pseudo(dst.clone());
            }
            I::Call(_) | I::Jmp(_) | I::JmpCC(_, _) | I::Label(_) | I::Ret | I::Cdq(_) => (),
        }
    }

    let prologue = sa.get_prologue();
    instructions.insert(0, prologue);
}

type StackAllocMap = HashMap<Identifier, i64>;

struct StackAllocator {
    offset: i64,
    map: StackAllocMap,
}

impl StackAllocator {
    fn new() -> Self {
        Self {
            map: StackAllocMap::new(),
            offset: 0,
        }
    }

    fn allocate_if_pseudo(&mut self, operand: Operand) -> Operand {
        match operand {
            Operand::Pseudo(name) => self.allocate(name),
            _ => operand,
        }
    }

    fn allocate(&mut self, name: Identifier) -> Operand {
        if let Some(entry) = self.map.get(&name) {
            return Operand::Stack(*entry);
        }

        let entry = ASM_SYM_TABLE.get_sym(&name).expect("Should have it");
        if entry.is_static() {
            return Operand::Data(name);
        }

        let size = entry.get_size().expect("Should not be function");
        self.offset += size;
        self.map.insert(name, -self.offset);
        let result = Operand::Stack(-self.offset);
        self.offset += 8 - size;
        result
    }

    #[allow(clippy::cast_sign_loss)]
    fn get_prologue(&self) -> AsmInstruction {
        let stack_size = self.offset + (16 - (self.offset % 16));
        let sp = Operand::Reg(Register::SP);
        AsmInstruction::Binary(
            AsmType::Quadword,
            AsmBinaryOp::Sub,
            Operand::Imm(stack_size as i128),
            sp,
        )
    }
}
