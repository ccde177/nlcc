use crate::codegen::{AsmInstruction, AsmInstructions, AsmType, Operand, Register};

fn fix_imul(instruction: AsmInstruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if instruction.is_mul_sndmem() {
        let AsmInstruction::Binary(t, op, src, dst) = instruction else {
            unreachable!()
        };
        let temp_reg = Register::R11;
        let mov1 = AsmInstruction::Mov(t, dst.clone(), Operand::Reg(temp_reg));
        let imul = AsmInstruction::Binary(t, op, src, Operand::Reg(temp_reg));
        let mov2 = AsmInstruction::Mov(t, Operand::Reg(temp_reg), dst);
        result.push(mov1);
        result.push(imul);
        result.push(mov2);
    }
    result
}

fn fix_div(instruction: AsmInstruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if let AsmInstruction::Div(t, operand) = instruction {
        let temp_reg = Register::R10;
        let mov1 = AsmInstruction::Mov(t, operand, Operand::Reg(temp_reg));
        let idiv = AsmInstruction::Idiv(t, Operand::Reg(temp_reg));
        result.push(mov1);
        result.push(idiv);
    }
    result
}

fn fix_idiv(instruction: AsmInstruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if instruction.is_idiv_constant() {
        let AsmInstruction::Idiv(t, operand) = instruction else {
            unreachable!()
        };
        let temp_reg = Register::R10;
        let mov1 = AsmInstruction::Mov(t, operand, Operand::Reg(temp_reg));
        let idiv = AsmInstruction::Idiv(t, Operand::Reg(temp_reg));
        result.push(mov1);
        result.push(idiv);
    }
    result
}

fn fix_two_memoperands(instruction: AsmInstruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if instruction.mem_operands() {
        let (AsmInstruction::Mov(t, src, dst)
        | AsmInstruction::Binary(t, _, src, dst)
        | AsmInstruction::Cmp(t, src, dst)) = instruction.clone()
        else {
            unreachable!()
        };
        let temp_reg = Register::R10;
        let mov1 = AsmInstruction::Mov(t, src.clone(), Operand::Reg(temp_reg));
        let snd = match instruction {
            AsmInstruction::Binary(t, op, _, _) => {
                AsmInstruction::Binary(t, op, Operand::Reg(temp_reg), dst.clone())
            }
            AsmInstruction::Mov(t, _, _) => {
                AsmInstruction::Mov(t, Operand::Reg(temp_reg), dst.clone())
            }
            AsmInstruction::Cmp(t, _, _) => {
                AsmInstruction::Cmp(t, Operand::Reg(temp_reg), dst.clone())
            }
            _ => unreachable!(),
        };
        result.push(mov1);
        result.push(snd);
    }
    result
}

fn fix_cmp_sndimm(instr: AsmInstruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if let AsmInstruction::Cmp(t, src, dst) = instr {
        let temp_reg = Register::R11;
        let mov = AsmInstruction::Mov(t, dst, Operand::Reg(temp_reg));
        let cmp = AsmInstruction::Cmp(t, src, Operand::Reg(temp_reg));
        result.push(mov);
        result.push(cmp);
    }
    result
}

fn fix_movsx(instr: AsmInstruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if let AsmInstruction::Movsx(src, dst) = instr {
        let r10 = Operand::Reg(Register::R10);
        let r11 = Operand::Reg(Register::R11);
        let mov1 = AsmInstruction::Mov(AsmType::Longword, src, r10.clone());
        let movsx = AsmInstruction::Movsx(r10, r11.clone());
        let mov2 = AsmInstruction::Mov(AsmType::Quadword, r11, dst);
        result.push(mov1);
        result.push(movsx);
        result.push(mov2);
    }
    result
}

fn fix_mov_imm(instr: AsmInstruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if let AsmInstruction::Mov(AsmType::Quadword, src, dst) = instr {
        let r10 = Operand::Reg(Register::R10);
        let mov_to_r10 = AsmInstruction::Mov(AsmType::Quadword, src, r10.clone());
        let mov_from_r10 = AsmInstruction::Mov(AsmType::Quadword, r10, dst);
        result.push(mov_to_r10);
        result.push(mov_from_r10);
    }
    result
}

fn fix_imm_toobig(mut instr: AsmInstruction) -> AsmInstructions {
    use AsmInstruction as I;
    use AsmType as AT;
    let mut result = AsmInstructions::new();
    if let I::Binary(AT::Quadword, _, src, _) | I::Push(src) | I::Cmp(AT::Quadword, src, _) =
        &mut instr
    {
        let r10 = Operand::Reg(Register::R10);
        let save_to_r10 = I::Mov(AT::Quadword, src.clone(), r10.clone());
        *src = r10;
        result.push(save_to_r10);
        result.push(instr);
    }
    result
}

#[allow(clippy::cast_possible_truncation)]
fn fix_truncate(mut instr: AsmInstruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if let AsmInstruction::Mov(AsmType::Longword, Operand::Imm(src), _) = &mut instr {
        let v = *src as i32;
        *src = i128::from(v);
    }
    result.push(instr);
    result
}

fn fix_with_fixer(
    instructions: &mut AsmInstructions,
    predicate: fn(&AsmInstruction) -> bool,
    fixer: fn(AsmInstruction) -> Vec<AsmInstruction>,
) {
    let indexes: Vec<_> = instructions
        .iter()
        .enumerate()
        .filter(|(_, i)| predicate(i))
        .map(|(i, _)| i)
        .collect();
    let mut count = 0;
    for i in indexes {
        let instr = instructions.remove(i + count);
        let fixed_instructions = fixer(instr);
        for instr in fixed_instructions {
            instructions.insert(i + count, instr);
            count += 1;
        }
        count -= 1;
    }
}

pub fn fix_zero_extend(instruction: AsmInstruction) -> AsmInstructions {
    use AsmInstruction as I;
    let mut result = AsmInstructions::new();
    if let I::MovZX(src, dst) = instruction {
        match dst {
            op if op.is_reg() => {
                let mov = I::Mov(AsmType::Longword, src, op);
                result.push(mov);
            }
            op if op.is_mem() => {
                let r11 = Operand::Reg(Register::R11);
                let mov1 = I::Mov(AsmType::Longword, src, r11.clone());
                let mov2 = I::Mov(AsmType::Quadword, r11, op);
                result.push(mov1);
                result.push(mov2);
            }
            _ => (),
        }
    }
    result
}

pub fn fix_instructions(instructions: &mut AsmInstructions) {
    use AsmInstruction as I;
    fix_with_fixer(instructions, I::is_mul_sndmem, fix_imul);
    fix_with_fixer(instructions, I::is_idiv_constant, fix_idiv);
    fix_with_fixer(instructions, I::is_div_constant, fix_div);
    fix_with_fixer(instructions, I::is_movsx_invalid, fix_movsx);
    fix_with_fixer(instructions, I::mem_operands, fix_two_memoperands);
    fix_with_fixer(instructions, I::is_cmp_sndimm, fix_cmp_sndimm);
    fix_with_fixer(instructions, I::is_mov_immtoobig, fix_mov_imm);
    fix_with_fixer(instructions, I::is_imm_toobig, fix_imm_toobig);
    fix_with_fixer(instructions, I::is_truncate_imm_toobig, fix_truncate);
    fix_with_fixer(instructions, I::is_zero_extend, fix_zero_extend);
}
