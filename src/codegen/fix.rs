use crate::codegen::asm_macro::*;
use crate::codegen::{AsmInstruction, AsmInstructions, AsmType, Operand, Register};

fn fix_imul(instruction: AsmInstruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if instruction.is_mul_sndmem() {
        if let AsmInstruction::Binary(t, _, src, dst) = instruction {
            assemble!(result {
                mov t, dst, %r11;
                imul t, src, %r11;
                mov t, %r11, dst;
            });
        }
    }
    result
}

fn fix_div(instruction: AsmInstruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if let AsmInstruction::Div(t, operand) = instruction {
        assemble!(result {
            mov t, operand, %r10;
            div t, %r10;
        });
    }
    result
}

fn fix_idiv(instruction: AsmInstruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if instruction.is_idiv_constant() {
        let AsmInstruction::Idiv(t, operand) = instruction else {
            unreachable!()
        };
        assemble!(result {
            mov t, operand, %r10;
            idiv t, %r10;
        });
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
        let temp_reg = if t.is_double() {
            Register::XMM14
        } else {
            Register::R10
        };
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
        assemble!(result {
            mov t, dst, %r11;
            cmp t, src, %r11;
        });
    }
    result
}

fn fix_movsx(instr: AsmInstruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if let AsmInstruction::Movsx(src, dst) = instr {
        assemble!(result {
            movl src, %r10;
            movsx %r10, %r11;
            movq %r11, dst;
        });
    }
    result
}

fn fix_mov_imm(instr: AsmInstruction) -> AsmInstructions {
    let mut result = AsmInstructions::new();
    if let AsmInstruction::Mov(AsmType::Quadword, src, dst) = instr {
        assemble!(result {
            movq src, %r10;
            movq %r10, dst;
        });
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

fn fix_zero_extend(instruction: AsmInstruction) -> AsmInstructions {
    use AsmInstruction as I;
    let mut result = AsmInstructions::new();
    if let I::MovZX(src, dst) = instruction {
        if dst.is_reg() {
            assemble!(result {
                movl src, dst;
            });
        }
        if dst.is_mem() {
            assemble!(result {
                movl src, %r11;
                movq %r11, dst;
            });
        }
    } else {
        panic!("internal error: bad fix predicate for fix_zero_extend");
    }
    result
}

fn fix_cvttsd2si(instruction: AsmInstruction) -> AsmInstructions {
    use AsmInstruction as I;
    let mut result = AsmInstructions::new();
    if let I::Cvttsd2si(t, src, dst) = instruction {
        assemble!(result {
            cvttsd2si t, src, %r11;
            mov t, %r11, dst;
        });
    } else {
        panic!("internal error: bad fix predicate for fix_cvttsd2si");
    }
    result
}

fn fix_cvtsi2sd(instruction: AsmInstruction) -> AsmInstructions {
    use AsmInstruction as I;
    let mut result = AsmInstructions::new();
    if let I::Cvtsi2sd(t, src, dst) = instruction {
        assemble!(result {
            mov t, src, %r10;
            cvtsi2sd t, %r10, %xmm15;
            movsd %xmm15, dst;
        });
    } else {
        panic!("internal error: bad fix predicate for fix_cvtsi2sd");
    }
    result
}

fn fix_comisd(instruction: AsmInstruction) -> AsmInstructions {
    use AsmInstruction as I;
    let mut result = AsmInstructions::new();
    if let I::Cmp(AsmType::Double, src, dst) = instruction {
        assemble!(result {
            movsd dst, %xmm15;
            comisd src, %xmm15;
        });
    } else {
        panic!("internal error: bad fix predicate for fix_comisd");
    }
    result
}

fn fix_binary_double(instruction: AsmInstruction) -> AsmInstructions {
    use AsmInstruction as I;
    let mut result = AsmInstructions::new();
    if let I::Binary(AsmType::Double, op, src, dst) = instruction {
        assemble!(result {
            movsd dst, %xmm15;
        });
        let binary =
            AsmInstruction::Binary(AsmType::Double, op, src, Operand::Reg(Register::XMM15));
        result.push(binary);
        assemble!(result {
            movsd %xmm15, dst;
        });
    } else {
        panic!("internal error: bad fix predicate for fix_binary_double");
    }
    result
}

pub fn fix_instructions(instrs: &mut AsmInstructions) {
    use AsmInstruction as I;
    fix_with_fixer(instrs, I::is_mul_sndmem, fix_imul);
    fix_with_fixer(instrs, I::is_idiv_constant, fix_idiv);
    fix_with_fixer(instrs, I::is_div_constant, fix_div);
    fix_with_fixer(instrs, I::is_movsx_invalid, fix_movsx);
    fix_with_fixer(instrs, I::mem_operands, fix_two_memoperands);
    fix_with_fixer(instrs, I::is_cmp_sndimm, fix_cmp_sndimm);
    fix_with_fixer(instrs, I::is_mov_immtoobig, fix_mov_imm);
    fix_with_fixer(instrs, I::is_imm_toobig, fix_imm_toobig);
    fix_with_fixer(instrs, I::is_truncate_imm_toobig, fix_truncate);
    fix_with_fixer(instrs, I::is_zero_extend, fix_zero_extend);
    fix_with_fixer(instrs, I::cvttsd2si_dst_not_reg, fix_cvttsd2si);
    fix_with_fixer(instrs, I::cvtsi2sd_needs_fix, fix_cvtsi2sd);
    fix_with_fixer(instrs, I::comisd_dst_not_reg, fix_comisd);
    fix_with_fixer(instrs, I::binary_double_dst_not_reg, fix_binary_double);
}
