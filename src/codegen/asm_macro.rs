/// Convinient macro for writing instructions in assembly-like manner
///
/// # Examples
///
/// ```
/// use nlcc::assemble;
/// use nlcc::codegen::{AsmInstruction, AsmInstructions, AsmType, Operand, Register, Condition};
/// let mut instructions = AsmInstructions::new();
/// let jump_label = String::from("jump_label");
/// let some_global_var = String::from("some_global_var");
/// assemble!(instructions {
///     cmpq #10, %rax;
///     je jump_label;
///     subq [some_global_var], %rax;
///jump_label:
///     ret;
/// });
/// assert_eq!(instructions[0], AsmInstruction::Cmp(AsmType::Quadword, Operand::Imm(10), Operand::Reg(Register::AX)));
/// assert_eq!(instructions[1], AsmInstruction::JmpCC(Condition::E, jump_label));
/// assert_eq!(instructions[4], AsmInstruction::Ret);
///```
///

#[macro_export]
macro_rules! assemble {
    ($instructions:ident {}) => ();
    //\[address\] [, exp]*
    ($instructions:ident { $name:ident [$addr:expr] $(,$args:expr)* ; $($rest:tt)* }) => {
        $instructions.push( AsmInstruction::$name(Operand::Data($addr.clone()) $(,$args.clone())*));
        assemble!($instructions {$($rest)*});
    };
    //\[address\], %REG
    ($instructions:ident { $name:ident [$addr:ident], %$reg:ident ; $($rest:tt)* }) => {
        $instructions.push(AsmInstruction::$name(Operand::Data($addr.clone()), Operand::Reg(Register::$reg())));
        assemble!($instructions {$($rest)*});
    };
    // #IMM, %REG
    ($instructions:ident { $name:ident #$l:expr, %$reg:ident ; $($rest:tt)* }) => {
        $instructions.push(AsmInstruction::$name(Operand::Imm($l as i128), Operand::Reg(Register::$reg())));
        assemble!($instructions {$($rest)*});
    };
    // #IMM, exp
    ($instructions:ident { $name:ident #$l:expr, $arg:expr ; $($rest:tt)* }) => {
        $instructions.push(AsmInstruction::$name(Operand::Imm($l as i128), $arg.clone()));
        assemble!($instructions {$($rest)*});
    };
    // [exp,] #IMM [,exp]*
    ($instructions:ident { $name:ident $($prearg:expr,)? #$l:literal $(,$args:expr)* ; $($rest:tt)* }) => {
        $instructions.push(AsmInstruction::$name($($prearg.clone(),)? Operand::Imm($l as i128) $(,$args.clone())*));
        assemble!($instructions {$($rest)*});
    };
    // %REG, %REG
    ($instructions:ident { $name:ident %$reg1:ident, %$reg2:ident ; $($rest:tt)* }) => {
        $instructions.push(AsmInstruction::$name(Operand::Reg(Register::$reg1()), Operand::Reg(Register::$reg2())));
        assemble!($instructions {$($rest)*});
    };
    ($instructions:ident { $name:ident $type:expr, %$reg_src:ident, %$reg_dst:ident ; $($rest:tt)* }) => {
        $instructions.push(AsmInstruction::$name($type, Operand::Reg(Register::$reg_src()), Operand::Reg(Register::$reg_dst())));
        assemble!($instructions {$($rest)*});
    };
    // [exp,]+ %REG [,exp]?
    ($instructions:ident { $name:ident $($arg:expr,)+ %$reg:ident $(,$after:expr)? ; $($rest:tt)* }) => {
        $instructions.push(AsmInstruction::$name( $($arg.clone(),)+ Operand::Reg(Register::$reg()) $(,$after)?));
        assemble!($instructions {$($rest)*});
    };
    // %REG [, exp]*
    ($instructions:ident { $name:ident %$reg:ident $(,$args:expr)* ; $($rest:tt)* }) => {
        $instructions.push(AsmInstruction::$name(Operand::Reg(Register::$reg()) $(,$args.clone())*));
        assemble!($instructions {$($rest)*});
    };
    // label:
    ($instructions:ident { $label:ident: $($rest:tt)* }) => {
        $instructions.push(AsmInstruction::Label($label.clone()));
        assemble!($instructions {$($rest)*});
    };
    // exp [, exp]*
    ($instructions:ident { $name:ident $($args:expr),+ ; $($rest:tt)* }) => {
        $instructions.push(AsmInstruction::$name( $($args.clone()),+));
        assemble!($instructions { $($rest)* });
    };

    ($instructions:ident { $name:ident ; $($rest:tt)* }) => {
        $instructions.push(AsmInstruction::$name());
        assemble!($instructions {$($rest)*})
    };
}
pub(super) use assemble;
