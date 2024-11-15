macro_rules! movq {
    ($src:expr, $dst:expr) => {
        AsmInstruction::Mov(AsmType::Quadword, $src, $dst)
    };
}

macro_rules! movl {
    ($src:expr, $dst:expr) => {
        AsmInstruction::Mov(AsmType::Longword, $src, $dst)
    };
}

macro_rules! movsd {
    ($src:ident, $dst:ident) => {
        AsmInstruction::Mov(
            AsmType::Double,
            Operand::Reg(Register::$src),
            Operand::Reg(Register::$dst),
        )
    };
    ($src:expr, $dst:expr) => {
        AsmInstruction::Mov(AsmType::Double, $src, $dst)
    };
}

macro_rules! subsd {
    ($src:expr, $dst:expr) => {
        AsmInstruction::Binary(AsmType::Double, AsmBinaryOp::Sub, $src, $dst)
    };
}
macro_rules! subl {
    ($src:expr, $dst:expr) => {
        AsmInstruction::Binary(AsmType::Longword, AsmBinaryOp::Sub, $src, $dst)
    };
}
macro_rules! subq {
    ($src:expr, $dst:expr) => {
        AsmInstruction::Binary(AsmType::Quadword, AsmBinaryOp::Sub, $src, $dst)
    };
}
macro_rules! addsd {
    ($src:expr, $dst:expr) => {
        AsmInstruction::Binary(AsmType::Double, AsmBinaryOp::Add, $src, $dst)
    };
}

macro_rules! addl {
    ($src:expr, $dst:expr) => {
        AsmInstruction::Binary(AsmType::Longword, AsmBinaryOp::Add, $src, $dst)
    };
}

macro_rules! addq {
    ($src:expr, $dst:expr) => {
        AsmInstruction::Binary(AsmType::Quadword, AsmBinaryOp::Add, $src, $dst)
    };
}

macro_rules! label {
    ($name:expr) => {
        AsmInstruction::Label($name)
    };
}

macro_rules! jae {
    ($to:expr) => {
        AsmInstruction::JmpCC(Condition::AE, $to)
    };
}

macro_rules! jnp {
    ($to:expr) => {
        AsmInstruction::JmpCC(Condition::NP, $to)
    };
}

macro_rules! jp {
    ($to:expr) => {
        AsmInstruction::JmpCC(Condition::P, $to)
    };
}

macro_rules! ret {
    () => {
        AsmInstruction::Ret
    };
}

macro_rules! jne {
    ($to:expr) => {
        AsmInstruction::JmpCC(Condition::NE, $to)
    };
}

macro_rules! je {
    ($to:expr) => {
        AsmInstruction::JmpCC(Condition::E, $to)
    };
}

macro_rules! jl {
    ($to:expr) => {
        AsmInstruction::JmpCC(Condition::L, $to)
    };
}

macro_rules! jmp {
    ($to:expr) => {
        AsmInstruction::Jmp($to)
    };
}

macro_rules! comisd {
    ($src:expr, $dst:expr) => {
        AsmInstruction::Cmp(AsmType::Double, $src, $dst)
    };
}

macro_rules! cmpl {
    ($src:expr, $dst:expr) => {
        AsmInstruction::Cmp(AsmType::Longword, $src, $dst)
    };
}

macro_rules! cmpq {
    ($src:expr, $dst:expr) => {
        AsmInstruction::Cmp(AsmType::Quadword, $src, $dst)
    };
}

macro_rules! cvttsd2siq {
    ($src:expr, $dst:expr) => {
        AsmInstruction::Cvttsd2si(AsmType::Quadword, $src, $dst)
    };
}
macro_rules! shrq {
    ($src:expr) => {
        AsmInstruction::Unary(AsmType::Quadword, AsmUnaryOp::Shr, $src)
    };
}

macro_rules! assemble {
    ($instructions:ident {}) => ();
    //\[address\] [, exp]*
    ($instructions:ident {$name:ident [$addr:expr] $(,$args:expr)* ; $($rest:tt)*}) => {
        $instructions.push( $name!(Operand::Data($addr.clone()) $(,$args.clone())*));
        assemble!($instructions {$($rest)*});
    };
    //\[address\], %REG
    ($instructions:ident {$name:ident [$addr:ident], %$reg:ident ; $($rest:tt)* }) => {
        $instructions.push($name!(Operand::Data($addr.clone()), Operand::Reg(Register::$reg())));
        assemble!($instructions {$($rest)*});
    };
    // #IMM, %REG
    ($instructions:ident {$name:ident #$l:expr, %$reg:ident ; $($rest:tt)*}) => {
        $instructions.push($name!(Operand::Imm($l as i128), Operand::Reg(Register::$reg())));
        assemble!($instructions {$($rest)*});
    };
    // #IMM, exp
    ($instructions:ident {$name:ident #$l:expr, $arg:expr ; $($rest:tt)*}) => {
        $instructions.push($name!(Operand::Imm($l as i128), $arg.clone()));
        assemble!($instructions {$($rest)*});
    };
    // [exp,] #IMM [,exp]*
    ($instructions:ident {$name:ident $($prearg:expr,)? #$l:literal $(,$args:expr)* ; $($rest:tt)* }) => {
        $instructions.push($name!($($prearg.clone(),)? Operand::Imm($l as i128) $(,$args.clone())*));
        assemble!($instructions {$($rest)*});
    };
    // %REG, %REG
    ($instructions:ident {$name:ident %$reg1:ident, %$reg2:ident ; $($rest:tt)*}) => {
        $instructions.push($name!(Operand::Reg(Register::$reg1()), Operand::Reg(Register::$reg2())));
        assemble!($instructions {$($rest)*});
    };
    ($instructions:ident {$name:ident $type:expr, %$reg_src:ident, %$reg_dst:ident ; $($rest:tt)*}) => {
        $instructions.push($name! ($type, Operand::Reg(Register::$reg_src()), Operand::Reg(Register::$reg_dst())));
        assemble!($instructions {$($rest)*});
    };
    // [exp,]+ %REG [,exp]?
    ($instructions:ident { $name:ident $($arg:expr,)+ %$reg:ident $(,$after:expr)? ; $($rest:tt)*}) => {
        $instructions.push($name!( $($arg.clone(),)+ Operand::Reg(Register::$reg()) $(,$after)?));
        assemble!($instructions {$($rest)*});
    };
    // %REG [, exp]*
    ($instructions:ident { $name:ident %$reg:ident $(,$args:expr)* ; $($rest:tt)*}) => {
        $instructions.push($name!(Operand::Reg(Register::$reg()) $(,$args.clone())*));
        assemble!($instructions {$($rest)*});
    };
    // label:
    ($instructions:ident {$label:ident: $($rest:tt)*}) => {
        $instructions.push(AsmInstruction::Label($label.clone()));
        assemble!($instructions {$($rest)*});
    };
    // exp [, exp]*
    ($instructions:ident { $name:ident $($args:expr),+ ; $($rest:tt)* }) => {
        $instructions.push($name!( $($args.clone()),+));
        assemble!($instructions { $($rest)* });
    };

    ($instructions:ident { $name:ident ; $($rest:tt)*}) => {
        $instructions.push($name!());
        assemble!($instructions {$($rest)*})
    };
}

macro_rules! xor {
    ($type:expr, $src:expr, $dst:expr) => {
        AsmInstruction::Binary($type, AsmBinaryOp::Xor, $src, $dst)
    };
}

macro_rules! xorl {
    (%$src:ident, $dst:expr) => {
        AsmInstruction::Binary(
            AsmType::Longword,
            AsmBinaryOp::Xor,
            Operand::Reg(Register::$src),
            $dst,
        )
    };
    ($src:expr, $dst:expr) => {
        AsmInstruction::Binary(AsmType::Longword, AsmBinaryOp::Xor, $src, $dst)
    };
}

macro_rules! xorq {
    ($src:expr, $dst:expr) => {
        AsmInstruction::Binary(AsmType::Quadword, AsmBinaryOp::Xor, $src, $dst)
    };
}

macro_rules! xorsd {
    ($src:expr, $dst:expr) => {
        AsmInstruction::Binary(AsmType::Double, AsmBinaryOp::Xor, $src, $dst)
    };
}

macro_rules! movzx {
    ($src:expr, $dst:expr) => {
        AsmInstruction::MovZX($src, $dst)
    };
}

macro_rules! movsx {
    ($src:expr, $dst:expr) => {
        AsmInstruction::Movsx($src, $dst)
    };
}

macro_rules! mov {
    ($type:expr, $src:expr, $dst:expr) => {
        AsmInstruction::Mov($type, $src, $dst)
    };
}

macro_rules! cmp {
    ($type:expr, $src:expr, $dst:expr) => {
        AsmInstruction::Cmp($type, $src, $dst)
    };
}

macro_rules! divsd {
    ($src:expr, $dst:expr) => {
        AsmInstruction::Binary(AsmType::Double, AsmBinaryOp::DivDouble, $src, $dst)
    };
}

macro_rules! sete {
    ($operand:expr) => {
        AsmInstruction::SetCC(Condition::E, $operand)
    };
}
macro_rules! setne {
    ($dst:expr) => {
        AsmInstruction::SetCC(Condition::NE, $dst)
    };
}

macro_rules! setcc {
    ($condition:expr, $dst:expr) => {
        AsmInstruction::SetCC($condition, $dst)
    };
}
macro_rules! setnp {
    ($dst:expr) => {
        AsmInstruction::SetCC(Condition::NP, $dst)
    };
}
macro_rules! setp {
    ($dst:expr) => {
        AsmInstruction::SetCC(Condition::P, $dst)
    };
}

macro_rules! cvtsi2sd {
    ($type:expr, $src:expr, $dst:expr) => {
        AsmInstruction::Cvtsi2sd($type, $src, $dst)
    };
}
macro_rules! cvttsd2si {
    ($type:expr, $src:expr, $dst:expr) => {
        AsmInstruction::Cvttsd2si($type, $src, $dst)
    };
}

macro_rules! cvtsi2sdq {
    ($src:expr, $dst:expr) => {
        AsmInstruction::Cvtsi2sd(AsmType::Quadword, $src, $dst)
    };
}

macro_rules! andq {
    ($src:expr, $dst:expr) => {
        AsmInstruction::Binary(AsmType::Quadword, AsmBinaryOp::And, $src, $dst)
    };
}

macro_rules! orq {
    ($src:expr, $dst:expr) => {
        AsmInstruction::Binary(AsmType::Quadword, AsmBinaryOp::Or, $src, $dst)
    };
}
macro_rules! push {
    ($from:expr) => {
        AsmInstruction::Push($from)
    };
}
macro_rules! call {
    ($name:expr) => {
        AsmInstruction::Call($name)
    };
}
macro_rules! cmovnel {
    ($src:expr, $dst:expr) => {
        AsmInstruction::CmovCC(AsmType::Longword, Condition::NE, $src, $dst)
    };
}
macro_rules! cmovcc {
    ($type:expr, $condition:expr, $src:expr, $dst:expr) => {
        AsmInstruction::CmovCC($type, $condition, $src, $dst)
    };
}

macro_rules! idiv {
    ($type:expr, $src:expr) => {
        AsmInstruction::Idiv($type, $src)
    };
}

macro_rules! div {
    ($type:expr, $src:expr) => {
        AsmInstruction::Div($type, $src)
    };
}

macro_rules! imul {
    ($type:expr, $src:expr, $dst:expr) => {
        AsmInstruction::Binary($type, AsmBinaryOp::Imul, $src, $dst)
    };
}
