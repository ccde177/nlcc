#[cfg(test)]
mod tacky_tests;

use crate::ast::*;
use crate::semantic_analysis::{StaticInit, SYM_TABLE};

use std::sync::atomic::{AtomicUsize, Ordering};

pub type Identifier = String;
pub type TInstructions = Vec<TInstruction>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TAst {
    pub toplevel_items: Vec<TopLevelItem>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TFunction {
    pub name: Identifier,
    pub params: Vec<Identifier>,
    pub body: TInstructions,
    pub global: bool,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StaticVariable {
    pub name: Identifier,
    pub global: bool,
    pub init: StaticInit,
    pub var_type: Type,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TopLevelItem {
    Fun(TFunction),
    Var(StaticVariable),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TInstruction {
    Truncate(TValue, TValue),   //(src, dst)
    SignExtend(TValue, TValue), // (src,dst)
    Return(TValue),
    Unary(TUnaryOp, TValue, TValue),
    Binary(TBinaryOp, TValue, TValue, TValue),
    Copy(TValue, TValue),
    Jump(Identifier),
    JumpIfZero(TValue, Identifier),
    JumpIfNotZero(TValue, Identifier),
    Label(Identifier),
    FunCall {
        name: Identifier,
        args: Vec<TValue>,
        dst: TValue,
    },
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TBinaryOp {
    Add,
    Substract,
    Multiply,
    Divide,
    Reminder,
    IsEqual,
    IsNotEqual,
    IsLessThan,
    IsLessOrEqual,
    IsGreaterThan,
    IsGreaterOrEqual,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TValue {
    Constant(AstConst),
    Var(Identifier),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TUnaryOp {
    Complement,
    Negate,
    LogicalNot,
}

impl TBinaryOp {
    pub fn is_shift(self) -> bool {
        matches!(self, TBinaryOp::ShiftLeft | TBinaryOp::ShiftRight)
    }
    pub fn is_div(self) -> bool {
        matches!(self, TBinaryOp::Divide)
    }

    pub fn is_rem(self) -> bool {
        matches!(self, TBinaryOp::Reminder)
    }

    pub fn is_comp(self) -> bool {
        matches!(
            self,
            Self::IsEqual
                | Self::IsNotEqual
                | Self::IsLessThan
                | Self::IsLessOrEqual
                | Self::IsGreaterThan
                | Self::IsGreaterOrEqual
        )
    }
}

static __GLOBAL_COUNTER_NAME: AtomicUsize = AtomicUsize::new(0);
fn get_uniq_name() -> String {
    let c = __GLOBAL_COUNTER_NAME.fetch_add(1, Ordering::AcqRel);
    format!("tmp.{c}")
}

static __GLOBAL_COUNTER_LABEL: AtomicUsize = AtomicUsize::new(0);
fn get_uniq_label() -> String {
    let c = __GLOBAL_COUNTER_LABEL.fetch_add(1, Ordering::AcqRel);
    format!("label_{c}")
}

impl From<AstUnaryOp> for TUnaryOp {
    fn from(value: AstUnaryOp) -> Self {
        match value {
            AstUnaryOp::Complement => TUnaryOp::Complement,
            AstUnaryOp::Negate => TUnaryOp::Negate,
            AstUnaryOp::LogicalNot => TUnaryOp::LogicalNot,
            _ => unimplemented!(),
        }
    }
}

impl From<AstBinaryOp> for TBinaryOp {
    fn from(value: AstBinaryOp) -> Self {
        match value {
            AstBinaryOp::Add => Self::Add,
            AstBinaryOp::Substract => Self::Substract,
            AstBinaryOp::Mod => Self::Reminder,
            AstBinaryOp::Multiply => Self::Multiply,
            AstBinaryOp::Div => Self::Divide,
            AstBinaryOp::IsEqual => Self::IsEqual,
            AstBinaryOp::IsNotEqual => Self::IsNotEqual,
            AstBinaryOp::LessThan => Self::IsLessThan,
            AstBinaryOp::LessOrEqual => Self::IsLessOrEqual,
            AstBinaryOp::GreaterThan => Self::IsGreaterThan,
            AstBinaryOp::GreaterOrEqual => Self::IsGreaterOrEqual,
            AstBinaryOp::BitwiseAnd => Self::BitwiseAnd,
            AstBinaryOp::BitwiseOr => Self::BitwiseOr,
            AstBinaryOp::BitwiseXor => Self::BitwiseXor,
            AstBinaryOp::ShiftLeft => Self::ShiftLeft,
            AstBinaryOp::ShiftRight => Self::ShiftRight,
            _ => unimplemented!(),
        }
    }
}

fn emit_postfix_incdec(op: AstUnaryOp, exp: Exp, instructions: &mut TInstructions) -> TValue {
    let t = exp.get_type().expect("Should have type after typechecking");
    let one = AstConst::new(&t, 1).expect("Should always be Some");
    let one = TValue::Constant(one);
    let op = if matches!(op, AstUnaryOp::PostfixIncrement) {
        TBinaryOp::Add
    } else {
        TBinaryOp::Substract
    };
    let original = emit_expression(exp, instructions);

    let new_var = new_tacky_var(t);

    let copy = TInstruction::Copy(original.clone(), new_var.clone());
    let modify = TInstruction::Binary(op, new_var.clone(), one, original);
    instructions.push(copy);
    instructions.push(modify);
    new_var
}

fn emit_prefix_incdec(op: AstUnaryOp, exp: Exp, instructions: &mut TInstructions) -> TValue {
    let t = exp.get_type().expect("Should have type after typechecking");
    let one = AstConst::new(&t, 1).expect("Should always be Some");
    let one = TValue::Constant(one);
    let op = if matches!(op, AstUnaryOp::PrefixIncrement) {
        TBinaryOp::Add
    } else {
        TBinaryOp::Substract
    };
    let src = emit_expression(exp, instructions);

    let dst = new_tacky_var(t);

    let modify = TInstruction::Binary(op, src.clone(), one, dst.clone());
    let copy = TInstruction::Copy(dst.clone(), src.clone());
    instructions.push(modify);
    instructions.push(copy);
    src
}

fn emit_unary(op: AstUnaryOp, exp: Exp, instructions: &mut TInstructions) -> TValue {
    let t = exp
        .get_type()
        .expect("Should have type after type checking");
    let tacky_op = TUnaryOp::from(op);
    let src = emit_expression(exp, instructions);
    let dst = new_tacky_var(t);
    let tacky_instruction = TInstruction::Unary(tacky_op, src, dst.clone());
    instructions.push(tacky_instruction);
    dst
}

fn emit_logical_and(src: Exp, dst: Exp, instructions: &mut TInstructions) -> TValue {
    let false_label = get_uniq_label();
    let label_end = get_uniq_label();
    let result = new_tacky_var(Type::Int);
    let copy0 = TInstruction::Copy(TValue::Constant(AstConst::Int(0)), result.clone());
    let copy1 = TInstruction::Copy(TValue::Constant(AstConst::Int(0)), result.clone());
    let jumpend = TInstruction::Jump(label_end.clone());

    let v1 = emit_expression(src, instructions);
    let jz1 = TInstruction::JumpIfZero(v1.clone(), false_label.clone());
    instructions.push(jz1);

    let v2 = emit_expression(dst, instructions);
    let jz2 = TInstruction::JumpIfZero(v2.clone(), false_label.clone());
    instructions.push(jz2);

    instructions.push(copy1);
    instructions.push(jumpend);
    instructions.push(TInstruction::Label(false_label));
    instructions.push(copy0);
    instructions.push(TInstruction::Label(label_end));

    result
}

fn emit_logical_or(src: Exp, dst: Exp, instructions: &mut TInstructions) -> TValue {
    let true_label = get_uniq_label();
    let label_end = get_uniq_label();
    let result = new_tacky_var(Type::Int);
    let copy0 = TInstruction::Copy(TValue::Constant(AstConst::Int(0)), result.clone());
    let copy1 = TInstruction::Copy(TValue::Constant(AstConst::Int(1)), result.clone());
    let jumpend = TInstruction::Jump(label_end.clone());

    let v1 = emit_expression(src, instructions);
    let jnz1 = TInstruction::JumpIfNotZero(v1.clone(), true_label.clone());
    instructions.push(jnz1);

    let v2 = emit_expression(dst, instructions);
    let jnz2 = TInstruction::JumpIfNotZero(v2.clone(), true_label.clone());
    instructions.push(jnz2);

    instructions.push(copy0);
    instructions.push(jumpend);
    instructions.push(TInstruction::Label(true_label));
    instructions.push(copy1);
    instructions.push(TInstruction::Label(label_end));

    result
}

fn emit_binary(
    op: AstBinaryOp,
    exp1: Exp,
    exp2: Exp,
    instructions: &mut TInstructions,
    t: Type,
) -> TValue {
    let v1 = emit_expression(exp1, instructions);
    let v2 = emit_expression(exp2, instructions);

    let dst = new_tacky_var(t);

    let tacky_op = TBinaryOp::from(op);
    let tacky_instruction = TInstruction::Binary(tacky_op, v1, v2, dst.clone());
    instructions.push(tacky_instruction);
    dst
}

fn emit_assignment(var: Exp, rhs: Exp, instructions: &mut TInstructions) -> TValue {
    let UntypedExp::Var(name) = var.into() else {
        unreachable!()
    };
    let rhs = emit_expression(rhs, instructions);
    let var = TValue::Var(name);
    let copy = TInstruction::Copy(rhs, var.clone());
    instructions.push(copy);
    var
}

fn emit_conditional(cond: ConditionalExp, instructions: &mut TInstructions) -> TValue {
    let ConditionalExp {
        condition,
        then,
        els,
    } = cond;
    let c = emit_expression(*condition, instructions);
    let e2 = get_uniq_label();
    let jz = TInstruction::JumpIfZero(c, e2.clone());
    instructions.push(jz);

    let then_type = then
        .get_type()
        .expect("Should have type after type checking");

    let v1 = emit_expression(*then, instructions);

    let result = new_tacky_var(then_type);

    let copy = TInstruction::Copy(v1, result.clone());
    instructions.push(copy);
    let end = get_uniq_label();
    let jmp_end = TInstruction::Jump(end.clone());
    instructions.push(jmp_end);
    let e2_label = TInstruction::Label(e2);
    instructions.push(e2_label);
    let v2 = emit_expression(*els, instructions);
    let copy = TInstruction::Copy(v2, result.clone());
    instructions.push(copy);
    let end_label = TInstruction::Label(end);
    instructions.push(end_label);
    result
}

fn emit_call(
    name: Identifier,
    args: Vec<Exp>,
    instructions: &mut TInstructions,
    ret_type: Type,
) -> TValue {
    let args = args
        .into_iter()
        .map(|e| emit_expression(e, instructions))
        .collect();

    let dst = new_tacky_var(ret_type);

    let tacky_call = TInstruction::FunCall {
        name,
        args,
        dst: dst.clone(),
    };
    instructions.push(tacky_call);
    dst
}

fn new_tacky_var(t: Type) -> TValue {
    let var_name = get_uniq_name();
    SYM_TABLE.add_local_sym(var_name.clone(), t);
    TValue::Var(var_name)
}

fn emit_cast(t: Type, e: Exp, instructions: &mut TInstructions) -> TValue {
    let inner_type = e.get_type().expect("Should have type after typechecking");
    let result = emit_expression(e, instructions);
    if t == inner_type {
        return result;
    }
    let dst = new_tacky_var(t.clone());
    let cast_instr = if t == Type::Long {
        TInstruction::SignExtend(result, dst.clone())
    } else {
        TInstruction::Truncate(result, dst.clone())
    };
    instructions.push(cast_instr);

    dst
}

fn emit_expression(exp: Exp, instructions: &mut TInstructions) -> TValue {
    use UntypedExp as UE;
    let t = exp
        .get_type()
        .expect("Should have type after type checking");
    let ue = exp.into();
    match ue {
        UE::Cast(casting_t, e) => emit_cast(casting_t, *e, instructions),
        UE::Call(name, args) => emit_call(name, args, instructions, t),
        UE::Unary(op @ (AstUnaryOp::PostfixIncrement | AstUnaryOp::PostfixDecrement), exp) => {
            emit_postfix_incdec(op, *exp, instructions)
        }
        UE::Unary(op @ (AstUnaryOp::PrefixIncrement | AstUnaryOp::PrefixDecrement), exp) => {
            emit_prefix_incdec(op, *exp, instructions)
        }
        UE::Unary(op, exp) => emit_unary(op, *exp, instructions),
        UE::Binary(AstBinaryOp::LogicalAnd, src, dst) => emit_logical_and(*src, *dst, instructions),
        UE::Binary(AstBinaryOp::LogicalOr, src, dst) => emit_logical_or(*src, *dst, instructions),
        UE::Binary(op, exp1, exp2) => emit_binary(op, *exp1, *exp2, instructions, t),
        UE::Assignment(var, rhs) => emit_assignment(*var, *rhs, instructions),
        UE::Var(name) => TValue::Var(name.clone()),
        UE::Constant(u) => TValue::Constant(u),
        UE::Conditional(cond) => emit_conditional(cond, instructions),
    }
}

fn emit_forinit(forinit: AstForInit, instructions: &mut TInstructions) {
    match forinit {
        AstForInit::InitDecl(vardec) => emit_vardec(vardec, instructions),
        AstForInit::InitExp(Some(exp)) => {
            let _ = emit_expression(exp, instructions);
        }
        AstForInit::InitExp(None) => (),
    }
}

fn emit_switch(switch: Switch, instructions: &mut TInstructions) {
    let Switch {
        ctrl_exp,
        body,
        cases,
        label,
    } = switch;
    let v = emit_expression(ctrl_exp, instructions);
    let mut default = None;
    let label_end = format!("break_{label}");
    for case in cases {
        if let Some(value) = case.0 {
            let cmp_result = new_tacky_var(Type::Int);
            let is_equal = TInstruction::Binary(
                TBinaryOp::IsEqual,
                v.clone(),
                TValue::Constant(value),
                cmp_result.clone(),
            );
            instructions.push(is_equal);
            let jnz = TInstruction::JumpIfNotZero(cmp_result, case.1);
            instructions.push(jnz);
        } else {
            default = Some(case.1);
        }
    }

    if let Some(default) = default {
        let jmp = TInstruction::Jump(default);
        instructions.push(jmp);
    }

    let jmp_end = TInstruction::Jump(label_end.clone());
    instructions.push(jmp_end);

    emit_statement(*body, instructions);

    let tlabel_end = TInstruction::Label(label_end);
    instructions.push(tlabel_end);
}

fn emit_cased(cased: CasedStatement, instructions: &mut TInstructions) {
    let label = TInstruction::Label(cased.label);
    instructions.push(label);
    emit_statement(*cased.body, instructions);
}

fn emit_dcased(dcased: DCasedStatement, instructions: &mut TInstructions) {
    let label = TInstruction::Label(dcased.label);
    instructions.push(label);
    emit_statement(*dcased.body, instructions);
}

fn emit_dowhile(dowhile: DoWhile, instructions: &mut TInstructions) {
    let DoWhile {
        body,
        condition,
        label,
    } = dowhile;
    let continue_label = format!("continue_{label}");
    let break_label = format!("break_{label}");
    let start_label = format!("start_{label}");

    let start = TInstruction::Label(start_label.clone());
    instructions.push(start);

    emit_statement(*body, instructions);

    let cont = TInstruction::Label(continue_label.clone());
    instructions.push(cont);

    let v = emit_expression(condition, instructions);
    let jnz = TInstruction::JumpIfNotZero(v, start_label);
    instructions.push(jnz);

    let brk = TInstruction::Label(break_label);
    instructions.push(brk);
}

fn emit_for_st(for_st: For, instructions: &mut TInstructions) {
    let For {
        init,
        condition,
        post,
        body,
        label,
    } = for_st;
    let continue_label = format!("continue_{label}");
    let break_label = format!("break_{label}");
    let start_label = format!("start_{label}");

    emit_forinit(init, instructions);

    let start = TInstruction::Label(start_label.clone());
    instructions.push(start);

    if let Some(exp) = condition {
        let v = emit_expression(exp, instructions);
        let jz = TInstruction::JumpIfZero(v, break_label.clone());
        instructions.push(jz);
    }

    emit_statement(*body, instructions);

    let cont = TInstruction::Label(continue_label);
    instructions.push(cont);

    if let Some(exp) = post {
        let _ = emit_expression(exp, instructions);
    }

    let jmp = TInstruction::Jump(start_label);
    instructions.push(jmp);

    let brk = TInstruction::Label(break_label);
    instructions.push(brk);
}

fn emit_while_st(while_st: While, instructions: &mut TInstructions) {
    let While {
        condition,
        body,
        label,
    } = while_st;
    let continue_label = format!("continue_{label}");
    let break_label = format!("break_{label}");

    let cont = TInstruction::Label(continue_label.clone());
    instructions.push(cont);

    let v = emit_expression(condition, instructions);
    let jz = TInstruction::JumpIfZero(v, break_label.clone());
    instructions.push(jz);

    emit_statement(*body, instructions);

    let jump = TInstruction::Jump(continue_label);
    instructions.push(jump);

    let brk = TInstruction::Label(break_label);
    instructions.push(brk);
}

fn emit_if_st(if_st: If, instructions: &mut TInstructions) {
    let If {
        condition,
        then,
        els,
    } = if_st;
    let c = emit_expression(condition, instructions);
    let end_or_else = get_uniq_label();
    let jz = TInstruction::JumpIfZero(c, end_or_else.clone());
    instructions.push(jz);
    emit_statement(*then, instructions);
    let end_or_else = TInstruction::Label(end_or_else);
    if els.is_some() {
        let els_label = end_or_else;
        let end = get_uniq_label();
        let jump_end = TInstruction::Jump(end.clone());
        instructions.push(jump_end);
        instructions.push(els_label);
        emit_statement(*els.unwrap(), instructions);
        let end_label = TInstruction::Label(end);
        instructions.push(end_label);
    } else {
        instructions.push(end_or_else);
    }
}

fn emit_compound(block: AstBlock, instructions: &mut TInstructions) {
    let AstBlock { items } = block;
    let mut block_items = emit_block_items(items);
    instructions.append(&mut block_items);
}

fn emit_labeled_st(name: String, st: Statement, instructions: &mut TInstructions) {
    let label = TInstruction::Label(name);
    instructions.push(label);
    emit_statement(st, instructions);
}

fn emit_statement(statement: Statement, instructions: &mut TInstructions) {
    use Statement as S;
    match statement {
        S::Break(label) | Statement::Continue(label) | Statement::Goto(label) => {
            let jump = TInstruction::Jump(label);
            instructions.push(jump);
        }
        S::Return(e) => {
            let value = emit_expression(e, instructions);
            instructions.push(TInstruction::Return(value));
        }
        S::Exp(e) => {
            emit_expression(e, instructions);
        }
        S::Switch(switch) => emit_switch(switch, instructions),
        S::Cased(cased) => emit_cased(cased, instructions),
        S::DCased(dcased) => emit_dcased(dcased, instructions),
        S::DoWhile(dowhile) => emit_dowhile(dowhile, instructions),
        S::For(for_st) => emit_for_st(for_st, instructions),
        S::While(while_st) => emit_while_st(while_st, instructions),
        S::If(if_st) => emit_if_st(if_st, instructions),
        S::Compound(block) => emit_compound(block, instructions),
        S::Labeled(name, st) => emit_labeled_st(name, *st, instructions),
        S::Null => (),
    }
}

fn emit_vardec(vardec: VarDec, instructions: &mut TInstructions) {
    if !vardec.storage_class.is_auto() {
        return;
    }
    if let Some(init) = vardec.init {
        let rhs = emit_expression(init, instructions);
        let var = TValue::Var(vardec.name);
        let copy = TInstruction::Copy(rhs, var.clone());
        instructions.push(copy);
    }
}

fn emit_block_items(blockitems: AstBlockItems) -> TInstructions {
    let mut instructions = TInstructions::new();
    for block in blockitems {
        match block {
            AstBlockItem::S(s) => emit_statement(s, &mut instructions),
            AstBlockItem::D(Declaration::Var(vardec)) => emit_vardec(vardec, &mut instructions),
            AstBlockItem::D(_) => (),
        }
    }
    instructions
}

fn emit_fundec(f: FunDec) -> Option<TFunction> {
    let FunDec {
        name,
        params,
        body,
        storage_class: _,
        fun_type: _,
    } = f;
    if let Some(body) = body {
        let AstBlock { items } = body;
        let mut body = emit_block_items(items);
        body.push(TInstruction::Return(TValue::Constant(AstConst::Int(0))));

        let st_entry = SYM_TABLE.get_symbol(&name).expect("Should never fail");
        let f = TFunction {
            name,
            params,
            body,
            global: st_entry.is_global(),
        };
        Some(f)
    } else {
        None
    }
}

fn emit_toplevel_dec(dec: Declaration) -> Option<TopLevelItem> {
    match dec {
        Declaration::Fun(fundec) => emit_fundec(fundec).map(TopLevelItem::Fun),
        // It will be processed later during emit_static_symbols
        Declaration::Var(_) => None,
    }
}

fn emit_static_symbols() -> Vec<TopLevelItem> {
    let mut defs = Vec::new();
    for symbol in SYM_TABLE.get_keys() {
        let entry = SYM_TABLE
            .get_symbol(&symbol)
            .expect("Should always be Some");
        if let Some(init) = entry.get_init() {
            let sym_type = entry.sym_type.clone();
            let tentative_init = match sym_type {
                Type::Int => StaticInit::Int(0),
                Type::Long => StaticInit::Long(0),
                _ => continue,
            };
            let init = init.get_static_init().unwrap_or(tentative_init);
            let name = symbol.to_owned();
            let global = entry.is_global();
            let staticvar = StaticVariable {
                name,
                global,
                init,
                var_type: sym_type,
            };
            defs.push(TopLevelItem::Var(staticvar));
        }
    }
    defs
}

#[allow(clippy::module_name_repetitions)]
pub fn emit_tacky(input: Ast) -> TAst {
    let Ast { declarations } = input;
    let mut toplevel_items: Vec<TopLevelItem> = declarations
        .into_iter()
        .filter_map(emit_toplevel_dec)
        .collect();

    let mut static_symbols = emit_static_symbols();
    toplevel_items.append(&mut static_symbols);
    TAst { toplevel_items }
}
