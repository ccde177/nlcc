//! Assembly generation
mod asm_ast;
#[macro_use]
mod asm_macro;
mod asm_sym_table;
mod fix;
mod gen;
mod stack;

use crate::tacky::*;

pub use asm_ast::*;
pub use asm_sym_table::{AsmSymTabEntry, GlobalAsmSymTable, ASM_SYM_TABLE};

use gen::{gen_toplevel_item, CONST_TABLE};

pub fn codegen(ast: TAst) -> AsmAst {
    let TAst { toplevel_items } = ast;
    ASM_SYM_TABLE.init();
    let mut asm_toplevel_items = toplevel_items
        .into_iter()
        .map(gen_toplevel_item)
        .collect::<Vec<_>>();

    // Can only be done after processing every toplevel item
    let mut constants = CONST_TABLE.collect();
    asm_toplevel_items.append(&mut constants);

    AsmAst { asm_toplevel_items }
}
