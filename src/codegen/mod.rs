pub mod asm_ast;
mod asm_sym_table;
mod fix;
mod gen;
mod stack;

use crate::tacky::*;

pub use asm_ast::*;
pub use asm_sym_table::ASM_SYM_TABLE;

use gen::gen_toplevel_item;

pub fn codegen(ast: TAst) -> AsmAst {
    let TAst { toplevel_items } = ast;
    ASM_SYM_TABLE.init();
    let asm_toplevel_items = toplevel_items.into_iter().map(gen_toplevel_item).collect();

    AsmAst { asm_toplevel_items }
}
