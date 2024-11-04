use crate::ast::Type;
use crate::codegen::AsmType;
use crate::semantic_analysis::SYM_TABLE;

use std::collections::HashMap;
use std::sync::OnceLock;

pub static ASM_SYM_TABLE: GlobalAsmSymTable = GlobalAsmSymTable::new();
pub struct GlobalAsmSymTable {
    inner: OnceLock<AsmSymTable>,
}

impl GlobalAsmSymTable {
    const fn new() -> Self {
        Self {
            inner: OnceLock::new(),
        }
    }

    pub(super) fn init(&self) {
        let names = SYM_TABLE.get_keys();
        let mut result = HashMap::with_capacity(names.len());
        for name in names {
            let entry = SYM_TABLE.get_symbol(&name).unwrap();
            let is_static = entry.attrs.is_static();
            let asm_entry = match entry.sym_type {
                Type::Int => AsmSymTabEntry::Obj {
                    asm_type: AsmType::Longword,
                    is_static,
                },
                Type::Long => AsmSymTabEntry::Obj {
                    asm_type: AsmType::Quadword,
                    is_static,
                },
                Type::Fun { .. } => {
                    let is_defined = entry.attrs.is_fun_defined();
                    AsmSymTabEntry::Fun { is_defined }
                }
            };
            result.insert(name, asm_entry);
        }
        self.inner.set(result).expect("Should not be initialized");
    }

    pub fn get_sym(&self, name: &str) -> Option<AsmSymTabEntry> {
        self.inner
            .get()
            .expect("Should be initialized")
            .get(name)
            .cloned()
    }

    pub fn is_sym_static(&self, name: &str) -> bool {
        self.inner
            .get()
            .expect("Should be initialized")
            .get(name)
            .filter(|sym| sym.is_static())
            .is_some()
    }

    pub fn get_type(&self, name: &str) -> Option<AsmType> {
        self.inner
            .get()
            .expect("Should be initialized")
            .get(name)
            .and_then(AsmSymTabEntry::get_type)
    }
}

type AsmSymTable = HashMap<String, AsmSymTabEntry>;

#[derive(Debug, Clone)]
pub enum AsmSymTabEntry {
    Obj { asm_type: AsmType, is_static: bool },
    Fun { is_defined: bool },
}

impl AsmSymTabEntry {
    pub fn is_static(&self) -> bool {
        match self {
            Self::Obj { is_static, .. } => *is_static,
            Self::Fun { .. } => false,
        }
    }

    pub fn get_type(&self) -> Option<AsmType> {
        match self {
            Self::Obj { asm_type, .. } => Some(*asm_type),
            Self::Fun { .. } => None,
        }
    }

    pub fn get_size(&self) -> Option<i64> {
        match self {
            Self::Obj { asm_type, .. } => match asm_type {
                AsmType::Longword => Some(4),
                AsmType::Quadword => Some(8),
            },
            Self::Fun { .. } => None,
        }
    }
}
