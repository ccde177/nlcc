use crate::ast::{AstConst, Type};
use crate::codegen::AsmType;
use crate::semantic_analysis::SYM_TABLE;

use std::collections::HashMap;
use std::sync::{OnceLock, RwLock};

pub static ASM_SYM_TABLE: GlobalAsmSymTable = GlobalAsmSymTable::new();
pub struct GlobalAsmSymTable {
    inner: OnceLock<RwLock<AsmSymTable>>,
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
                Type::Double => AsmSymTabEntry::Obj {
                    asm_type: AsmType::Double,
                    is_static,
                    is_const: false,
                },
                Type::Int | Type::UInt => AsmSymTabEntry::Obj {
                    asm_type: AsmType::Longword,
                    is_static,
                    is_const: false,
                },
                Type::Long | Type::ULong => AsmSymTabEntry::Obj {
                    asm_type: AsmType::Quadword,
                    is_static,
                    is_const: false,
                },
                Type::Fun { .. } => {
                    let is_defined = entry.attrs.is_fun_defined();
                    AsmSymTabEntry::Fun { is_defined }
                }
            };
            result.insert(name, asm_entry);
        }
        self.inner
            .set(RwLock::new(result))
            .expect("Should not be initialized");
    }

    pub fn get_sym(&self, name: &str) -> Option<AsmSymTabEntry> {
        self.inner
            .get()
            .expect("Should be initialized")
            .read()
            .expect("Should not be poisoned")
            .get(name)
            .cloned()
    }

    pub fn is_sym_static(&self, name: &str) -> bool {
        self.inner
            .get()
            .expect("Should be initialized")
            .read()
            .expect("Should not be poisoned")
            .get(name)
            .filter(|sym| sym.is_static())
            .is_some()
    }

    pub fn add_static_const(&self, name: String, value: AstConst) {
        let asm_type = match value.get_type() {
            Type::Long | Type::ULong => AsmType::Quadword,
            Type::Int | Type::UInt => AsmType::Longword,
            Type::Double => AsmType::Double,
            _ => unreachable!("Const can't have type of function"),
        };

        let entry = AsmSymTabEntry::Obj {
            asm_type,
            is_static: true,
            is_const: true,
        };

        self.inner
            .get()
            .expect("Should be initialized")
            .write()
            .expect("Should not be poisoned")
            .insert(name, entry);
    }

    pub fn get_type(&self, name: &str) -> Option<AsmType> {
        self.inner
            .get()
            .expect("Should be initialized")
            .read()
            .expect("Should not be poisoned")
            .get(name)
            .and_then(AsmSymTabEntry::get_type)
    }
}

type AsmSymTable = HashMap<String, AsmSymTabEntry>;

#[derive(Debug, Clone)]
pub enum AsmSymTabEntry {
    Obj {
        asm_type: AsmType,
        is_static: bool,
        is_const: bool,
    },
    Fun {
        is_defined: bool,
    },
}

impl AsmSymTabEntry {
    pub fn is_const(&self) -> bool {
        matches!(self, Self::Obj { is_const: true, .. })
    }
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
                AsmType::Double => Some(8),
            },
            Self::Fun { .. } => None,
        }
    }
}
