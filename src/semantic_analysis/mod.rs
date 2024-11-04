mod case_collection;
mod goto;
mod loop_labeling;
mod name_resolution;
mod semanalysis_error;
mod typecheck;

use crate::ast::*;
use case_collection::collect_cases;
use goto::ensure_goto_correctness;
use loop_labeling::label_loops;
use name_resolution::name_resolution;
use typecheck::check_types;

pub use semanalysis_error::{Result, SemAnalysisError};
pub use typecheck::{StaticInit, SYM_TABLE};

// Order of stages must be:
// name_resolution > [goto >] check_types >
// .. > label_loops > .. > collect_cases > ..
pub fn validate(ast: Ast) -> Result<Ast> {
    let mut resolved = name_resolution(ast)?;
    ensure_goto_correctness(&mut resolved)?;

    check_types(resolved)
        .and_then(label_loops)
        .and_then(collect_cases)
}
