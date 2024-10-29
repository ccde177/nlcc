#![deny(unused_must_use)]
#![warn(clippy::pedantic)]
#![allow(clippy::wildcard_imports)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::module_name_repetitions)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::must_use_candidate)]

pub mod ast;
pub mod codegen;
pub mod emission;
pub mod lexer;
pub mod parser;
pub mod semantic_analysis;
pub mod tacky;
