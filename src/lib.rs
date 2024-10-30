#![deny(unused_must_use)]
#![warn(clippy::pedantic)]
#![allow(clippy::wildcard_imports)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::module_name_repetitions)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::must_use_candidate)]

pub mod ast;
#[cfg(feature = "codegen")]
pub mod codegen;
#[cfg(feature = "emission")]
pub mod emission;
#[cfg(feature = "lexer")]
pub mod lexer;
#[cfg(feature = "parser")]
pub mod parser;
#[cfg(feature = "semantic_analysis")]
pub mod semantic_analysis;
#[cfg(feature = "tacky")]
pub mod tacky;
