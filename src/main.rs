#![warn(clippy::pedantic)]
#![allow(clippy::wildcard_imports)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::module_name_repetitions)]

mod args;
mod ast;
mod codegen;
mod emission;
mod lexer;
mod parser;
mod semantic_analysis;
mod tacky;

use args::Args;

use std::fs;
use std::process::Command;

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let mut preprocessed = args.input.clone();
    preprocessed.set_extension("i");

    let status = Command::new("gcc")
        .arg("-E")
        .arg("-P")
        .arg(&args.input)
        .arg("-o")
        .arg(&preprocessed)
        .status()?;
    if !status.success() {
        return Err(anyhow::anyhow!("Failed to run preprocessor"));
    }

    let source = std::fs::read_to_string(&preprocessed).expect("Can't open preprocessed file");
    let tokens = lexer::lex(source)?;
    fs::remove_file(&preprocessed)?;

    if args.lex {
        dbg!(tokens);
        return Ok(());
    }

    let ast = parser::parse(tokens)?;

    if args.parse {
        dbg!(ast);
        return Ok(());
    }

    let validated_ast = semantic_analysis::validate(ast)?;

    if args.validate {
        dbg!(validated_ast);
        return Ok(());
    }

    let tacky = tacky::emit_tacky(validated_ast.clone());

    if args.tacky {
        dbg!(tacky);
        return Ok(());
    }

    let asm_ast = codegen::codegen(tacky);

    if args.codegen {
        dbg!(asm_ast);
        return Ok(());
    }

    let mut asm_file = args.input.clone();
    asm_file.set_extension("s");
    fs::write(&asm_file, asm_ast.to_string())?;

    if args.no_assemble {
        return Ok(());
    }

    // -pie is used here as a dummy value
    let c_arg = if args.no_link { "-c" } else { "-pie" };
    let out_extension = if args.no_link { "o" } else { "" };
    let mut out_file = args.input;
    out_file.set_extension(out_extension);

    let status = Command::new("gcc")
        .arg(&asm_file)
        .arg(c_arg)
        .arg("-o")
        .arg(&out_file)
        .status()?;

    if !status.success() {
        return Err(anyhow::anyhow!("Failed to run assembler"));
    }

    fs::remove_file(asm_file)?;

    Ok(())
}
