use clap::Parser;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

mod codegen;
mod emission;
mod lexer;
mod parser;
mod semantical_analysis;
mod tacky;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(long)]
    lex: bool,

    #[arg(long)]
    parse: bool,

    #[arg(long)]
    codegen: bool,

    #[arg(long)]
    tacky: bool,

    #[arg(long)]
    validate: bool,

    input: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let mut preprocessed = args.input.clone();
    preprocessed.set_extension("i");

    let status = Command::new("gcc")
        .arg("-E")
        .arg("-P")
        .arg(args.input.clone())
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

    let validated_ast = semantical_analysis::validate(ast)?;

    if args.validate {
        return Ok(());
    }

    let tacky = tacky::emit_tacky(validated_ast.clone());

    if args.tacky {
        dbg!(tacky);
        return Ok(());
    }

    let asm = codegen::codegen(tacky);

    if args.codegen {
        dbg!(asm);
        return Ok(());
    }

    let mut asm_file = args.input.clone();
    asm_file.set_extension("s");
    fs::write(&asm_file, asm.to_string())?;

    let mut out_file = args.input.clone();
    out_file.set_extension("");
    let status = Command::new("gcc")
        .arg(&asm_file)
        .arg("-o")
        .arg(&out_file)
        .status()?;
    if !status.success() {
        return Err(anyhow::anyhow!("Failed to run assembler"));
    }
    fs::remove_file(asm_file)?;

    Ok(())
}
