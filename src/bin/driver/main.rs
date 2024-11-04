mod args;
mod driver_error;

use args::Args;
use driver_error::{DriverError, Result};
#[allow(unused_imports)]
use nlcc::ast::Ast;
use nlcc::*;
use std::fs;
use std::path::PathBuf;
use std::process::exit;
use std::process::Command;

fn preprocess(args: &Args) -> Result<PathBuf> {
    let mut preprocessed = args.input.clone();
    preprocessed.set_extension("i");

    let status = Command::new("gcc")
        .arg("-E")
        .arg("-P")
        .arg(&args.input)
        .arg("-o")
        .arg(&preprocessed)
        .status()
        .map_err(DriverError::from)?;
    if !status.success() {
        let err = DriverError::PreprocessorFailed;
        Err(err)?;
    }
    Ok(preprocessed)
}

#[cfg(feature = "lexer")]
fn tokenize(preprocessed: PathBuf, args: &Args) -> Result<Vec<lexer::Token>> {
    let source = std::fs::read_to_string(&preprocessed).expect("Can't open preprocessed file");

    let tokens = lexer::lex(&source)?;

    fs::remove_file(&preprocessed).map_err(DriverError::from)?;

    if args.lex {
        dbg!(&tokens);
        exit(0)
    }
    Ok(tokens)
}

#[cfg(feature = "parser")]
fn parse(tokens: &[lexer::Token], args: &Args) -> Result<Ast> {
    let ast = parser::parse(tokens)?;
    if args.parse {
        dbg!(&ast);
        exit(0);
    }
    Ok(ast)
}

#[cfg(feature = "semantic_analysis")]
fn validate(ast: ast::Ast, args: &Args) -> Result<Ast> {
    let validated_ast = semantic_analysis::validate(ast)?;
    if args.validate {
        dbg!(&validated_ast);
        exit(0)
    }
    Ok(validated_ast)
}

#[cfg(feature = "tacky")]
fn gen_tacky(ast: ast::Ast, args: &Args) -> tacky::TAst {
    let tacky = tacky::emit_tacky(ast);
    if args.tacky {
        dbg!(&tacky);
        exit(0);
    }
    tacky
}

#[cfg(feature = "codegen")]
fn gen_asm(tacky: tacky::TAst, args: &Args) -> codegen::AsmAst {
    let asm_ast = codegen::codegen(tacky);
    if args.codegen {
        dbg!(&asm_ast);
        exit(0);
    }
    asm_ast
}

#[cfg(feature = "emission")]
fn emit_asm(asm_ast: codegen::AsmAst, args: &Args) -> Result<()> {
    let mut asm_file = args.input.clone();
    asm_file.set_extension("s");
    fs::write(&asm_file, asm_ast.to_string())?;

    if args.no_assemble {
        exit(0);
    }

    // -pie is used here as a dummy value
    let c_arg = if args.no_link { "-c" } else { "-pie" };
    let out_extension = if args.no_link { "o" } else { "" };
    let mut out_file = args.input.clone();
    out_file.set_extension(out_extension);

    let status = Command::new("gcc")
        .arg(&asm_file)
        .arg(c_arg)
        .arg("-o")
        .arg(&out_file)
        .status()?;

    if !status.success() {
        let err = DriverError::AssemblerFailed;
        Err(err)?;
    }
    fs::remove_file(asm_file)?;

    Ok(())
}

#[allow(unused_variables)]
pub fn main() -> Result<()> {
    let args = Args::parse();

    let file_exists = fs::exists(&args.input)?;
    if !file_exists {
        let filename = args.input.to_string_lossy().to_string();
        let err = DriverError::InputFileDoesNotExist(filename);
        return Err(err);
    }

    let preprocessed = preprocess(&args)?;

    #[cfg(feature = "lexer")]
    let tokens = tokenize(preprocessed, &args)?;

    #[cfg(feature = "parser")]
    let ast = parse(&tokens, &args)?;

    #[cfg(feature = "semantic_analysis")]
    let validated_ast = validate(ast, &args)?;

    #[cfg(feature = "tacky")]
    let tacky = gen_tacky(validated_ast, &args);

    #[cfg(feature = "codegen")]
    let asm_ast = gen_asm(tacky, &args);

    #[cfg(feature = "emission")]
    emit_asm(asm_ast, &args)?;

    Ok(())
}
