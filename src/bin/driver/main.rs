mod args;
mod driver_error;

use args::Args;
use driver_error::DriverError;
use nlcc::*;
use std::path::PathBuf;

use std::fs;
use std::process::Command;

type BoxedError = Box<dyn std::error::Error>;

fn preprocess(args: &Args) -> Result<PathBuf, BoxedError> {
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
        let err = DriverError::PreprocessorFailed;
        Err(err)?;
    }
    Ok(preprocessed)
}

#[cfg(feature = "lexer")]
fn tokenize(preprocessed: PathBuf, args: &Args) -> Result<Vec<lexer::Token>, BoxedError> {
    let source = std::fs::read_to_string(&preprocessed).expect("Can't open preprocessed file");

    let tokens = lexer::lex(&source)?;

    fs::remove_file(&preprocessed)?;

    if args.lex {
        dbg!(&tokens);
    }
    Ok(tokens)
}

#[cfg(feature = "parser")]
fn parse(tokens: &[lexer::Token], args: &Args) -> Result<ast::Ast, BoxedError> {
    let ast = parser::parse(tokens)?;
    if args.parse {
        dbg!(&ast);
    }
    Ok(ast)
}

#[cfg(feature = "semantic_analysis")]
fn validate(ast: ast::Ast, args: &Args) -> Result<ast::Ast, BoxedError> {
    let validated_ast = semantic_analysis::validate(ast)?;
    if args.validate {
        dbg!(&validated_ast);
    }
    Ok(validated_ast)
}

#[cfg(feature = "tacky")]
fn gen_tacky(ast: ast::Ast, args: &Args) -> Result<tacky::TAst, BoxedError> {
    let tacky = tacky::emit_tacky(validated_ast);
    if args.tacky {
        dbg!(&tacky)
    }
    Ok(tacky)
}

#[cfg(feature = "codegen")]
fn gen_asm(tacky: tacky::TAst, args: &Args) -> codegen::AsmAst {
    let asm_ast = codegen::codegen(tacky);
    if args.codegen {
        dbg!(&asm_ast);
    }
    asm_ast
}

#[cfg(feature = "emission")]
fn emit_asm(asm_ast: codegen::AsmAst, args: &Args) -> Result<(), BoxedError> {
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
        let err = DriverError::AssemblerFailed;
        Err(err)?;
    }
    fs::remove_file(asm_file)?;

    Ok(())
}

#[allow(unused_variables)]
pub fn main() -> Result<(), BoxedError> {
    let args = Args::parse();

    let file_exists = fs::exists(&args.input)?;
    if !file_exists {
        let filename = args.input.to_string_lossy().to_string();
        let err = DriverError::InputFileDoesNotExist(filename);
        Err(err)?;
    }
    let preprocessed = preprocess(&args)?;
    #[cfg(feature = "lexer")]
    let tokens = tokenize(preprocessed, &args)?;

    if args.lex {
        return Ok(());
    }

    #[cfg(feature = "parser")]
    let ast = parse(&tokens, &args)?;

    if args.parse {
        return Ok(());
    }

    #[cfg(feature = "semantic_analysis")]
    let validated_ast = vlidate(ast, &args)?;

    if args.validate {
        return Ok(());
    }

    #[cfg(feature = "tacky")]
    let tacky = gen_tacky(validated_ast, &args)?;

    if args.tacky {
        return Ok(());
    }

    #[cfg(feature = "codegen")]
    let asm_ast = gen_asm(tacky, &args);

    if args.codegen {
        return Ok(());
    }

    #[cfg(feature = "emission")]
    emit_asm(asm_ast, &args);

    Ok(())
}
