/*!
This crate is an attempt to build a C compiler following incremental approach from [Writing a C Compiler] book by Nora Sandler.
This project follows this ideas and goals:
  * **No sh drivers** - Driver must be written in Rust.
  * **No external dependencies** - Do not use any kind of libraries for command-line argument parsing, error handling and etc. All these features are really easy to implement thanks to featureful Rust's [std]. This rule also applies to regex, so unlike in the book, the lexer is handwritten. The only kind of dependency which can be possibly added in the future is Dev-only dependencies.
  * **No unstable features** - Project should be compilable with stable Rust toolchain.
  * **Reusable modules** - Every module except for [ast] is hidden by feature flag with the same name. This allows to reuse any module from this crate as a library (e.g. for lexing or parsing a C code).
  ## Command-line options
  ```shell
Usage: nlcc [OPTIONS] FILE

Options:
  -h, --help             Show this message
      --lex              Stop after lexing
      --parse            Stop after parsing
      --validate         Stop after semantic analysis
      --tacky            Stop after producing TAC IR
      --codegen          Stop after code generation
 -c, --no-link          Compile and assemble, but do not link
                         (Output object file)
 -S, --no-assemble      Compile only; do not assemble or link
                         (Output assembly file)
  ```
## Supported targets
  * **x86_64-linux-gnu** - currently only supported target. There is plans for adding AArch64 and MacOS support.

## What is implemented
  * **Chapter 1** - Return statement, top-level main function and numeric constants.
  * **Chatper 2** - Unary operators on numeric constants: `-` and `~`.
  * **Chatper 3** - Binary operators on numeric constants: `+`, `-`, `*`, `/`, `%`, `&`, `|` and `^`. Support for nested sub-expressions with paranthesis.
  * **Chapter 4** - Logical operators: `!`, `&&`, `||`. Relational operators: `<`, `>`, `==`, `!=`, `<=` and `>=`.
  * **Chapter 5** - Support for local variables of type int. Compound assignment operators: `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `<<=` and `>>=`. Prefix and postfix Increment(`++`) and Decrement(`--`).
  * **Chapter 6** - If statments and condtional(ternary) expressions. Goto statement and Labels.
  * **Chapter 7** - Compound statements(blocks) which are formed with curly brackets {}.
  * **Chapter 8** - For, while and do-while loops. Switch, break and continue statements.
  * **Chapter 9** - File-scoped functions(declarations and definitions).
  * **Chapter 10** - File-scoped variable declarations, static variables, extern variables. Extern and static functions.
  * **Chapter 11** - Long type and long numeric constants (e.g. `100l`).
  * **Chapter 12** - Unsigned int and unsigned long types and correlating numeric constant types (e.g. `100u` and `100ul`).
  * **Chapter 13** - Double-precision floating point type `double`. `NaN` values handling.

## Compilation stages
1. **Preprocessing** - Driver uses gcc to preproccess input file and writes the result to file with .i extension.
2. **Tokenization(lexing)** - [lex](lexer::lex) scans preprocessed file for known token types and produses [Tokens](lexer::Tokens) - a collection of [LinedToken](lexer::LinedToken) - a structure which contains token type and its position in preprocessed file. When unknown token met(e.g. `1@2`,  `wHile`, ..) it produces [LexError](lexer::LexError). Driver stops after this stage if --lex argument is provided.
3. **Parsing** - [parse](parser::parse) takes a collection of tokens and produses [Ast](ast::Ast) - abstract syntax tree of input file. Any syntactic error(e.g. `int a = if`, ..) will raise [ParseError](parser::ParseError). Driver stops after this stage if  --parse argument is provided.
4. **Validation** - [validate](semantic_analysis::validate) takes in [Ast](ast::Ast) and after number of checks and passes produces new [Ast](ast::Ast) where each [Exp](ast::Exp) has [Type](ast::Type) attached to it. This stage also checks that `goto` labels are in current function's scope, every variable has declaration, no conflicting redeclarations and provides [SYM_TABLE](semantic_analysis::SYM_TABLE) - global symbol table which contains every symbol's type information. Any kind of semantic error will produce [SemAnalysisError](semantic_analysis::SemAnalysisError). Driver stops after this stage if --validation argument is provided.
5. **Tacky** - [emit_tacky](tacky::emit_tacky) takes typed [Ast](ast::Ast) and produces [Three-Address Code] intermidiate representation [TAst](tacky::TAst). From this stage on any error means bug in compiler and raises [panic!] instead of producing typed error. Driver stops after this stage if --tacky argument is provided.
6. **Codegen** - [codegen] takes in [TAst](tacky::TAst) and produces assembly AST [AsmAst](codegen::AsmAst) - intremidiate representation of compiled compilation unit ready to be emited as an object file. Driver stops after this stage if --codegen argument is provided.
7. **Emission** - [emission] module provides [Display] trait implementation for [AsmAst](codegen::AsmAst) which allows to [write!] it to any target. Driver emits assembly file with .s extension and stops if -S argument is provided.
8. **Assembly** - Driver uses GNU Assembly to compile assembly file to object file with an .o extension. Stops if -c argument is provided.
9. **Linkage** - Driver uses gcc linker to link object file and produce [ELF] executable.

## Versioning
Starting from 0.12.0 following versioning rules are applied:
0.X.Y where X is a corelating chapter number and Y is a number of implemented extra features from the book.

[Writing a C Compiler]: https://nostarch.com/writing-c-compiler
[std]: https://doc.rust-lang.org/std/
[function declaration]: ast::FunDec
[Three-Address Code]: https://en.wikipedia.org/wiki/Three-address_code
[panic!]: https://doc.rust-lang.org/std/macro.panic.html
[write!]: https://doc.rust-lang.org/std/macro.write.html
[ELF]: https://en.wikipedia.org/wiki/Executable_and_Linkable_Format
[Display]: https://doc.rust-lang.org/std/fmt/trait.Display.html
*/

#![deny(unused_must_use)]
#![allow(unused_macros)]
#![warn(clippy::pedantic)]
#![allow(clippy::wildcard_imports)]
#![allow(clippy::module_name_repetitions)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::must_use_candidate)]
#![allow(clippy::return_self_not_must_use)]

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
