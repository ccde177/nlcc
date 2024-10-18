use std::env;
use std::path::PathBuf;
use std::process::exit;

#[allow(clippy::struct_excessive_bools)]
pub struct Args {
    pub lex: bool,
    pub parse: bool,
    pub tacky: bool,
    pub codegen: bool,
    pub validate: bool,
    pub no_link: bool,
    pub no_assemble: bool,
    pub input: PathBuf,
}

impl Args {
    pub fn parse() -> Self {
        let args = env::args();

        let mut lex = false;
        let mut parse = false;
        let mut tacky = false;
        let mut codegen = false;
        let mut validate = false;
        let mut no_link = false;
        let mut no_assemble = false;
        let mut input = None;

        for arg in args.skip(1) {
            match arg.as_str() {
                "-c" | "--no-link" => no_link = true,
                "-S" | "--no-assemble" => no_assemble = true,
                "--lex" => lex = true,
                "--parse" => parse = true,
                "--tacky" => tacky = true,
                "--codegen" => codegen = true,
                "--validate" => validate = true,
                "-h" | "--help" => Self::usage(),
                _ => {
                    if input.is_some() {
                        Self::usage();
                    }
                    let file = PathBuf::from(arg);
                    input = Some(file);
                }
            }
        }
        let input = input.unwrap_or_else(|| Self::usage());
        Self {
            lex,
            parse,
            tacky,
            codegen,
            validate,
            no_link,
            no_assemble,
            input,
        }
    }

    #[allow(clippy::items_after_statements)]
    fn usage() -> ! {
        let cmd0 = std::env::args().next().unwrap_or("drirver".to_owned());
        let usage_msg = format!("Usage: {cmd0} [OPTIONS] FILE\n");
        static OPTIONS: &str = concat!(
            "Options:\n",
            "  -h, --help             Show this message\n",
            "      --lex              Stop after lexing\n",
            "      --parse            Stop after parsing\n",
            "      --validate         Stop after semantic analysis\n",
            "      --tacky            Stop after producing TAC IR\n",
            "      --codegen          Stop after code generation\n",
            " -c, --no-link           Compile and assemble, but do not link\n",
            "                         (Output object file)\n",
            " -S, --no-assemble       Compile only; do not assemble or link\n",
            "                         (Output assembly file)\n",
        );
        print!("Nameless c compiler\n\n{usage_msg}\n{OPTIONS}");
        exit(0)
    }
}
