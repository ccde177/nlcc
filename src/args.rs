use std::env;
use std::path::PathBuf;
use std::process::exit;

#[allow(clippy::struct_excessive_bools)]
#[derive(Default)]
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
        let env_args = env::args();
        let mut args = Self::default();
        let mut input_file = None;

        for arg in env_args.skip(1) {
            match arg.as_str() {
                "-c" | "--no-link" => args.no_link = true,
                "-S" | "--no-assemble" => args.no_assemble = true,
                "--lex" => args.lex = true,
                "--parse" => args.parse = true,
                "--tacky" => args.tacky = true,
                "--codegen" => args.codegen = true,
                "--validate" => args.validate = true,
                "-h" | "--help" => Self::usage(),
                _ => {
                    if input_file.is_some() {
                        Self::usage();
                    }
                    let file = PathBuf::from(arg);
                    input_file = Some(file);
                }
            }
        }
        args.input = input_file.unwrap_or_else(|| Self::usage());
        args
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
