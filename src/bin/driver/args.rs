use std::env;
use std::path::PathBuf;
use std::process::exit;

#[allow(clippy::struct_excessive_bools)]
#[derive(Default)]
pub struct Args {
    #[cfg(feature = "lexer")]
    pub lex: bool,

    #[cfg(feature = "parser")]
    pub parse: bool,

    #[cfg(feature = "tacky")]
    pub tacky: bool,

    #[cfg(feature = "codegen")]
    pub codegen: bool,

    #[cfg(feature = "semantic_analysis")]
    pub validate: bool,

    #[cfg(feature = "emission")]
    pub no_link: bool,
    #[cfg(feature = "emission")]
    pub no_assemble: bool,

    #[cfg(feature = "emission")]
    pub link_options: Vec<String>,

    pub input: PathBuf,
}

impl Args {
    pub fn parse() -> Self {
        let env_args = env::args();
        let mut args = Self::default();
        let mut input_file = None;

        for arg in env_args.skip(1) {
            match arg.as_str() {
                #[cfg(feature = "emission")]
                s if s.starts_with("-l") => {
                    args.link_options.push(s.to_string());
                }
                #[cfg(feature = "emission")]
                "-c" | "--no-link" => args.no_link = true,
                #[cfg(feature = "emission")]
                "-S" | "--no-assemble" => args.no_assemble = true,
                #[cfg(feature = "lexer")]
                "--lex" => args.lex = true,
                #[cfg(feature = "parser")]
                "--parse" => args.parse = true,
                #[cfg(feature = "tacky")]
                "--tacky" => args.tacky = true,
                #[cfg(feature = "codegen")]
                "--codegen" => args.codegen = true,
                #[cfg(feature = "semantic_analysis")]
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

    fn usage() -> ! {
        let cmd0 = std::env::args().next().unwrap_or("drirver".to_owned());
        let usage_msg = format!("Usage: {cmd0} [OPTIONS] FILE\n");
        let options = [
            "Options:\n",
            "  -h, --help             Show this message\n",
            #[cfg(feature = "lexer")]
            "      --lex              Stop after lexing\n",
            #[cfg(feature = "parser")]
            "      --parse            Stop after parsing\n",
            #[cfg(feature = "semantic_analysis")]
            "      --validate         Stop after semantic analysis\n",
            #[cfg(feature = "tacky")]
            "      --tacky            Stop after producing TAC IR\n",
            #[cfg(feature = "codegen")]
            "      --codegen          Stop after code generation\n",
            #[cfg(feature = "emission")]
            " -c, --no-link           Compile and assemble, but do not link\n",
            #[cfg(feature = "emission")]
            "-l<lib>                  Link with <lib>",
            #[cfg(feature = "emission")]
            "                         (Output object file)\n",
            #[cfg(feature = "emission")]
            " -S, --no-assemble       Compile only; do not assemble or link\n",
            #[cfg(feature = "emission")]
            "                         (Output assembly file)\n",
        ];

        print!("Nameless c compiler\n\n{usage_msg}\n");
        options.into_iter().for_each(|o| print!("{o}"));

        exit(0)
    }
}
