#[derive(Debug)]
pub enum DriverError {
    InputFileDoesNotExists(String),
    PreprocessorFailed,
    AssemblerFailed,
}

impl std::fmt::Display for DriverError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::InputFileDoesNotExists(name) => write!(f, "File {name} does not exist"),
            Self::PreprocessorFailed => write!(f, "Failed to run preprocessor"),
            Self::AssemblerFailed => write!(f, "Failed to run assembler"),
        }
    }
}

impl std::error::Error for DriverError {}
