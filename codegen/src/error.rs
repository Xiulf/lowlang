pub enum Error {
    Codegen(cranelift_module::ModuleError),
}

impl From<cranelift_module::ModuleError> for Error {
    fn from(src: cranelift_module::ModuleError) -> Error {
        Error::Codegen(src)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Error::Codegen(e) => match e {
                cranelift_module::ModuleError::Compilation(e2) => {
                    write!(f, "Compilation error: ")?;

                    match e2 {
                        cranelift_codegen::CodegenError::Verifier(e) => write!(f, "\n{}", e),
                        _ => write!(f, "{}", e),
                    }
                },
                _ => write!(f, "{}", e),
            },
        }
    }
}
