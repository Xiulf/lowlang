use std::path::Path;
use tempfile::NamedTempFile;

pub enum Backend {
    Clif,
    Llvm,
}

pub struct Assembly {
    file: NamedTempFile,
}

impl Assembly {
    pub fn path(&self) -> &Path {
        self.file.path()
    }
}

pub fn run(module: &ir::Module, backend: Backend) -> Assembly {
    #[cfg(debug_assertions)]
    let path = match backend {
        | Backend::Clif => "target/debug/libcodegen_clif.so",
        | Backend::Llvm => "target/debug/libcodegen_llvm.so",
    };

    #[cfg(not(debug_assertions))]
    let path = match backend {
        | Backend::Clif => "target/release/libcodegen_clif.so",
        | Backend::Llvm => "target/release/libcodegen_llvm.so",
    };

    unsafe {
        let lib = libloading::Library::new(path).unwrap();
        let func: libloading::Symbol<extern "C" fn(&ir::Module) -> Assembly> = lib.get(b"run_codegen").unwrap();

        func(module)
    }
}
