use libloading::{Library, Symbol};
pub use target_lexicon::Triple;
pub use tempfile::NamedTempFile;

pub enum Backend {
    Llvm,
}

pub fn compile_module(ir: &ir::Module, target: Triple, backend: Backend) -> NamedTempFile {
    let mut dir = std::env::current_exe().unwrap().parent().unwrap().to_path_buf();

    match backend {
        | Backend::Llvm => dir.push("libbackend_llvm.so"),
    }

    let mut object_file = NamedTempFile::new().unwrap();

    unsafe {
        let lib = Library::new(dir).unwrap();
        let symbol: Symbol<fn(&ir::Module, Triple, &mut NamedTempFile)> = lib.get(b"compile_module").unwrap();

        symbol(ir, target, &mut object_file);
    }

    object_file
}
