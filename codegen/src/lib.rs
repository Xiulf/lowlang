use libloading::{Library, Symbol};
pub use target_lexicon::Triple;
pub use tempfile::NamedTempFile;

pub enum Backend {
    Llvm,
    Clif,
}

pub fn compile_module(db: &dyn ir::db::IrDatabase, ir: &ir::Module, backend: Backend) -> NamedTempFile {
    let mut dir = std::env::current_exe().unwrap().parent().unwrap().to_path_buf();

    if cfg!(target_os = "windows") {
		match backend {
			| Backend::Llvm => dir.push("backend_llvm.dll"),
			| Backend::Clif => dir.push("backend_clif.dll"),
		}
    } else {
		match backend {
			| Backend::Llvm => dir.push("libbackend_llvm.so"),
			| Backend::Clif => dir.push("libbackend_clif.so"),
		}
    }

    let mut object_file = NamedTempFile::new().unwrap();

    unsafe {
        let lib = Library::new(dir).unwrap();
        let symbol: Symbol<fn(&dyn ir::db::IrDatabase, &ir::Module, &mut NamedTempFile)> = lib.get(b"compile_module").unwrap();

        symbol(db, ir, &mut object_file);
    }

    object_file
}
