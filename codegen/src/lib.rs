pub use target_lexicon::Triple;
pub use tempfile::NamedTempFile;

pub enum Backend {
    Llvm,
    Clif,
}

pub fn compile_module(db: &dyn ir::db::IrDatabase, ir: &ir::Module, backend: Backend) -> NamedTempFile {
    let codegen = match backend {
        | Backend::Llvm => todo!(),
        | Backend::Clif => backend_clif::compile_module,
    };

    let mut object_file = NamedTempFile::new().unwrap();

    codegen(db, ir, &mut object_file);

    object_file
}
