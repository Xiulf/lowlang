#[cfg(feature = "cranelift")]
pub fn assemble(
    module: &ir::Module,
    target: target_lexicon::Triple,
) -> codegen::obj_file::ObjectFile {
    let backend = codegen_cranelift::ClifBackend::new();
    let mcx = codegen::ModuleCtx::new(module, target, backend);

    mcx.build()
}
