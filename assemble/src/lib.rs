#[cfg(feature = "cranelift")]
pub fn assemble(module: &ir::Module, target: target_lexicon::Triple) {
    let backend = codegen_cranelift::ClifBackend::new();
    let mcx = codegen::ModuleCtx::new(module, target, backend);
    let obj_file = mcx.build();

    obj_file.copy(&std::path::PathBuf::from("test.o"));
}
