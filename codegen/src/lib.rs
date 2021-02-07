pub enum Backend {
    Clif,
    Llvm,
}

pub fn run(module: &ir::Module, backend: Backend) {
    let path = match backend {
        | Backend::Clif => "target/debug/libcodegen_clif.so",
        | Backend::Llvm => "target/debug/libcodegen_llvm.so",
    };

    unsafe {
        let lib = libloading::Library::new(path).unwrap();
        let func: libloading::Symbol<extern "C" fn(&ir::Module)> = lib.get(b"__codegen_backend").unwrap();

        func(module)
    }
}
