fn main() {
    let reporter = diagnostics::Reporter::new();
    let filemap = diagnostics::Filemap::new();
    let mut source = diagnostics::SourceFile::load("test/main.ll");
    
    filemap.add(&mut source);
    
    let ast = syntax::parse::<syntax::Program>(&source, &reporter, None);
    
    reporter.report(&filemap, true);
    
    let mut type_ctx = check::ctx::TypeCtx::new(&reporter);
    
    check::typecheck::typecheck(&mut type_ctx, &ast);
    
    reporter.report(&filemap, true);
    
    // {
    //     let mut vm = vm::VM::new(ast);
    //     let start = std::time::Instant::now();
    //     let res = vm.run();
    //     let elapsed = start.elapsed();
        
    //     println!("result: {:?} in {:?}", res, elapsed);
    //     println!("{}", vm.memory);
    // }
    
    {
        let mut jit = backend_cranelift::JIT::new();
        let fns = jit.compile(&ast).unwrap();
        let main_fn = unsafe { std::mem::transmute::<_, fn() -> i32>(fns["main"]) };
        let res = main_fn();
        
        println!("result: {}", res);
    }
}