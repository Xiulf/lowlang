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
        let mut jit = ll_cranelift::JIT::new();
        let fns = jit.compile(&ast).expect("JIT error");
        let jit_main: fn() -> i32 = unsafe { std::mem::transmute(fns["main"]) };
        let start = std::time::Instant::now();
        let result = jit_main();
        let elapsed = start.elapsed();
        
        println!("result: {} in {:?}", result, elapsed);
    }
}