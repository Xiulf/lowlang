fn main() {
    let reporter = diagnostics::Reporter::new();
    let filemap = diagnostics::Filemap::new();
    let mut source = diagnostics::SourceFile::load("test/main.ll");
    
    filemap.add(&mut source);
    
    let ast = syntax::parse::<syntax::Program>(&source, &reporter, None);
    
    reporter.report(&filemap, true);
    
    let mut vm = vm::VM::new(ast);
    let start = std::time::Instant::now();
    let res = vm.run();
    let elapsed = start.elapsed();
    
    println!("result: {:?} in {:?}", res, elapsed);
    println!("{}", vm.memory);
}