fn main() {
    // let reporter = diagnostics::Reporter::new();
    // let filemap = diagnostics::Filemap::new();
    // let mut source = diagnostics::SourceFile::load("test/main.ll");
    
    // filemap.add(&mut source);
    
    // let ast = syntax::parse::<syntax::Program>(&source, &reporter, None);
    
    // reporter.report(&filemap, true);
    
    // let mut type_ctx = check::ctx::TypeCtx::new(&reporter);
    
    // check::typecheck::typecheck(&mut type_ctx, &ast);
    
    // reporter.report(&filemap, true);
    
    // let mut vm = vm::VM::new(ast);
    // let start = std::time::Instant::now();
    // let res = vm.run();
    // let elapsed = start.elapsed();
    
    // println!("result: {:?} in {:?}", res, elapsed);
    // println!("{}", vm.memory);
    use syntax::place;
    
    let mut builder = syntax::builder::Builder::new();
    let mut loop_ = builder.create_function("loop".to_string(), Default::default(), syntax::Type::Int(syntax::IntTy::I32));
    let sum = loop_.add_local(syntax::Type::Int(syntax::IntTy::I32));
    let i = loop_.add_local(syntax::Type::Int(syntax::IntTy::I32));
    let tmp1 = loop_.add_local(syntax::Type::Bool);
    let bb0 = loop_.create_bb();
    let bb1 = loop_.create_bb();
    let bb2 = loop_.create_bb();
    let i_op = loop_.copy(place!(i), Default::default());
    let tmp1_op = loop_.copy(place!(tmp1), Default::default());
    let sum_op = loop_.copy(place!(sum), Default::default());
    let const_10i32 = loop_.const_(loop_.i32(10), Default::default());
    let const_1i32 = loop_.const_(loop_.i32(1), Default::default());
    let const_5i32 = loop_.const_(loop_.i32(5), Default::default());
    
    loop_.use_bb(bb0);
    loop_.ins().init(i);
    loop_.ins().init(tmp1);
    loop_.ins().lt(place!(tmp1), i_op.clone(), const_10i32.clone(), Default::default());
    loop_.ins().assert(tmp1_op.clone(), true, bb1, Some(bb2));
    
    loop_.use_bb(bb1);
    loop_.ins().add(place!(i), i_op.clone(), const_1i32, Default::default());
    loop_.ins().add(place!(sum), sum_op, const_5i32, Default::default());
    loop_.ins().lt(place!(tmp1), i_op, const_10i32, Default::default());
    loop_.ins().assert(tmp1_op, true, bb1, Some(bb2));
    
    loop_.use_bb(bb2);
    loop_.ins().drop(i);
    loop_.ins().drop(tmp1);
    loop_.ins().return_();
    std::mem::drop(loop_);
    
    println!("{}", builder.get_function("loop").unwrap());
}