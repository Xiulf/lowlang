use ir::ty::*;
use ir::*;

fn main() {
    let mut module = Module::new("test");
    // let identity = mk_identity(&mut module);
    let factorial = mk_factorial(&mut module);
    let main = mk_main(&mut module, factorial /*, identity */);

    println!("{}", module);

    let obj_file = codegen::compile_module(&module);

    std::fs::copy(obj_file.path(), "test.o").unwrap();
}

fn mk_main(module: &mut Module, factorial: FuncId /*, identity: FuncId*/) -> FuncId {
    let int32 = Ty::int(Integer::I32, true);
    let u8_ptr_ptr = Ty::int(Integer::I8, false).ptr().ptr();
    let sig = Signature::new().param(int32.clone()).param(u8_ptr_ptr.clone()).ret(int32.clone());
    let sig = Ty::new(typ::Func(sig));

    let main = module.declare_func("main", Linkage::Export, sig);
    let body = module.declare_body();
    let mut builder = module.define_body(body);
    let entry = builder.create_block();
    let argc = builder.add_param(entry, int32.clone());
    let argv = builder.add_param(entry, u8_ptr_ptr);

    builder.set_block(entry);

    // let identity = builder.func_ref(identity);
    // let res = builder.apply(identity, [Subst::Type(int32.clone())], [argc])[0];
    // let three = builder.const_int(3, int32.clone());
    // let res = builder.intrinsic("add_i32", [argc, three]);
    let fac_ref = builder.func_ref(factorial);
    let n5 = builder.const_int(5, int32.clone());
    let res = builder.apply(fac_ref, [], [n5]);

    builder.return_(res);
    builder.finish();

    module.define_func(main, body);
    main
}

fn mk_factorial(module: &mut Module) -> FuncId {
    let int32 = Ty::int(Integer::I32, true);
    let sig = Signature::new().param(int32.clone()).ret(int32.clone());
    let sig = Ty::new(typ::Func(sig));

    let factorial = module.declare_func("factorial", Linkage::Local, sig);
    let body = module.declare_body();
    let mut builder = module.define_body(body);
    let entry = builder.create_block();
    let arg = builder.add_param(entry, int32.clone());
    let zero = builder.const_int(0, int32.clone());
    let one = builder.const_int(1, int32.clone());
    let le = builder.intrinsic("le_i32", [arg, zero])[0];
    let then_block = builder.create_block();
    let else_block = builder.create_block();
    let mut switch = builder.switch(le);

    switch.case(0, else_block, []);
    switch.build(then_block, []);

    builder.set_block(then_block);
    builder.return_([one]);
    builder.set_block(else_block);

    let fac_ref = builder.func_ref(factorial);
    let sub = builder.intrinsic("sub_i32", [arg, one])[0];
    let app = builder.apply(fac_ref, [], [sub])[0];
    let mul = builder.intrinsic("mul_i32", [arg, app])[0];

    builder.return_([mul]);
    builder.finish();

    module.define_func(factorial, body);
    factorial
}

// fn mk_identity(module: &mut Module) -> FuncId {
//     let mut generic = Ty::generic();
//     let param = generic.add_param(GenericParam::Type).at(0);
//     let param = Ty::new(typ::Var(param));
//     let sig = Signature::new().param(param.clone()).ret(param);
//     let sig = generic.finish(Ty::new(typ::Func(sig)));
//
//     let identity = module.declare_func("identity", Linkage::Local, sig);
//     let body = module.declare_body();
//     let mut builder = module.define_body(body);
//     let entry = builder.create_block();
//     let param = builder.add_generic_param(GenericParam::Type);
//
//     builder.set_block(entry);
//
//     let arg = builder.add_param(entry, Ty::new(typ::Var(param)));
//
//     builder.return_([arg]);
//     builder.finish();
//
//     module.define_func(identity, body);
//     identity
// }
