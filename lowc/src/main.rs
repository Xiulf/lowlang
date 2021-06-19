use ir::*;

fn main() {
    let mut module = Module::new("test");
    let identity = mk_identity(&mut module);
    let main = mk_main(&mut module, identity);

    println!("{}", module);
}

fn mk_main(module: &mut Module, identity: FuncId) -> FuncId {
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

    let identity = builder.func_ref(identity);
    let res = builder.apply(identity, [Subst::Type(int32.clone())], [argc])[0];
    let three = builder.const_int(3, int32.clone());
    let added = builder.intrinsic("add.i32", [res, three]);

    builder.return_(added);
    builder.finish();

    module.define_func(main, body);
    main
}

fn mk_identity(module: &mut Module) -> FuncId {
    let mut generic = Ty::generic();
    let param = generic.add_param(GenericParam::Type).at(0);
    let param = Ty::new(typ::Var(param));
    let sig = Signature::new().param(param.clone()).ret(param);
    let sig = generic.finish(Ty::new(typ::Func(sig)));

    let identity = module.declare_func("identity", Linkage::Local, sig);
    let body = module.declare_body();
    let mut builder = module.define_body(body);
    let entry = builder.create_block();
    let param = builder.add_generic_param(GenericParam::Type);

    builder.set_block(entry);

    let arg = builder.add_param(entry, Ty::new(typ::Var(param)));

    builder.return_([arg]);
    builder.finish();

    module.define_func(identity, body);
    identity
}
