use ir::*;

fn main() {
    let mut module = Module::default();
    let main = module.declare(
        "main",
        Linkage::Export,
        Ty::new(Type::Func(Signature::new().param(Ty::I32).param(Ty::U8.ptr().ptr()).ret(Ty::I32))),
    );

    let print_args_ty = Ty::new(Type::Func(Signature::new().param(Ty::I32).param(Ty::U8.ptr().ptr())));
    let print_args = module.declare("print_args", Linkage::Local, print_args_ty);

    let puts_ty = Ty::new(Type::Func(Signature::new().param(Ty::U8.ptr()).ret(Ty::I32)));
    let puts = module.declare("puts", Linkage::Import, puts_ty);

    let mut builder = module.define(main);
    let entry = builder.create_block();

    builder.set_block(entry);

    let argc = builder.add_param(Ty::I32);
    let argv = builder.add_param(Ty::U8.ptr().ptr());
    let _ = builder.call(Const::Addr(print_args), (argc, argv));

    builder.return_(Const::Scalar(0));

    let mut builder = module.define(print_args);
    let entry = builder.create_block();
    let rest = builder.create_block();
    let exit = builder.create_block();

    builder.set_block(entry);

    let argc = builder.add_param(Ty::I32);
    let argv = builder.add_param(Ty::U8.ptr().ptr());

    builder.brif(argc, rest, exit, Vec::new());
    builder.set_block(exit);
    builder.return_(Vec::new());
    builder.set_block(rest);

    let arg = builder.load(argv);
    let _puts = builder.call(Const::Addr(puts), arg);
    let argc = builder.sub(argc, Const::Scalar(1));
    let argv = builder.offset(argv, Const::Scalar(1));

    builder.br(entry, vec![argc, argv]);

    println!("{}", module);

    codegen::run(&module, codegen::Backend::Clif);
}
