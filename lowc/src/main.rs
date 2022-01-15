use ir::db::IrDatabase;
use ir::*;
use std::sync::Arc;

#[salsa::database(ir::db::IrDatabaseStorage)]
#[derive(Default)]
struct LlDatabase {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for LlDatabase {
}

fn main() {
    let mut db = LlDatabase::default();

    db.set_triple(Arc::new(ir::layout::Triple::host()));

    let source = std::fs::read_to_string("test.ll").unwrap();
    let parser = parser::Parser::new(&db, &source);
    let module = parser.parse().unwrap();

    println!("{}", module.display(&db));

    // let mut vm = eval::VM::new(&db, &module);
    // let main = module.func("main").unwrap();
    // let res = vm.apply(main, Vec::new()).unwrap();
    //
    // dbg!(res);

    let obj_file = codegen::compile_module(&db, &module, codegen::Backend::Clif);
    let _ = std::fs::copy(obj_file.path(), "test.o").unwrap();
    /*let link_args = [
        "-dynamic-linker",
        "/lib64/ld-linux-x86-64.so.2",
        "-o",
        "test",
        "/usr/lib/x86_64-linux-gnu/crt1.o",
        "/usr/lib/x86_64-linux-gnu/crti.o",
        "test.o",
        "runtime/target/runtime.so",
        "-L",
        "/usr/lib/x86_64-linux-gnu",
        "-l",
        "c",
        "/usr/lib/x86_64-linux-gnu/crtn.o",
    ];

    let link_args = link_args.iter().map(<&str>::to_string).collect::<Vec<_>>();

    if let Err(e) = mun_lld::link(mun_lld::LldFlavor::Elf, &link_args).ok() {
        eprintln!("{}", e);
    }

    std::fs::remove_file("test.o").unwrap();*/
}
