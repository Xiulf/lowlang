use ir::ty::*;
use ir::*;

fn main() {
    let source = std::fs::read_to_string("test.ll").unwrap();
    let parser = parser::Parser::new(&source);
    let module = parser.parse().unwrap();

    println!("{}", module);

    let obj_file = codegen::compile_module(&module);
    let _ = std::fs::copy(obj_file.path(), "test.o").unwrap();
    let link_args = [
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

    std::fs::remove_file("test.o").unwrap();
}
