use ir::ty::*;
use ir::*;

fn main() {
    let mut source = std::fs::read_to_string("test.ll").unwrap();
    let mut parser = parser::Parser::new(&source);
    let module = parser.parse().unwrap();

    println!("{}", module);

    let obj_file = codegen::compile_module(&module);
    let _ = std::fs::copy(obj_file.path(), "test.o").unwrap();
    let link_args = ["test.o", "runtime/target/runtime.so", "-l", "c", "-o", "test"];
    let link_args = link_args.iter().map(<&str>::to_string).collect::<Vec<_>>();

    mun_lld::link(mun_lld::LldFlavor::Elf, &link_args).ok().unwrap();

    // std::process::Command::new("cc")
    //     .arg("test.o")
    //     .arg("runtime/target/runtime.so")
    //     .arg("-o")
    //     .arg("test")
    //     .status()
    //     .unwrap();

    std::fs::remove_file("test.o").unwrap();
}
