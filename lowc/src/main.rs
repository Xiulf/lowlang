fn main() {
    let mut args = std::env::args();
    let arg = args.nth(1).unwrap();
    let source = std::fs::read_to_string(arg).unwrap();
    let mut module = ir::parser::parse(&source).unwrap();
    let target = target_lexicon::Triple::host();

    analysis::mandatory(&mut module, &target);

    println!("{}", module);

    let obj = assemble::assemble(&module, target);

    obj.copy(&std::path::PathBuf::from("test.o"));
}
