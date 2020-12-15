fn main() {
    let mut args = std::env::args();
    let arg = args.nth(1).unwrap();
    let source = std::fs::read_to_string(arg).unwrap();
    let mut module = ir::parser::parse(&source).unwrap();

    analysis::analyze(analysis::witness::WitnessAnalyzer, &mut module);
    analysis::analyze(analysis::thunk::ThunkAnalyzer::new(), &mut module);
    analysis::analyze(analysis::generic::GenericAnalyzer, &mut module);
    analysis::analyze(analysis::copy::CopyAnalyzer, &mut module);

    println!("{}", module);
}
