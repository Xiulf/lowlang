use intern::Intern;

pub fn main() {
    let mut args = std::env::args();
    let arg = args.nth(1).unwrap();
    let arg2 = args.next().unwrap();
    let source = std::fs::read_to_string(&arg).unwrap();
    let reporter = diagnostics::Reporter::default();
    let file_id = diagnostics::FileInfo {
        name: arg.into(),
        source: source.clone(),
    }.intern();

    let module = syntax::parse::<syntax::Package>(&source, file_id, &reporter, None);

    reporter.report(true);

    match codegen::compile(&module, arg2.into()) {
        Ok(_) => {},
        Err(err) => eprintln!("{}", err),
    }
}
