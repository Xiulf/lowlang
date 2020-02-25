use lowlang_syntax as syntax;
use intern::Intern;

pub fn main() {
    let mut args = std::env::args();
    let arg = args.nth(1).unwrap();
    let arg2 = args.next().unwrap();
    let source = std::fs::read_to_string(&arg).unwrap();
    let reporter = diagnostics::Reporter::default();
    let files = diagnostics::FileInterner::new();
    let file_id = diagnostics::FileInfo {
        name: arg.into(),
        source: source.clone(),
    }.intern(&files);

    let type_interner = syntax::ty::TypeInterner::new();
    let types = syntax::ty::TyCtx::new(&type_interner);
    let mut package: syntax::Package = syntax::parse(&source, file_id, &reporter, None, &types);

    reporter.report(true);

    syntax::post::post_process(&mut package);
    lowlang_transform::pre(&mut package);
    syntax::mono::monomorphize(&mut package, &types);
    lowlang_check::verify(&package, &types);

    let layout_interner = lowlang_syntax::layout::LayoutInterner::new();
    let triple = lowlang_cranelift::triple(&target_lexicon::HOST.to_string());
    let lcx = lowlang_syntax::layout::LayoutCtx::new(&types, &layout_interner, &triple);

    lowlang_transform::post(&mut package, &lcx);

    println!("{}", package);

    match lowlang_cranelift::compile(&package, &types, &target_lexicon::HOST.to_string(), true, arg2.into()) {
        Ok(_) => {},
        Err(err) => eprintln!("{}", err),
    }
}
