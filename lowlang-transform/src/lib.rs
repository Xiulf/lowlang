pub mod const_var;
pub mod blocks;
pub mod vars;
pub mod inline;

use lowlang_syntax::Package;
use lowlang_syntax::layout::LayoutCtx;

pub trait Transformer<'t> {
    fn transform(&mut self, package: &mut Package<'t>);
}

pub fn pre(package: &mut Package) {
    let mut blocks = blocks::BlockRemover::new();

    run(vec![&mut blocks], package);
}

pub fn post<'t>(package: &mut Package<'t>, lcx: &LayoutCtx<'t, '_>) {
    let mut const_var = const_var::ConstVar::new(lcx);
    let mut inliner = inline::Inliner::new();

    run(vec![&mut const_var, &mut inliner], package);
}

fn run<'t>(transformers: Vec<&mut dyn Transformer<'t>>, package: &mut Package<'t>) {
    for transformer in transformers {
        transformer.transform(package);
    }
}
