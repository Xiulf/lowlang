pub mod const_var;
pub mod blocks;
pub mod vars;
pub mod inline;
pub mod switch;
pub mod merge;

use lowlang_syntax::Package;
use lowlang_syntax::layout::LayoutCtx;

pub trait Transformer<'t> {
    fn transform(&mut self, package: &mut Package<'t>) -> bool;
    fn reset(&mut self);
}

pub fn pre(package: &mut Package) {
    let mut blocks = blocks::BlockRemover::new();
    let mut vars = vars::VarRemover::new();

    run(vec![&mut blocks, &mut vars], package);
}

pub fn post<'t>(package: &mut Package<'t>, lcx: &LayoutCtx<'t, '_>) {
    let mut const_var = const_var::ConstVar::new(lcx);
    let mut vars = vars::VarRemover::new();
    let mut switch = switch::SwitchRemover::new();
    let mut merge = merge::BlockMerger::new();
    let mut blocks = blocks::BlockRemover::new();
    let mut inliner = inline::Inliner::new();

    run(vec![
        &mut const_var,
        &mut vars,
        &mut switch,
        &mut merge,
        &mut blocks,
        &mut inliner
    ], package);
}

fn run<'t>(mut transformers: Vec<&mut dyn Transformer<'t>>, package: &mut Package<'t>) {
    let mut changed = false;

    loop {
        for transformer in &mut transformers {
            changed |= transformer.transform(package);
            transformer.reset();
        }

        if !changed {
            break;
        }

        changed = false;
    }
}
