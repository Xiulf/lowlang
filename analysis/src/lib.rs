#![feature(drain_filter)]

pub mod copy;
pub mod generic;
pub mod thunk;
pub mod witness;

use transform::Transform;

pub trait Analyzer {
    type Output: Transform;

    fn analyze(&mut self, module: &ir::Module) -> Self::Output;
}

pub fn analyze<A: Analyzer>(mut analyzer: A, module: &mut ir::Module) {
    let mut transform = analyzer.analyze(module);

    transform.apply(module);
}
