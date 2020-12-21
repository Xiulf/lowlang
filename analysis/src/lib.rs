#![feature(drain_filter)]
#![feature(bindings_after_at)]
#![feature(box_patterns)]

pub mod copy;
pub mod generic;
pub mod lifetime;
pub mod thunk;
pub mod type_info;
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
