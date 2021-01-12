#![feature(drain_filter)]
#![feature(bindings_after_at)]
#![feature(box_patterns)]

pub mod copy;
pub mod generic;
pub mod lifetime;
pub mod stack_alloc;
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

pub fn mandatory(module: &mut ir::Module, target: &target_lexicon::Triple) {
    analyze(witness::WitnessAnalyzer, module);
    analyze(thunk::ThunkAnalyzer::new(), module);
    analyze(generic::GenericAnalyzer, module);
    analyze(copy::CopyAnalyzer, module);
    analyze(type_info::TypeInfoAnalyzer::new(target), module);
    analyze(lifetime::LifetimeAnalyzer::new(), module);
    analyze(stack_alloc::AllocAnalyzer::new(target), module);
}
