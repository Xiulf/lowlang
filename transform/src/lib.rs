use ir::*;

pub trait Transform {
    fn apply(&mut self, _module: &mut Module) {}
}

pub struct NullTransform;

impl Transform for NullTransform {
    fn apply(&mut self, _: &mut Module) {}
}

impl<T: Transform> Transform for Vec<T> {
    fn apply(&mut self, module: &mut Module) {
        for trans in self {
            trans.apply(module);
        }
    }
}

impl<T: Transform> Transform for Option<T> {
    fn apply(&mut self, module: &mut Module) {
        if let Some(trans) = self {
            trans.apply(module);
        }
    }
}
