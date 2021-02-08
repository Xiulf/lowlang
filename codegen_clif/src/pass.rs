use crate::*;
use ir::layout::Abi;

#[derive(Debug, Clone, Copy)]
pub enum PassMode {
    NoPass,
    ByVal(clif::Type),
    ByRef,
}

pub fn pass_mode(module: &impl Module, layout: &ir::layout::TyLayout) -> PassMode {
    if layout.is_zst() {
        PassMode::NoPass
    } else {
        match &layout.abi {
            | Abi::Uninhabited => PassMode::NoPass,
            | Abi::Scalar(s) => PassMode::ByVal(scalar_ty(module, s)),
            | Abi::ScalarPair(_, _) => unimplemented!(),
            | Abi::Aggregate { .. } => PassMode::ByRef,
        }
    }
}
