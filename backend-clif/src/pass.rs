use super::*;
use ir::layout::{Abi, Size, TyAndLayout};

#[derive(Clone, Copy)]
pub enum PassMode {
    NoPass,
    ByVal(clif::Type),
    ByValPair(clif::Type, clif::Type),
    ByRef { size: Option<Size> },
}

impl<'ctx> CodegenCtx<'ctx> {
    pub fn pass_mode(&self, layout: &TyAndLayout) -> PassMode {
        if layout.is_zst() {
            PassMode::NoPass
        } else {
            match &layout.abi {
                | Abi::Uninhabited => PassMode::NoPass,
                | Abi::Scalar(s) => PassMode::ByVal(self.scalar_type(s)),
                | Abi::ScalarPair(a, b) => PassMode::ByValPair(self.scalar_type(a), self.scalar_type(b)),
                | Abi::Vector { elem, count } => PassMode::ByVal(self.vector_type(elem, *count)),
                | Abi::Aggregate { sized: true } => PassMode::ByRef { size: Some(layout.size) },
                | Abi::Aggregate { sized: false } => PassMode::ByRef { size: None },
            }
        }
    }
}
