use super::*;
use ir::layout::{Abi, Size, TyAndLayout};

#[derive(Clone, Copy)]
pub enum PassMode {
    NoPass,
    ByVal(clif::Type),
    ByValPair(clif::Type, clif::Type),
    ByRef { size: Option<Size> },
}

pub enum EmptySinglePair<T> {
    Empty,
    Single(T),
    Pair(T, T),
}

impl<T> Iterator for EmptySinglePair<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match std::mem::replace(self, Self::Empty) {
            | Self::Empty => None,
            | Self::Single(v) => Some(v),
            | Self::Pair(a, b) => {
                *self = Self::Single(b);
                Some(a)
            },
        }
    }
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

impl<'a, 'ctx> BodyCtx<'a, 'ctx> {
    pub fn value_for_ret(&mut self, val: Val) -> EmptySinglePair<clif::Value> {
        match self.pass_mode(val.layout()) {
            | PassMode::NoPass | PassMode::ByRef { .. } => EmptySinglePair::Empty,
            | PassMode::ByVal(_) => EmptySinglePair::Single(val.load(self)),
            | PassMode::ByValPair(_, _) => {
                let (a, b) = val.load_pair(self);

                EmptySinglePair::Pair(a, b)
            },
        }
    }

    pub fn value_for_arg(&mut self, val: Val) -> EmptySinglePair<clif::Value> {
        match self.pass_mode(val.layout()) {
            | PassMode::NoPass => EmptySinglePair::Empty,
            | PassMode::ByVal(_) => EmptySinglePair::Single(val.load(self)),
            | PassMode::ByValPair(_, _) => {
                let (a, b) = val.load_pair(self);

                EmptySinglePair::Pair(a, b)
            },
            | PassMode::ByRef { .. } => match val.on_stack(self) {
                | (ptr, None) => EmptySinglePair::Single(ptr.get_addr(self)),
                | (ptr, Some(meta)) => EmptySinglePair::Pair(ptr.get_addr(self), meta),
            },
        }
    }
}
