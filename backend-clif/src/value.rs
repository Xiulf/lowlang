use super::*;
use ir::layout::{Layout, TyAndLayout};
use ptr::Pointer;

pub struct Val {
    inner: ValInner,
    layout: TyAndLayout,
}

pub enum ValInner {
    Value(clif::Value),
    ValuePair(clif::Value, clif::Value),
    Ref(Pointer, Option<clif::Value>),
}

impl Val {
    pub fn new_val(val: clif::Value, layout: TyAndLayout) -> Self {
        Self {
            inner: ValInner::Value(val),
            layout,
        }
    }

    pub fn new_val_pair(a: clif::Value, b: clif::Value, layout: TyAndLayout) -> Self {
        Self {
            inner: ValInner::ValuePair(a, b),
            layout,
        }
    }

    pub fn new_ref(ptr: Pointer, layout: TyAndLayout) -> Self {
        Self {
            inner: ValInner::Ref(ptr, None),
            layout,
        }
    }

    pub fn new_ref_meta(ptr: Pointer, meta: clif::Value, layout: TyAndLayout) -> Self {
        Self {
            inner: ValInner::Ref(ptr, Some(meta)),
            layout,
        }
    }

    pub fn new_zst(layout: TyAndLayout) -> Self {
        Self::new_ref(Pointer::dangling(layout.align), layout)
    }
}
