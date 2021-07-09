use super::*;
use ir::layout::{Abi, Primitive, Scalar, TyAndLayout};
use ptr::Pointer;

#[derive(Clone)]
pub struct Val {
    inner: ValInner,
    layout: TyAndLayout,
}

#[derive(Clone, Copy)]
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

    pub fn new_const(ctx: &mut BodyCtx, val: u128, layout: TyAndLayout) -> Self {
        match &layout.abi {
            | Abi::Scalar(scalar) => {
                let ty = ctx.ir_type(&layout).unwrap();
                let val = match scalar.value {
                    | Primitive::F32 => ctx.bcx.ins().f32const(f64::from_bits(val as u64) as f32),
                    | Primitive::F64 => ctx.bcx.ins().f64const(val as u64),
                    | Primitive::Int(ir::ty::Integer::I128, _) => {
                        let lsb = ctx.bcx.ins().iconst(clif::types::I64, val as u64 as i64);
                        let msb = ctx.bcx.ins().iconst(clif::types::I64, (val >> 64) as u64 as i64);

                        ctx.bcx.ins().iconcat(lsb, msb)
                    },
                    | _ => ctx.bcx.ins().iconst(ty, val as i64),
                };

                Self::new_val(val, layout)
            },
            | _ => unimplemented!(),
        }
    }

    pub fn layout(&self) -> &TyAndLayout {
        &self.layout
    }

    pub fn load(self, ctx: &mut BodyCtx) -> clif::Value {
        match self.inner {
            | ValInner::Value(v) => v,
            | ValInner::Ref(ptr, None) => {
                let ty = ctx.ir_type(self.layout()).unwrap();

                ptr.load(ctx, ty, clif::MemFlags::new())
            },
            | _ => unreachable!(),
        }
    }

    pub fn load_pair(self, ctx: &mut BodyCtx) -> (clif::Value, clif::Value) {
        match self.inner {
            | ValInner::ValuePair(a, b) => (a, b),
            | ValInner::Ref(ptr, None) => {
                let (a, b) = match &self.layout.abi {
                    | Abi::ScalarPair(a, b) => (a, b),
                    | _ => unreachable!(),
                };

                let b_offset = scalar_pair_calculabe_b_offset(ctx, a, b);
                let ty1 = ctx.scalar_type(a);
                let ty2 = ctx.scalar_type(b);
                let val1 = ptr.load(ctx, ty1, clif::MemFlags::new());
                let val2 = ptr.offset(ctx, b_offset).load(ctx, ty2, clif::MemFlags::new());

                (val1, val2)
            },
            | _ => unreachable!(),
        }
    }

    pub fn on_stack(self, ctx: &mut BodyCtx) -> (Pointer, Option<clif::Value>) {
        match self.inner {
            | ValInner::Ref(ptr, meta) => (ptr, meta),
            | ValInner::Value(_) | ValInner::ValuePair(_, _) => unreachable!(),
        }
    }

    pub fn store_to(self, ctx: &mut BodyCtx, ptr: Pointer) {
    }
}

fn scalar_pair_calculabe_b_offset(ctx: &mut BodyCtx, a: &Scalar, b: &Scalar) -> clif::ir::immediates::Offset32 {
    let triple = ctx.db.triple();
    let b_offset = a.value.size(&triple).align_to(b.value.align(&triple));

    clif::ir::immediates::Offset32::new(b_offset.bytes() as i32)
}
