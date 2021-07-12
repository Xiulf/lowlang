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
    Func(clif::ir::FuncRef),
    Addr(Pointer),
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

    pub fn new_func(func: clif::ir::FuncRef, layout: TyAndLayout) -> Self {
        Self {
            inner: ValInner::Func(func),
            layout,
        }
    }

    pub fn new_addr(ptr: Pointer, layout: TyAndLayout) -> Self {
        Self {
            inner: ValInner::Addr(ptr),
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
            | ValInner::Addr(ptr) => ptr.get_addr(ctx),
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

                let ty1 = ctx.scalar_type(a);
                let ty2 = ctx.scalar_type(b);
                let a_offset = self.layout().fields.offset(0).bytes() as i32;
                let b_offset = self.layout().fields.offset(1).bytes() as i32;
                let val1 = ptr.offset(ctx, a_offset).load(ctx, ty1, clif::MemFlags::new());
                let val2 = ptr.offset(ctx, b_offset).load(ctx, ty2, clif::MemFlags::new());

                (val1, val2)
            },
            | _ => unreachable!(),
        }
    }

    pub fn as_ptr(&self) -> Pointer {
        match self.inner {
            | ValInner::Addr(ptr) => ptr,
            | ValInner::Value(addr) => Pointer::addr(addr),
            | _ => unreachable!(),
        }
    }

    pub fn as_ref(&self) -> (Pointer, Option<clif::Value>) {
        match self.inner {
            | ValInner::Ref(ptr, meta) => (ptr, meta),
            | _ => unreachable!(),
        }
    }

    pub fn as_func(&self) -> Option<clif::ir::FuncRef> {
        match self.inner {
            | ValInner::Func(func) => Some(func),
            | _ => None,
        }
    }

    pub fn deref(self, ctx: &mut BodyCtx) -> Self {
        let pointee = self.layout().pointee(ctx.db);
        let val = self.load(ctx);

        Val::new_ref(Pointer::addr(val), pointee)
    }

    pub fn field(self, ctx: &mut BodyCtx, idx: usize) -> Self {
        let layout = self.layout().field(ctx.db, idx);

        match self.inner {
            | ValInner::Value(val) => match self.layout().abi {
                | Abi::Vector { .. } => {
                    let lane = ctx.bcx.ins().extractlane(val, idx as u8);

                    Self::new_val(lane, layout)
                },
                | _ => unreachable!(),
            },
            | ValInner::ValuePair(a, b) => match self.layout().abi {
                | Abi::ScalarPair(_, _) => {
                    let val = match idx {
                        | 0 => a,
                        | 1 => b,
                        | _ => unreachable!(),
                    };

                    Self::new_val(val, layout)
                },
                | _ => unreachable!(),
            },
            | ValInner::Ref(ptr, None) => {
                let offset = self.layout().fields.offset(idx).bytes();
                let ptr = ptr.offset_i64(ctx, offset as i64);

                Self::new_ref(ptr, layout)
            },
            | ValInner::Ref(_, Some(_)) => todo!(),
            | ValInner::Func(_) => unreachable!(),
            | ValInner::Addr(_) => unreachable!(),
        }
    }

    pub fn store_to(self, ctx: &mut BodyCtx, ptr: Pointer, flags: clif::MemFlags) {
        match self.inner {
            | ValInner::Func(func) => {
                let ptr_type = ctx.module.target_config().pointer_type();
                let addr = ctx.bcx.ins().func_addr(ptr_type, func);

                ptr.store(ctx, addr, flags);
            },
            | ValInner::Value(val) => {
                ptr.store(ctx, val, flags);
            },
            | ValInner::ValuePair(a, b) => {
                let a_offset = self.layout().fields.offset(0).bytes() as i32;
                let b_offset = self.layout().fields.offset(1).bytes() as i32;

                ptr.offset(ctx, a_offset).store(ctx, a, flags);
                ptr.offset(ctx, b_offset).store(ctx, b, flags);
            },
            | ValInner::Ref(src, None) => {
                let align = self.layout().align.bytes();
                let size = self.layout().size.bytes();

                ctx.emit_memcpy(ptr, src, size, align, true, flags);
            },
            | ValInner::Addr(addr) => {
                let addr = addr.get_addr(ctx);

                ptr.store(ctx, addr, flags);
            },
            | ValInner::Ref(_, Some(_)) => unimplemented!(),
        }
    }
}

fn scalar_pair_calculabe_b_offset(ctx: &mut BodyCtx, a: &Scalar, b: &Scalar) -> clif::ir::immediates::Offset32 {
    let triple = ctx.db.triple();
    let b_offset = a.value.size(&triple).align_to(b.value.align(&triple));

    clif::ir::immediates::Offset32::new(b_offset.bytes() as i32)
}
