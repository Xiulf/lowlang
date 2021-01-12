use crate::place::Place;
use crate::ptr::Pointer;
use crate::{ClifBackend, FunctionCtx};
use cranelift::codegen::ir as cir;
use cranelift::prelude::InstBuilder;
use ir::layout::{Abi, Primitive, TyLayout};
use std::convert::TryFrom;
use std::marker::PhantomData;

#[derive(Debug, Clone)]
pub struct Value<'ctx> {
    pub kind: ValueKind,
    pub layout: TyLayout,
    pub(crate) _marker: PhantomData<&'ctx cranelift::codegen::Context>,
}

#[derive(Debug, Clone, Copy)]
pub enum ValueKind {
    Ref(Pointer, Option<cir::Value>),
    Val(cir::Value),
    ValPair(cir::Value, cir::Value),
}

impl<'ctx> Value<'ctx> {
    pub fn new_ref(ptr: Pointer, layout: TyLayout) -> Self {
        Value {
            kind: ValueKind::Ref(ptr, None),
            layout,
            _marker: PhantomData,
        }
    }

    pub fn new_ref_meta(ptr: Pointer, meta: cir::Value, layout: TyLayout) -> Self {
        Value {
            kind: ValueKind::Ref(ptr, Some(meta)),
            layout,
            _marker: PhantomData,
        }
    }

    pub fn new_val(val: cir::Value, layout: TyLayout) -> Self {
        Value {
            kind: ValueKind::Val(val),
            layout,
            _marker: PhantomData,
        }
    }

    pub fn new_val_pair(val1: cir::Value, val2: cir::Value, layout: TyLayout) -> Self {
        Value {
            kind: ValueKind::ValPair(val1, val2),
            layout,
            _marker: PhantomData,
        }
    }

    pub fn new_const(
        val: u128,
        fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>,
        layout: TyLayout,
    ) -> Self {
        let ty = fx.ir_type(&layout).unwrap();
        let val = match &layout.abi {
            Abi::Scalar(scalar) => match scalar.value {
                Primitive::F32 => fx.bcx.ins().f32const(f64::from_bits(val as u64) as f32),
                Primitive::F64 => fx.bcx.ins().f64const(val as u64),
                _ => fx.bcx.ins().iconst(ty, val as i64),
            },
            _ => unimplemented!(),
        };

        Value::new_val(val, layout)
    }

    pub fn new_unit() -> Self {
        Value::new_val(cir::Value::with_number(0).unwrap(), TyLayout::unit())
    }

    pub fn on_stack(
        self,
        fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>,
    ) -> (Pointer, Option<cir::Value>) {
        match self.kind {
            ValueKind::Ref(ptr, meta) => (ptr, meta),
            ValueKind::Val(_) | ValueKind::ValPair(_, _) => {
                use codegen::Place as _;
                let place = Place::new_stack(fx, self.layout.clone());

                place.clone().store(fx, self);

                (place.as_ptr(), None)
            }
        }
    }
}

impl<'ctx> codegen::Value<'ctx> for Value<'ctx> {
    type Backend = ClifBackend<'ctx>;
    type Raw = cir::Value;

    fn layout(&self) -> &TyLayout {
        &self.layout
    }

    // comment
    #[track_caller]
    fn load_scalar(self, fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>) -> Self::Raw {
        match self.kind {
            ValueKind::Ref(ptr, None) => {
                let ty = fx.ir_type(&self.layout).unwrap();

                ptr.load(fx, ty, cir::MemFlags::new())
            }
            ValueKind::Val(val) => val,
            ValueKind::Ref(_, Some(_)) => unreachable!(),
            ValueKind::ValPair(_, _) => unreachable!(),
        }
    }

    fn load_scalar_pair(
        self,
        fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>,
    ) -> (Self::Raw, Self::Raw) {
        match self.kind {
            ValueKind::Ref(ptr, None) => {
                let (a, b) = match &self.layout.abi {
                    Abi::ScalarPair(a, b) => (a, b),
                    _ => unreachable!(),
                };

                let b_offset = crate::place::scalar_pair_calculate_b_offset(&fx.target, a, b);
                let ty1 = fx.scalar_ty(a);
                let ty2 = fx.scalar_ty(b);
                let val1 = ptr.load(fx, ty1, cir::MemFlags::new());
                let val2 = ptr.offset(fx, b_offset).load(fx, ty2, cir::MemFlags::new());

                (val1, val2)
            }
            ValueKind::Ref(ptr, Some(meta)) => (ptr.get_addr(fx), meta),
            ValueKind::Val(_) => unreachable!(),
            ValueKind::ValPair(a, b) => (a, b),
        }
    }

    fn cast(
        self,
        _fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>,
        layout: TyLayout,
    ) -> Self {
        Value {
            kind: self.kind,
            layout,
            _marker: PhantomData,
        }
    }

    fn field(self, fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>, idx: usize) -> Self {
        match self.kind {
            ValueKind::Val(_) => {
                if idx == 0 {
                    self
                } else {
                    unreachable!();
                }
            }
            ValueKind::Ref(ptr, None) => {
                let (field_ptr, field_layout) = gen_field(fx, ptr, None, self.layout, idx);

                Value::new_ref(field_ptr, field_layout)
            }
            ValueKind::Ref(ptr, Some(meta)) => {
                let (field_ptr, field_layout) = gen_field(fx, ptr, Some(meta), self.layout, idx);

                Value::new_ref_meta(field_ptr, meta, field_layout)
            }
            ValueKind::ValPair(a, b) => {
                let field_layout = self.layout.field(idx, &fx.target);

                if idx == 0 {
                    Value::new_val(a, field_layout)
                } else {
                    Value::new_val(b, field_layout)
                }
            }
        }
    }

    fn deref(self, fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>) -> Self {
        let pointee = self.layout.pointee(&fx.target);
        let is_box = matches!(self.layout.ty.kind, ir::Type::Box(_));
        let ptr = self.load_scalar(fx);

        if is_box {
            use crate::clif::Module;
            let ptr_type = fx.module.target_config().pointer_type();
            let ptr = Pointer::addr(ptr);
            let ptr = ptr.load(fx, ptr_type, crate::clif::MemFlags::trusted());

            Value::new_ref(Pointer::addr(ptr), pointee)
        } else {
            Value::new_val(ptr, pointee)
        }
    }
}

fn gen_field<'ctx>(
    fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>,
    base: Pointer,
    meta: Option<cir::Value>,
    layout: TyLayout,
    field: usize,
) -> (Pointer, TyLayout) {
    let field_offset = layout.fields.offset(field);
    let field_layout = layout.field(field, &fx.target);
    let simple = |fx| {
        (
            base.offset_i64(fx, i64::try_from(field_offset.bytes()).unwrap()),
            field_layout.clone(),
        )
    };

    if let Some(_meta) = meta {
        if !field_layout.is_unsized() {
            return simple(fx);
        }

        match field_layout.ty {
            // Type::Str => simple(fx),
            _ => unimplemented!(),
        }
    } else {
        simple(fx)
    }
}
