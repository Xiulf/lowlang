use crate::*;
use ir::layout::{Abi, Integer, Primitive, TyLayout};

#[derive(Debug, Clone)]
pub struct Value {
    pub kind: ValueKind,
    pub layout: TyLayout,
}

#[derive(Debug, Clone, Copy)]
pub enum ValueKind {
    Ref(Pointer),
    Val(clif::Value),
}

impl Value {
    pub fn new_ref(ptr: Pointer, layout: TyLayout) -> Self {
        Value {
            kind: ValueKind::Ref(ptr),
            layout,
        }
    }

    pub fn new_val(val: clif::Value, layout: TyLayout) -> Self {
        Value {
            kind: ValueKind::Val(val),
            layout,
        }
    }

    pub fn new_unit(layout: TyLayout) -> Self {
        Value::new_val(clif::Value::with_number(0).unwrap(), layout)
    }

    pub fn new_const(val: u128, fx: &mut FunctionCtx<impl Module>, layout: TyLayout) -> Self {
        let val = match &layout.abi {
            | Abi::Scalar(scalar) => match scalar.value {
                | Primitive::Bool => fx.bcx.ins().bconst(clif::types::B8, val != 0),
                | Primitive::Pointer => fx.bcx.ins().iconst(fx.module.target_config().pointer_type(), val as i64),
                | Primitive::F32 => fx.bcx.ins().f32const(f32::from_bits(val as u32)),
                | Primitive::F64 => fx.bcx.ins().f64const(f64::from_bits(val as u64)),
                | Primitive::Int(Integer::I8, _) => fx.bcx.ins().iconst(clif::types::I8, val as i64),
                | Primitive::Int(Integer::I16, _) => fx.bcx.ins().iconst(clif::types::I16, val as i64),
                | Primitive::Int(Integer::I32, _) => fx.bcx.ins().iconst(clif::types::I32, val as i64),
                | Primitive::Int(Integer::I64, _) => fx.bcx.ins().iconst(clif::types::I64, val as i64),
                | Primitive::Int(Integer::I128, _) => fx.bcx.ins().iconst(clif::types::I128, val as i64),
            },
            | _ => unimplemented!(),
        };

        Value {
            kind: ValueKind::Val(val),
            layout,
        }
    }

    pub fn on_stack(self, fx: &mut FunctionCtx<impl Module>) -> Pointer {
        match self.kind {
            | ValueKind::Ref(ptr) => ptr,
            | ValueKind::Val(val) => {
                let ss = clif::StackSlotData {
                    kind: clif::StackSlotKind::ExplicitSlot,
                    size: self.layout.size.bytes() as u32,
                    offset: None,
                };

                let ss = fx.bcx.create_stack_slot(ss);
                let ptr = Pointer::stack(ss);

                ptr.store(fx, val, clif::MemFlags::trusted());
                ptr
            },
        }
    }

    pub fn load_scalar(self, fx: &mut FunctionCtx<impl Module>) -> clif::Value {
        match self.kind {
            | ValueKind::Val(val) => val,
            | ValueKind::Ref(ptr) => {
                let ty = clif_type(&fx.module, &self.layout).unwrap();

                ptr.load(fx, ty, clif::MemFlags::new())
            },
        }
    }

    pub fn bitcast(self, layout: TyLayout) -> Self {
        Value { layout, ..self }
    }

    pub fn deref(self, fx: &mut FunctionCtx<impl Module>) -> Self {
        let pointee = self.layout.pointee(fx.module.isa().triple());
        let addr = self.load_scalar(fx);
        let ptr = Pointer::addr(addr);

        Value::new_ref(ptr, pointee)
    }
}
