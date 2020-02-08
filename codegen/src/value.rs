use crate::{FunctionCtx, Backend};
use crate::ptr::Pointer;
use crate::place::Place;
use syntax::layout::Layout;
use cranelift_codegen::ir::{self, InstBuilder};

#[derive(Clone, Copy)]
pub struct Value {
    pub kind: ValueKind,
    pub layout: Layout,
}

#[derive(Clone, Copy)]
pub enum ValueKind {
    Ref(Pointer),
    Val(ir::Value),
}

impl Value {
    pub fn new_ref(ptr: Pointer, layout: Layout) -> Value {
        Value {
            kind: ValueKind::Ref(ptr),
            layout
        }
    }

    pub fn new_val(value: ir::Value, layout: Layout) -> Value {
        Value {
            kind: ValueKind::Val(value),
            layout,
        }
    }

    pub fn new_const(fx: &mut FunctionCtx<impl Backend>, val: u128, layout: Layout) -> Value {
        let clif_type = fx.clif_type(layout).unwrap();
        let val = fx.builder.ins().iconst(clif_type, val as i64);

        Value::new_val(val, layout)
    }

    pub fn new_unit() -> Value {
        Value::new_val(ir::Value::with_number(0).unwrap(), syntax::layout::UNIT)
    }

    pub fn on_stack(self, fx: &mut FunctionCtx<impl Backend>) -> Pointer {
        match self.kind {
            ValueKind::Ref(ptr) => ptr,
            ValueKind::Val(_) => {
                let place = Place::new_stack(fx, self.layout);

                place.store(fx, self);
                place.as_ptr(fx)
            },
        }
    }

    pub fn load_scalar(self, fx: &mut FunctionCtx<impl Backend>) -> ir::Value {
        match self.kind {
            ValueKind::Ref(ptr) => {
                let clif_type = fx.clif_type(self.layout).unwrap();

                ptr.load(fx, clif_type, ir::MemFlags::new())
            },
            ValueKind::Val(val) => val,
        }
    }

    pub fn cast(self, layout: Layout) -> Value {
        Value {
            kind: self.kind,
            layout,
        }
    }

    pub fn field(self, fx: &mut FunctionCtx<impl Backend>, idx: usize) -> Value {
        match self.kind {
            ValueKind::Val(_val) => unimplemented!(),
            ValueKind::Ref(ptr) => {
                let (offset, layout) = self.layout.details().fields[idx];
                let new_ptr = ptr.offset_i64(fx, offset as i64);

                Value::new_ref(new_ptr, layout)
            },
        }
    }
}
