use crate::{FunctionCtx, Backend};
use crate::ptr::Pointer;
use crate::place::Place;
use lowlang_syntax as syntax;
use syntax::layout::TyLayout;
use cranelift_codegen::ir::{self, InstBuilder};

#[derive(Clone, Copy)]
pub struct Value<'t, 'l> {
    pub kind: ValueKind,
    pub layout: TyLayout<'t, 'l>,
}

#[derive(Clone, Copy)]
pub enum ValueKind {
    Ref(Pointer),
    Val(ir::Value),
}

impl<'t, 'l> Value<'t, 'l> {
    pub fn new_ref(ptr: Pointer, layout: TyLayout<'t, 'l>) -> Value<'t, 'l> {
        Value {
            kind: ValueKind::Ref(ptr),
            layout
        }
    }

    pub fn new_val(value: ir::Value, layout: TyLayout<'t, 'l>) -> Value<'t, 'l> {
        Value {
            kind: ValueKind::Val(value),
            layout,
        }
    }

    pub fn new_const<'a>(fx: &mut FunctionCtx<'a, 't, 'l, impl Backend>, val: u128, layout: TyLayout<'t, 'l>) -> Value<'t, 'l> {
        let clif_type = fx.clif_type(layout).unwrap();
        let val = fx.builder.ins().iconst(clif_type, val as i64);

        Value::new_val(val, layout)
    }

    pub fn new_unit(cx: &syntax::layout::LayoutCtx<'t, 'l>) -> Value<'t, 'l> {
        Value::new_val(ir::Value::with_number(0).unwrap(), cx.defaults.unit)
    }

    pub fn on_stack<'a>(self, fx: &mut FunctionCtx<'a, 't, 'l, impl Backend>) -> Pointer {
        match self.kind {
            ValueKind::Ref(ptr) => ptr,
            ValueKind::Val(_) => {
                let place = Place::new_stack(fx, self.layout);

                place.store(fx, self);
                place.as_ptr(fx)
            },
        }
    }

    pub fn load_scalar<'a>(self, fx: &mut FunctionCtx<'a, 't , 'l, impl Backend>) -> ir::Value {
        match self.kind {
            ValueKind::Ref(ptr) => {
                let clif_type = fx.clif_type(self.layout).unwrap();

                ptr.load(fx, clif_type, ir::MemFlags::new())
            },
            ValueKind::Val(val) => val,
        }
    }

    pub fn cast(self, layout: TyLayout<'t, 'l>) -> Value<'t, 'l> {
        Value {
            kind: self.kind,
            layout,
        }
    }

    pub fn field<'a>(self, fx: &mut FunctionCtx<'a, 't, 'l, impl Backend>, idx: usize) -> Value<'t, 'l> {
        match self.kind {
            ValueKind::Val(_val) => unimplemented!(),
            ValueKind::Ref(ptr) => {
                let (offset, layout) = self.layout.details.fields[idx];
                let new_ptr = ptr.offset_i64(fx, offset as i64);

                Value::new_ref(new_ptr, layout)
            },
        }
    }
}
