use crate::{FunctionCtx, Backend};
use crate::ptr::Pointer;
use crate::value::{Value, ValueKind};
use syntax::layout::Layout;
use intern::Intern;
use cranelift_frontend::Variable;
use cranelift_codegen::ir::{self, InstBuilder};

#[derive(Clone, Copy)]
pub struct Place {
    pub kind: PlaceKind,
    pub layout: Layout,
}

#[derive(Clone, Copy)]
pub enum PlaceKind {
    Var(Variable),
    Addr(Pointer),
    NoPlace,
}

impl Place {
    pub fn new_var(fx: &mut FunctionCtx<impl Backend>, id: syntax::LocalId, layout: Layout) -> Place {
        let var = Variable::with_u32(id.0 as u32);

        fx.builder.declare_var(var, fx.clif_type(layout).unwrap());

        Place {
            kind: PlaceKind::Var(var),
            layout,
        }
    }

    pub fn new_stack(fx: &mut FunctionCtx<impl Backend>, layout: Layout) -> Place {
        if layout.details().size == 0 {
            return Place {
                kind: PlaceKind::NoPlace,
                layout,
            };
        }

        let slot = fx.builder.create_stack_slot(ir::StackSlotData {
            kind: ir::StackSlotKind::ExplicitSlot,
            size: layout.details().size as u32,
            offset: None,
        });

        Place {
            kind: PlaceKind::Addr(Pointer::stack(slot)),
            layout,
        }
    }

    pub fn new_ref(ptr: Pointer, layout: Layout) -> Place {
        Place {
            kind: PlaceKind::Addr(ptr),
            layout,
        }
    }

    pub fn as_ptr(self, fx: &mut FunctionCtx<impl Backend>) -> Pointer {
        match self.kind {
            PlaceKind::Addr(ptr) => ptr,
            PlaceKind::Var(var) => Pointer::addr(fx.builder.use_var(var)),
            PlaceKind::NoPlace => Pointer::const_addr(fx, 0),
        }
    }

    pub fn to_value(self, fx: &mut FunctionCtx<impl Backend>) -> Value {
        match self.kind {
            PlaceKind::Var(var) => {
                let val = fx.builder.use_var(var);

                Value::new_val(val, self.layout)
            },
            PlaceKind::Addr(ptr) => {
                Value::new_ref(ptr, self.layout)
            },
            PlaceKind::NoPlace => {
                Value::new_ref(Pointer::const_addr(fx, 0), self.layout)
            },
        }
    }

    pub fn deref(self, fx: &mut FunctionCtx<impl Backend>) -> Place {
        if let Some(pointee) = self.layout.details().pointee {
            Place {
                kind: PlaceKind::Addr(Pointer::addr(self.to_value(fx).load_scalar(fx))),
                layout: pointee,
            }
        } else {
            unreachable!();
        }
    }

    pub fn index(self, fx: &mut FunctionCtx<impl Backend>, idx: ir::Value) -> Place {
        let layout = self.layout.details().idx.unwrap();
        let new_idx = fx.builder.ins().imul_imm(idx, layout.details().size as i64);

        match &*syntax::Type::untern(self.layout.details().ty) {
            syntax::Type::Array(..) => {
                let ptr = self.as_ptr(fx);
                let new_ptr = ptr.offset_value(fx, new_idx);

                Place {
                    kind: PlaceKind::Addr(new_ptr),
                    layout,
                }
            },
            syntax::Type::Vector(..) => {
                unimplemented!();
            },
            _ => {
                let ptr = self.field(fx, 0).deref(fx).as_ptr(fx);
                let new_ptr = ptr.offset_value(fx, new_idx);

                Place {
                    kind: PlaceKind::Addr(new_ptr),
                    layout,
                }
            },
        }
    }

    pub fn const_index(self, fx: &mut FunctionCtx<impl Backend>, idx: usize) -> Place {
        let layout = self.layout.details().idx.unwrap();
        let new_idx = idx * layout.details().size;

        match &*syntax::Type::untern(self.layout.details().ty) {
            syntax::Type::Array(..) => {
                let ptr = self.as_ptr(fx);
                let new_ptr = ptr.offset_i64(fx, new_idx as i64);

                Place {
                    kind: PlaceKind::Addr(new_ptr),
                    layout,
                }
            },
            syntax::Type::Vector(..) => {
                unimplemented!();
            },
            _ => {
                let ptr = self.field(fx, 0).deref(fx).as_ptr(fx);
                let new_ptr = ptr.offset_i64(fx, new_idx as i64);

                Place {
                    kind: PlaceKind::Addr(new_ptr),
                    layout,
                }
            },
        }
    }

    pub fn field(self, fx: &mut FunctionCtx<impl Backend>, idx: usize) -> Place {
        let (offset, layout) = self.layout.details().fields[idx];
        let ptr = self.as_ptr(fx);
        let new_ptr = ptr.offset_i64(fx, offset as i64);

        Place {
            kind: PlaceKind::Addr(new_ptr),
            layout,
        }
    }

    pub fn store(self, fx: &mut FunctionCtx<impl Backend>, value: Value) {
        let to_ptr = match self.kind {
            PlaceKind::Var(var) => {
                let data = value.load_scalar(fx);

                fx.builder.def_var(var, data);

                return;
            },
            PlaceKind::Addr(ptr) => ptr,
            PlaceKind::NoPlace => return,
        };

        match value.kind {
            ValueKind::Val(val) => {
                to_ptr.store(fx, val, ir::MemFlags::new());
            },
            ValueKind::Ref(val_ptr) => {
                let val_addr = val_ptr.get_addr(fx);
                let to_addr = to_ptr.get_addr(fx);
                let size = self.layout.details().size;
                let val_align = value.layout.details().align as u8;
                let to_align = self.layout.details().align as u8;

                fx.builder.emit_small_memcpy(
                    fx.module.target_config(),
                    to_addr,
                    val_addr,
                    size as u64,
                    to_align,
                    val_align,
                );
            },
        }
    }
}
