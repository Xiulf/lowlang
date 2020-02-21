use crate::{FunctionCtx, Backend};
use cranelift_codegen::ir::{self, InstBuilder, immediates::Offset32};

#[derive(Clone, Copy)]
pub struct Pointer {
    pub kind: PointerKind,
    pub offset: Offset32,
}

#[derive(Clone, Copy)]
pub enum PointerKind {
    Addr(ir::Value),
    Stack(ir::StackSlot),
}

impl Pointer {
    pub fn addr(addr: ir::Value) -> Pointer {
        Pointer {
            kind: PointerKind::Addr(addr),
            offset: Offset32::new(0),
        }
    }

    pub fn stack(slot: ir::StackSlot) -> Pointer {
        Pointer {
            kind: PointerKind::Stack(slot),
            offset: Offset32::new(0),
        }
    }

    pub fn const_addr(fx: &mut FunctionCtx<impl Backend>, addr: i64) -> Pointer {
        let addr = fx.builder.ins().iconst(fx.pointer_type, addr);

        Pointer {
            kind: PointerKind::Addr(addr),
            offset: Offset32::new(0),
        }
    }

    pub fn get_addr(self, fx: &mut FunctionCtx<impl Backend>) -> ir::Value {
        match self.kind {
            PointerKind::Addr(addr) => {
                let offset: i64 = self.offset.into();

                if offset == 0 {
                    addr
                } else {
                    fx.builder.ins().iadd_imm(addr, offset)
                }
            },
            PointerKind::Stack(slot) => fx.builder.ins().stack_addr(fx.pointer_type, slot, self.offset),
        }
    }

    pub fn try_get_addr_and_offset(self) -> Option<(ir::Value, Offset32)> {
        match self.kind {
            PointerKind::Addr(addr) => Some((addr, self.offset)),
            PointerKind::Stack(_) => None,
        }
    }

    pub fn offset(self, fx: &mut FunctionCtx<impl Backend>, extra_offset: Offset32) -> Pointer {
        self.offset_i64(fx, extra_offset.into())
    }

    pub fn offset_i64(self, fx: &mut FunctionCtx<impl Backend>, extra_offset: i64) -> Pointer {
        if let Some(new_offset) = self.offset.try_add_i64(extra_offset) {
            Pointer {
                kind: self.kind,
                offset: new_offset,
            }
        } else {
            let base_offset: i64 = self.offset.into();
            
            if let Some(new_offset) = base_offset.checked_add(extra_offset){
                let base_addr = match self.kind {
                    PointerKind::Addr(addr) => addr,
                    PointerKind::Stack(slot) => fx.builder.ins().stack_addr(fx.pointer_type, slot, 0),
                };
                
                let addr = fx.builder.ins().iadd_imm(base_addr, new_offset);
                
                Pointer {
                    kind: PointerKind::Addr(addr),
                    offset: Offset32::new(0),
                }
            } else {
                panic!("self.offset ({}) + extra_offset ({}) not representable in i64", base_offset, extra_offset);
            }
        }
    }

    pub fn offset_value(self, fx: &mut FunctionCtx<impl Backend>, extra_offset: ir::Value) -> Pointer {
        match self.kind {
            PointerKind::Addr(addr) => Pointer {
                kind: PointerKind::Addr(fx.builder.ins().iadd(addr, extra_offset)),
                offset: self.offset,
            },
            PointerKind::Stack(slot) => {
                let addr = fx.builder.ins().stack_addr(fx.pointer_type, slot, self.offset);
                
                Pointer {
                    kind: PointerKind::Addr(fx.builder.ins().iadd(addr, extra_offset)),
                    offset: Offset32::new(0),
                }
            }
        }
    }

    pub fn load(self, fx: &mut FunctionCtx<impl Backend>, ty: ir::Type, flags: ir::MemFlags) -> ir::Value {
        match self.kind {
            PointerKind::Addr(addr) => fx.builder.ins().load(ty, flags, addr, self.offset),
            PointerKind::Stack(slot) => fx.builder.ins().stack_load(ty, slot, self.offset),
        }
    }

    pub fn store(self, fx: &mut FunctionCtx<impl Backend>, value: ir::Value, flags: ir::MemFlags) {
        match self.kind {
            PointerKind::Addr(addr) => { fx.builder.ins().store(flags, value, addr, self.offset); },
            PointerKind::Stack(slot) => { fx.builder.ins().stack_store(value, slot, self.offset); },
        }
    }
}
