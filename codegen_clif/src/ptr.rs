use crate::*;
use clif::immediates::Offset32;

#[derive(Debug, Clone, Copy)]
pub struct Pointer {
    pub kind: PointerKind,
    pub offset: Offset32,
}

#[derive(Debug, Clone, Copy)]
pub enum PointerKind {
    Addr(clif::Value),
    Stack(clif::StackSlot),
    Dangling(ir::layout::Align),
}

impl Pointer {
    pub fn addr(addr: clif::Value) -> Self {
        Pointer {
            kind: PointerKind::Addr(addr),
            offset: Offset32::new(0),
        }
    }

    pub fn stack(slot: clif::StackSlot) -> Self {
        Pointer {
            kind: PointerKind::Stack(slot),
            offset: Offset32::new(0),
        }
    }

    pub fn const_addr(fx: &mut FunctionCtx<impl Module>, addr: i64) -> Self {
        let ptr_type = fx.module.target_config().pointer_type();
        let addr = fx.bcx.ins().iconst(ptr_type, addr);

        Pointer {
            kind: PointerKind::Addr(addr),
            offset: Offset32::new(0),
        }
    }

    pub fn dangling(align: ir::layout::Align) -> Self {
        Pointer {
            kind: PointerKind::Dangling(align),
            offset: Offset32::new(0),
        }
    }

    pub fn get_addr(self, fx: &mut FunctionCtx<impl Module>) -> clif::Value {
        let ptr_type = fx.module.target_config().pointer_type();

        match self.kind {
            | PointerKind::Addr(addr) => {
                let offset: i64 = self.offset.into();

                if offset == 0 {
                    addr
                } else {
                    fx.bcx.ins().iadd_imm(addr, offset)
                }
            },
            | PointerKind::Stack(ss) => fx.bcx.ins().stack_addr(ptr_type, ss, self.offset),
            | PointerKind::Dangling(align) => fx.bcx.ins().iconst(ptr_type, align.bytes() as i64),
        }
    }

    pub fn offset(self, fx: &mut FunctionCtx<impl Module>, offset: Offset32) -> Self {
        self.offset_i64(fx, offset.into())
    }

    pub fn offset_i64(self, fx: &mut FunctionCtx<impl Module>, offset: i64) -> Self {
        if let Some(new_offset) = self.offset.try_add_i64(offset) {
            Pointer { offset: new_offset, ..self }
        } else {
            let base_offset: i64 = self.offset.into();
            let ptr_type = fx.module.target_config().pointer_type();

            if let Some(new_offset) = base_offset.checked_add(offset) {
                let base_addr = match self.kind {
                    | PointerKind::Addr(addr) => addr,
                    | PointerKind::Stack(ss) => fx.bcx.ins().stack_addr(ptr_type, ss, 0),
                    | PointerKind::Dangling(align) => fx.bcx.ins().iconst(ptr_type, align.bytes() as i64),
                };

                let addr = fx.bcx.ins().iadd_imm(base_addr, new_offset);

                Pointer {
                    kind: PointerKind::Addr(addr),
                    offset: Offset32::new(0),
                }
            } else {
                panic!("too large offset");
            }
        }
    }

    pub fn offset_value(self, fx: &mut FunctionCtx<impl Module>, offset: clif::Value) -> Self {
        let ptr_type = fx.module.target_config().pointer_type();

        match self.kind {
            | PointerKind::Addr(addr) => Pointer {
                kind: PointerKind::Addr(fx.bcx.ins().iadd(addr, offset)),
                offset: self.offset,
            },
            | PointerKind::Stack(ss) => {
                let addr = fx.bcx.ins().stack_addr(ptr_type, ss, self.offset);

                Pointer {
                    kind: PointerKind::Addr(fx.bcx.ins().iadd(addr, offset)),
                    offset: Offset32::new(0),
                }
            },
            | PointerKind::Dangling(align) => {
                let addr = fx.bcx.ins().iconst(ptr_type, align.bytes() as i64);

                Pointer {
                    kind: PointerKind::Addr(fx.bcx.ins().iadd(addr, offset)),
                    offset: self.offset,
                }
            },
        }
    }

    pub fn load(self, fx: &mut FunctionCtx<impl Module>, ty: clif::Type, flags: clif::MemFlags) -> clif::Value {
        match self.kind {
            | PointerKind::Addr(addr) => fx.bcx.ins().load(ty, flags, addr, self.offset),
            | PointerKind::Stack(ss) => fx.bcx.ins().stack_load(ty, ss, self.offset),
            | PointerKind::Dangling(_) => unreachable!(),
        }
    }

    pub fn store(self, fx: &mut FunctionCtx<impl Module>, value: clif::Value, flags: clif::MemFlags) {
        match self.kind {
            | PointerKind::Addr(addr) => {
                fx.bcx.ins().store(flags, value, addr, self.offset);
            },
            | PointerKind::Stack(ss) => {
                fx.bcx.ins().stack_store(value, ss, self.offset);
            },
            | PointerKind::Dangling(_) => unreachable!(),
        }
    }
}
