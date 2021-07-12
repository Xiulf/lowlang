use super::*;
use clif::ir::immediates::Offset32;
use clif::{InstBuilder, Module};
use ir::layout::Align;

#[derive(Debug, Clone, Copy)]
pub struct Pointer {
    kind: PointerKind,
    offset: Offset32,
}

#[derive(Debug, Clone, Copy)]
enum PointerKind {
    Addr(clif::Value),
    Stack(clif::ir::StackSlot),
    Dangling(Align),
}

impl Pointer {
    pub fn addr(addr: clif::Value) -> Self {
        Self {
            kind: PointerKind::Addr(addr),
            offset: Offset32::new(0),
        }
    }

    pub fn stack(slot: clif::ir::StackSlot) -> Self {
        Self {
            kind: PointerKind::Stack(slot),
            offset: Offset32::new(0),
        }
    }

    pub fn dangling(align: Align) -> Self {
        Self {
            kind: PointerKind::Dangling(align),
            offset: Offset32::new(0),
        }
    }

    pub fn const_addr(ctx: &mut BodyCtx, addr: i64) -> Self {
        let ptr_type = ctx.module.target_config().pointer_type();
        let addr = ctx.bcx.ins().iconst(ptr_type, addr);

        Self {
            kind: PointerKind::Addr(addr),
            offset: Offset32::new(0),
        }
    }

    pub fn get_addr(self, ctx: &mut BodyCtx) -> clif::Value {
        let ptr_type = ctx.module.target_config().pointer_type();

        match self.kind {
            | PointerKind::Addr(addr) => {
                let offset: i64 = self.offset.into();

                if offset == 0 {
                    addr
                } else {
                    ctx.bcx.ins().iadd_imm(addr, offset)
                }
            },
            | PointerKind::Stack(ss) => ctx.bcx.ins().stack_addr(ptr_type, ss, self.offset),
            | PointerKind::Dangling(align) => ctx.bcx.ins().iconst(ptr_type, align.bytes() as i64),
        }
    }

    pub fn load(self, ctx: &mut BodyCtx, ty: clif::Type, flags: clif::MemFlags) -> clif::Value {
        match self.kind {
            | PointerKind::Addr(addr) => ctx.bcx.ins().load(ty, flags, addr, self.offset),
            | PointerKind::Stack(ss) => ctx.bcx.ins().stack_load(ty, ss, self.offset),
            | PointerKind::Dangling(_) => unreachable!(),
        }
    }

    pub fn store(self, ctx: &mut BodyCtx, value: clif::Value, flags: clif::MemFlags) {
        match self.kind {
            | PointerKind::Addr(addr) => ctx.bcx.ins().store(flags, value, addr, self.offset),
            | PointerKind::Stack(ss) => ctx.bcx.ins().stack_store(value, ss, self.offset),
            | PointerKind::Dangling(_) => unreachable!(),
        };
    }

    pub fn offset(self, ctx: &mut BodyCtx, offset: impl Into<Offset32>) -> Self {
        self.offset_i64(ctx, offset.into().into())
    }

    pub fn offset_i64(self, ctx: &mut BodyCtx, offset: i64) -> Self {
        if let Some(new_offset) = self.offset.try_add_i64(offset) {
            Self {
                kind: self.kind,
                offset: new_offset,
            }
        } else {
            let base_offset: i64 = self.offset.into();
            let ptr_type = ctx.module.target_config().pointer_type();

            if let Some(new_offset) = base_offset.checked_add(offset) {
                let base_addr = match self.kind {
                    | PointerKind::Addr(addr) => addr,
                    | PointerKind::Stack(ss) => ctx.bcx.ins().stack_addr(ptr_type, ss, 0),
                    | PointerKind::Dangling(align) => ctx.bcx.ins().iconst(ptr_type, align.bytes() as i64),
                };

                let addr = ctx.bcx.ins().iadd_imm(base_addr, new_offset);

                Self {
                    kind: PointerKind::Addr(addr),
                    offset: Offset32::new(0),
                }
            } else {
                panic!("too large offset");
            }
        }
    }

    pub fn offset_value(self, ctx: &mut BodyCtx, offset: clif::Value) -> Self {
        let ptr_type = ctx.module.target_config().pointer_type();

        match self.kind {
            | PointerKind::Addr(addr) => {
                let addr = ctx.bcx.ins().iadd(addr, offset);

                Self {
                    kind: PointerKind::Addr(addr),
                    offset: self.offset,
                }
            },
            | PointerKind::Stack(ss) => {
                let addr = ctx.bcx.ins().stack_addr(ptr_type, ss, self.offset);
                let addr = ctx.bcx.ins().iadd(addr, offset);

                Self {
                    kind: PointerKind::Addr(addr),
                    offset: Offset32::new(0),
                }
            },
            | PointerKind::Dangling(align) => {
                let addr = ctx.bcx.ins().iadd_imm(offset, align.bytes() as i64);

                Self {
                    kind: PointerKind::Addr(addr),
                    offset: self.offset,
                }
            },
        }
    }
}
