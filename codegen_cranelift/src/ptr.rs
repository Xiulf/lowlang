use crate::*;
use clif::ir::{self as cir, immediates::Offset32};
use clif::InstBuilder;
use clif::Module as _;
use ir::layout::Align;
use std::convert::TryFrom;

#[derive(Debug, Clone, Copy)]
pub struct Pointer {
    pub kind: PointerKind,
    pub offset: Offset32,
}

#[derive(Debug, Clone, Copy)]
pub enum PointerKind {
    Addr(cir::Value),
    Stack(cir::StackSlot),
    Dangling(Align),
}

impl Pointer {
    pub fn addr(addr: cir::Value) -> Self {
        Pointer {
            kind: PointerKind::Addr(addr),
            offset: Offset32::new(0),
        }
    }

    pub fn stack(slot: cir::StackSlot) -> Self {
        Pointer {
            kind: PointerKind::Stack(slot),
            offset: Offset32::new(0),
        }
    }

    pub fn const_addr<'ctx>(
        fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>,
        addr: i64,
    ) -> Self {
        let ptr_type = fx.module.target_config().pointer_type();
        let addr = fx.bcx.ins().iconst(ptr_type, addr);

        Pointer {
            kind: PointerKind::Addr(addr),
            offset: Offset32::new(0),
        }
    }

    pub fn dangling(align: Align) -> Self {
        Pointer {
            kind: PointerKind::Dangling(align),
            offset: Offset32::new(0),
        }
    }

    pub fn get_addr<'ctx>(
        self,
        fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>,
    ) -> cir::Value {
        let ptr_type = fx.module.target_config().pointer_type();

        match self.kind {
            PointerKind::Addr(addr) => {
                let offset: i64 = self.offset.into();

                if offset == 0 {
                    addr
                } else {
                    fx.bcx.ins().iadd_imm(addr, offset)
                }
            }
            PointerKind::Stack(ss) => fx.bcx.ins().stack_addr(ptr_type, ss, self.offset),
            PointerKind::Dangling(align) => fx
                .bcx
                .ins()
                .iconst(ptr_type, i64::try_from(align.bytes()).unwrap()),
        }
    }

    pub fn offset<'ctx>(
        self,
        fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>,
        offset: Offset32,
    ) -> Self {
        self.offset_i64(fx, offset.into())
    }

    pub fn offset_i64<'ctx>(
        self,
        fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>,
        offset: i64,
    ) -> Self {
        if let Some(new_offset) = self.offset.try_add_i64(offset) {
            Pointer {
                kind: self.kind,
                offset: new_offset,
            }
        } else {
            let base_offset: i64 = self.offset.into();
            let ptr_type = fx.module.target_config().pointer_type();

            if let Some(new_offset) = base_offset.checked_add(offset) {
                let base_addr = match self.kind {
                    PointerKind::Addr(addr) => addr,
                    PointerKind::Stack(ss) => fx.bcx.ins().stack_addr(ptr_type, ss, 0),
                    PointerKind::Dangling(align) => fx
                        .bcx
                        .ins()
                        .iconst(ptr_type, i64::try_from(align.bytes()).unwrap()),
                };

                let addr = fx.bcx.ins().iadd_imm(base_addr, new_offset);

                Pointer {
                    kind: PointerKind::Addr(addr),
                    offset: Offset32::new(0),
                }
            } else {
                panic!("too large offset")
            }
        }
    }

    pub fn offset_value<'ctx>(
        self,
        fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>,
        offset: cir::Value,
    ) -> Self {
        let ptr_type = fx.module.target_config().pointer_type();

        match self.kind {
            PointerKind::Addr(addr) => Pointer {
                kind: PointerKind::Addr(fx.bcx.ins().iadd(addr, offset)),
                offset: self.offset,
            },
            PointerKind::Stack(slot) => {
                let addr = fx.bcx.ins().stack_addr(ptr_type, slot, self.offset);

                Pointer {
                    kind: PointerKind::Addr(fx.bcx.ins().iadd(addr, offset)),
                    offset: Offset32::new(0),
                }
            }
            PointerKind::Dangling(align) => {
                let addr = fx
                    .bcx
                    .ins()
                    .iconst(ptr_type, i64::try_from(align.bytes()).unwrap());

                Pointer {
                    kind: PointerKind::Addr(fx.bcx.ins().iadd(addr, offset)),
                    offset: self.offset,
                }
            }
        }
    }

    pub fn load<'ctx>(
        self,
        fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>,
        ty: cir::Type,
        flags: cir::MemFlags,
    ) -> cir::Value {
        match self.kind {
            PointerKind::Addr(addr) => fx.bcx.ins().load(ty, flags, addr, self.offset),
            PointerKind::Stack(ss) => fx.bcx.ins().stack_load(ty, ss, self.offset),
            PointerKind::Dangling(_) => unreachable!(),
        }
    }

    pub fn store<'ctx>(
        self,
        fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>,
        value: cir::Value,
        flags: cir::MemFlags,
    ) {
        match self.kind {
            PointerKind::Addr(addr) => {
                fx.bcx.ins().store(flags, value, addr, self.offset);
            }
            PointerKind::Stack(ss) => {
                fx.bcx.ins().stack_store(value, ss, self.offset);
            }
            PointerKind::Dangling(_) => unreachable!(),
        }
    }
}
