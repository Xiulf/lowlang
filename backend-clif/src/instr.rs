use super::*;
use ::middle::TypeInfo;
use ir::{layout::TyAndLayout, ty::typ};

impl<'a, 'db, 'ctx> BodyCtx<'a, 'db, 'ctx> {
    pub fn copy_value(&mut self, val: Val) -> Val {
        let lookup = val.layout().ty.lookup(self.db);
        let ptr_type = self.module.target_config().pointer_type();

        if lookup.flags.is_set(Flags::TRIVIAL) {
            val
        } else {
            match lookup.kind {
                | typ::Box(BoxKind::None, ty) => {
                    let layout = self.db.layout_of(ty);
                    let size = self.bcx.ins().iconst(ptr_type, layout.size.bytes() as i64);
                    let size = self.dynamic_size(size, ty);
                    let layout = val.layout().clone();
                    let res = self.call_malloc(size);
                    let res = Val::new_val(res, layout);

                    self.copy_addr(res.clone(), val, ty, true);
                    res
                },
                | typ::Box(BoxKind::Gen, _) => val,
                | typ::Box(BoxKind::Rc, _) => {
                    let ptr = val.clone().load(self);
                    let strong_count = self.bcx.ins().load(ptr_type, clif::MemFlags::trusted(), ptr, 0);
                    let strong_count = self.bcx.ins().iadd_imm(strong_count, 1);

                    self.bcx.ins().store(clif::MemFlags::trusted(), strong_count, ptr, 0);
                    val
                },
                | typ::Tuple(ref tys) => {
                    let vals = (0..tys.len())
                        .map(|i| {
                            let field = val.clone().field(self, i);

                            self.copy_value(field)
                        })
                        .collect();

                    self.mk_tuple(vals, val.layout().clone())
                },
                | _ => todo!(),
            }
        }
    }

    pub fn drop_value(&mut self, val: Val) {
        let lookup = val.layout().ty.lookup(self.db);
        let ptr_type = self.module.target_config().pointer_type();

        if !lookup.flags.is_set(Flags::TRIVIAL) {
            match lookup.kind {
                | typ::Var(_) => unreachable!(),
                | typ::Box(BoxKind::None, ty) => {
                    self.drop_addr(val.clone(), ty);

                    let val = val.load(self);

                    self.call_free(val);
                },
                | typ::Box(BoxKind::Gen, _) => {},
                | typ::Box(BoxKind::Rc, _) => {
                    let (_, ty) = match val.layout().ty.lookup(self.db).kind {
                        | TypeKind::Box(kind, ty) => (kind, ty),
                        | _ => unreachable!(),
                    };

                    let ptr = val.as_ptr().get_addr(self);
                    let one = self.bcx.ins().iconst(ptr_type, 1);
                    let strong_count = self.bcx.ins().load(ptr_type, clif::MemFlags::trusted(), ptr, 0);
                    let strong_count = self.bcx.ins().isub(strong_count, one);
                    let cond = self.bcx.ins().icmp_imm(clif::IntCC::SignedLessThanOrEqual, strong_count, 0);
                    let drop_block = self.bcx.create_block();
                    let exit_block = self.bcx.create_block();

                    self.bcx.ins().store(clif::MemFlags::trusted(), strong_count, ptr, 0);
                    self.bcx.ins().brnz(cond, drop_block, &[]);
                    self.bcx.ins().jump(exit_block, &[]);
                    self.bcx.switch_to_block(drop_block);

                    let layout = self.db.layout_of(ty.ptr(self.db));
                    let val = Pointer::addr(ptr).offset_i64(self, ptr_type.bytes() as i64 * 2);
                    let val = Val::new_addr(val, layout);

                    self.drop_addr(val, ty);
                    self.call_free(ptr);
                    self.bcx.ins().jump(exit_block, &[]);
                    self.bcx.switch_to_block(exit_block);
                },
                | typ::Tuple(ref tys) => {
                    for i in 0..tys.len() {
                        let field = val.clone().field(self, i);

                        self.drop_value(field);
                    }
                },
                | _ => todo!(),
            }
        }
    }

    pub fn copy_addr(&mut self, dst: Val, src: Val, ty: ir::ty::Ty, copy: bool) {
        let lookup = ty.lookup(self.db);
        let ptr_type = self.cx.module.target_config().pointer_type();

        if let typ::Var(var) = lookup.kind {
            let info = self.generic_params[var.idx()].clone();
            let copy_fn = self.get_vwt_field(ty, if copy { 3 } else { 4 }).load(self);
            let mut copy_sig = self.cx.module.make_signature();

            copy_sig.params = (0..3).map(|_| clif::AbiParam::new(ptr_type)).collect();

            let copy_sig = self.bcx.import_signature(copy_sig);
            let info = info.as_ptr().get_addr(self);
            let dst = dst.load(self);
            let src = src.load(self);

            self.bcx.ins().call_indirect(copy_sig, copy_fn, &[dst, src, info]);
        } else if lookup.flags.is_set(Flags::TRIVIAL) {
            let align = dst.layout().align.bytes();
            let size = dst.layout().size.bytes();

            self.emit_memcpy(dst.as_ptr(), src.as_ptr(), size, align, true, clif::MemFlags::new());
        } else {
            todo!();
        }
    }

    pub fn drop_addr(&mut self, addr: Val, ty: ir::ty::Ty) {
        let lookup = ty.lookup(self.db);
        let ptr_type = self.cx.module.target_config().pointer_type();

        if let typ::Var(var) = lookup.kind {
            let info = self.generic_params[var.idx()].clone();
            let drop_fn = self.get_vwt_field(ty, 5).load(self);
            let mut drop_sig = self.cx.module.make_signature();

            drop_sig.params = (0..2).map(|_| clif::AbiParam::new(ptr_type)).collect();

            let drop_sig = self.bcx.import_signature(drop_sig);
            let info = info.as_ptr().get_addr(self);
            let addr = addr.load(self);

            self.bcx.ins().call_indirect(drop_sig, drop_fn, &[addr, info]);
        } else if !lookup.flags.is_set(Flags::TRIVIAL) {
            match lookup.kind {
                | TypeKind::Box(..) => todo!(),
                | TypeKind::Tuple(ref tys) => {
                    let ptr = addr.as_ptr();
                    let pointee = addr.layout().pointee(self.db);

                    for i in 0..tys.len() {
                        let ty = pointee.field(self.db, i).ty;
                        let layout = self.db.layout_of(ty.ptr(self.db));
                        let field = self.dynamic_offset(ptr, &pointee, i);
                        let field = Val::new_addr(field, layout);

                        self.drop_addr(field, ty);
                    }
                },
                | _ => {
                    let mut mcx = self.cx.middle_ctx();
                    let id = self.cx.middle.info_of(&mut mcx, self.db, ty);

                    let drop_fn = match self.cx.middle[id] {
                        | TypeInfo::Trivial { .. } => unreachable!(),
                        | TypeInfo::Concrete { vwt } => self.cx.middle[vwt].drop_fn,
                        | TypeInfo::Generic { drop_fn, .. } => drop_fn,
                        | TypeInfo::External(_) => todo!(),
                    };

                    let drop_fn = self.cx.module.declare_func_in_func(drop_fn, &mut self.bcx.func);
                    let info = self.get_type_info(ty).as_ptr().get_addr(self);
                    let addr = addr.load(self);

                    self.bcx.ins().call(drop_fn, &[addr, info]);
                },
            }
        }
    }

    pub fn mk_tuple(&mut self, vals: Vec<Val>, layout: TyAndLayout) -> Val {
        let mut vals = vals.into_iter();

        match &layout.abi {
            | Abi::Scalar(_) => vals.next().unwrap().cast(layout),
            | Abi::ScalarPair(_, _) => {
                let a = vals.next().unwrap();

                if let Some(b) = vals.next() {
                    assert!(vals.next().is_none());
                    let a = a.load(self);
                    let b = b.load(self);

                    Val::new_val_pair(a, b, layout)
                } else {
                    a.cast(layout)
                }
            },
            | _ if layout.abi.is_unsized() => todo!(),
            | _ => {
                let ss = self.bcx.create_stack_slot(clif::StackSlotData {
                    kind: clif::StackSlotKind::ExplicitSlot,
                    size: layout.size.bytes() as u32,
                    offset: None,
                });

                let res = Val::new_ref(Pointer::stack(ss), layout);

                for (i, val) in vals.enumerate() {
                    let (ptr, _) = res.clone().field(self, i).as_ref();

                    val.store_to(self, ptr, clif::MemFlags::trusted());
                }

                res
            },
        }
    }
}
