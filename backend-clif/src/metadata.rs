use super::*;
use ::middle::TypeInfo;
use ir::{
    layout::{Fields, Size, TyAndLayout},
    ty::{typ, Subst},
    Flags, TypeDefBody,
};

impl<'a, 'db, 'ctx> BodyCtx<'a, 'db, 'ctx> {
    pub fn drop_addr(&mut self, addr: Val, ty: ir::ty::Ty) {
        let lookup = ty.lookup(self.db);
        let ptr_type = self.cx.module.target_config().pointer_type();
        let addr = addr.load(self);

        if let typ::Var(var) = lookup.kind {
            let info = self.generic_params[var.idx()].clone();
            let vwt = info.clone().field(self, 0).deref(self);
            let drop_fn = vwt.field(self, 5).load(self);
            let mut drop_sig = self.cx.module.make_signature();

            drop_sig.params = (0..2).map(|_| clif::AbiParam::new(ptr_type)).collect();

            let drop_sig = self.bcx.import_signature(drop_sig);
            let info = info.as_ptr().get_addr(self);

            self.bcx.ins().call_indirect(drop_sig, drop_fn, &[addr, info]);
        } else if !lookup.flags.is_set(Flags::TRIVIAL) {
            match lookup.kind {
                | TypeKind::Box(..) => todo!(),
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
                    let info = self.get_type_metadata(ty).as_ptr().get_addr(self);

                    self.bcx.ins().call(drop_fn, &[addr, info]);
                },
            }
        }
    }

    pub fn get_type_metadata(&mut self, ty: ir::ty::Ty) -> Val {
        let lookup = ty.lookup(self.db);

        if let typ::Var(var) = lookup.kind {
            return self.generic_params[var.idx()].clone();
        } else if lookup.flags.is_set(Flags::TRIVIAL) {
            let layout = self.db.layout_of(ty);
            let size = layout.size.bytes();

            if let Some(val) = self.get_trivial_meta(size) {
                return val;
            }
        }

        let mut mcx = self.cx.middle_ctx();
        let ptr_type = self.module.target_config().pointer_type();
        let id = self.cx.middle.info_of(&mut mcx, self.db, ty);
        let val = match self.cx.middle[id] {
            | TypeInfo::External(data) => {
                let gv = self.cx.module.declare_data_in_func(data, &mut self.bcx.func);

                self.bcx.ins().global_value(ptr_type, gv)
            },
            | TypeInfo::Generic {
                generic_count,
                info_field_count,
                mk_generics,
                mk_vwt,
                mk_info,
                ..
            } => {
                let subst = match lookup.kind {
                    | typ::Def(_, Some(ref s)) => s,
                    | _ => unreachable!(),
                };

                let mk_generics = self.cx.module.declare_func_in_func(mk_generics, &mut self.bcx.func);
                let mk_vwt = self.cx.module.declare_func_in_func(mk_vwt, &mut self.bcx.func);
                let mk_info = self.cx.module.declare_func_in_func(mk_info, &mut self.bcx.func);
                let generics = self.bcx.create_stack_slot(clif::StackSlotData {
                    kind: clif::StackSlotKind::ExplicitSlot,
                    size: ptr_type.bytes() * generic_count as u32,
                    offset: None,
                });

                let generics = self.bcx.ins().stack_addr(ptr_type, generics, 0);
                let args = std::iter::once(generics)
                    .chain(subst.iter().map(|s| match s {
                        | Subst::Type(ty) => self.get_type_metadata(*ty).load(self),
                        | _ => todo!(),
                    }))
                    .collect::<Vec<_>>();

                self.bcx.ins().call(mk_generics, &args);

                let vwt = self.bcx.create_stack_slot(clif::StackSlotData {
                    kind: clif::StackSlotKind::ExplicitSlot,
                    size: ptr_type.bytes() * 6,
                    offset: None,
                });

                let vwt = self.bcx.ins().stack_addr(ptr_type, vwt, 0);

                self.bcx.ins().call(mk_vwt, &[vwt, generics]);

                let info = self.bcx.create_stack_slot(clif::StackSlotData {
                    kind: clif::StackSlotKind::ExplicitSlot,
                    size: (2 + generic_count + info_field_count) as u32 * ptr_type.bytes(),
                    offset: None,
                });

                let info = self.bcx.ins().stack_addr(ptr_type, info, 0);

                self.bcx.ins().call(mk_info, &[info, vwt, generics]);
                info
            },
            | _ => todo!(),
        };

        let ty = self.typ().ptr(self.db);
        let layout = self.db.layout_of(ty);

        Val::new_val(val, layout)
    }

    pub fn get_trivial_meta(&mut self, size: u64) -> Option<Val> {
        let ptr_type = self.module.target_config().pointer_type();
        let trivial_metas = self.cx.trivial_metas();
        let idx = match size {
            | 0 => 0,
            | 1 => 1,
            | 2 => 2,
            | 4 => 3,
            | 8 => 4,
            | 16 => 5,
            | _ => return None,
        };

        let typ = self.typ();
        let layout = self.db.layout_of(typ);
        let gv = self.cx.module.declare_data_in_func(trivial_metas, &mut self.bcx.func);
        let addr = self.bcx.ins().global_value(ptr_type, gv);
        let addr = self.bcx.ins().iadd_imm(addr, layout.stride.bytes() as i64 * idx);
        let layout = self.db.layout_of(typ.ptr(self.db));

        Some(Val::new_val(addr, layout))
    }

    pub fn dynamic_offset(&mut self, mut ptr: Pointer, layout: &TyAndLayout, field: usize) -> Pointer {
        if let Fields::Arbitrary { offsets, memory_index } = &layout.fields {
            let inverse_index = ir::layout::invert_mapping(memory_index);
            let mut offset = Size::ZERO;

            match layout.ty.lookup(self.db).kind {
                | typ::Tuple(ref fields) => {
                    for (i, j) in inverse_index.into_iter().enumerate() {
                        if j == field {
                            break;
                        }

                        if let typ::Var(var) = fields[j].lookup(self.db).kind {
                            let typ = self.generic_params[var.idx()].clone();
                            let vwt = typ.clone().field(self, 0).deref(self);
                            let stride = vwt.field(self, 2);
                            let stride = stride.load(self);

                            ptr = ptr.offset_i64(self, offset.bytes() as i64);
                            ptr = ptr.offset_value(self, stride);
                            offset = Size::ZERO;
                        } else {
                            offset = offsets[i];
                        }
                    }
                },
                | typ::Def(id, ref subst) => {
                    let subst = subst.as_deref().unwrap_or(&[]);
                    let info = id.lookup(self.db);
                    let body = info.body.as_ref().unwrap();

                    match body {
                        | TypeDefBody::Struct { fields } => {
                            for (i, j) in inverse_index.into_iter().enumerate() {
                                if j == field {
                                    break;
                                }

                                if let typ::Var(var) = fields[j].ty.subst(self.db, subst, 0).lookup(self.db).kind {
                                    let typ = self.generic_params[var.idx()].clone();
                                    let vwt = typ.clone().field(self, 0).deref(self);
                                    let stride = vwt.field(self, 2);
                                    let stride = stride.load(self);

                                    ptr = ptr.offset_i64(self, offset.bytes() as i64);
                                    ptr = ptr.offset_value(self, stride);
                                    offset = Size::ZERO;
                                } else {
                                    offset = offsets[i];
                                }
                            }
                        },
                        | _ => unreachable!(),
                    }
                },
                | _ => unreachable!(),
            }

            ptr.offset_i64(self, offset.bytes() as i64)
        } else {
            ptr.offset_i64(self, layout.fields.offset(field).bytes() as i64)
        }
    }
}
