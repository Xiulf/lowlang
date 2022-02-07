use super::*;
use ::middle::TypeInfo;
use ir::{
    layout::{Fields, Size, TyAndLayout},
    ty::{typ, GenericVar, Subst},
    Flags, TypeDefBody,
};

impl<'a, 'db, 'ctx> BodyCtx<'a, 'db, 'ctx> {
    pub fn drop_addr(&mut self, addr: Val, ty: ir::ty::Ty) {
        let lookup = ty.lookup(self.db);
        let ptr_type = self.cx.module.target_config().pointer_type();
        let addr = addr.load(self);

        if let typ::Var(var) = lookup.kind {
            let info = self.generic_params[var.idx()].clone();
            let drop_fn = self.get_vwt_field(ty, 5).load(self);
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
                    let info = self.get_type_info(ty).as_ptr().get_addr(self);

                    self.bcx.ins().call(drop_fn, &[addr, info]);
                },
            }
        }
    }

    pub fn get_vwt_field(&mut self, ty: ir::ty::Ty, field: usize) -> Val {
        if let Some(val) = self.cache.vwt_field.get(&(ty, field)) {
            return val.clone();
        }

        let vwt = self.get_vwt(ty);
        let val = vwt.field(self, field);

        self.cache.vwt_field.insert((ty, field), val.clone());
        val
    }

    pub fn get_vwt(&mut self, ty: ir::ty::Ty) -> Val {
        if let Some(val) = self.cache.vwt.get(&ty) {
            return val.clone();
        }

        let info = self.get_type_info(ty);
        let vwt = info.field(self, 0).deref(self);

        self.cache.vwt.insert(ty, vwt.clone());
        vwt
    }

    pub fn get_type_info(&mut self, ty: ir::ty::Ty) -> Val {
        if let Some(val) = self.cache.type_info.get(&ty) {
            return val.clone();
        }

        let lookup = ty.lookup(self.db);

        if let typ::Var(var) = lookup.kind {
            return self.generic_params[var.idx()].clone();
        }

        let mut mcx = self.cx.middle_ctx();
        let ptr_type = self.module.target_config().pointer_type();
        let id = self.cx.middle.info_of(&mut mcx, self.db, ty);
        let val = match self.cx.middle[id] {
            | TypeInfo::External(data) => {
                let gv = self.cx.module.declare_data_in_func(data, &mut self.bcx.func);

                self.bcx.ins().global_value(ptr_type, gv)
            },
            | TypeInfo::Trivial { .. } | TypeInfo::Concrete { .. } => {
                let data = self.cx.middle.info_alloc(id);
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
                        | Subst::Type(ty) => self.get_type_info(*ty).as_ptr().get_addr(self),
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
        };

        let ty = self.typ().ptr(self.db);
        let layout = self.db.layout_of(ty);
        let val = Val::new_val(val, layout);

        self.cache.type_info.insert(ty, val.clone());
        val
    }

    // pub fn get_trivial_meta(&mut self, size: u64) -> Option<Val> {
    //     let ptr_type = self.module.target_config().pointer_type();
    //     let trivial_metas = self.cx.trivial_metas();
    //     let idx = match size {
    //         | 0 => 0,
    //         | 1 => 1,
    //         | 2 => 2,
    //         | 4 => 3,
    //         | 8 => 4,
    //         | 16 => 5,
    //         | _ => return None,
    //     };

    //     let typ = self.typ();
    //     let layout = self.db.layout_of(typ);
    //     let gv = self.cx.module.declare_data_in_func(trivial_metas, &mut self.bcx.func);
    //     let addr = self.bcx.ins().global_value(ptr_type, gv);
    //     let addr = self.bcx.ins().iadd_imm(addr, layout.stride.bytes() as i64 * idx);
    //     let layout = self.db.layout_of(typ.ptr(self.db));

    //     Some(Val::new_val(addr, layout))
    // }

    pub fn dynamic_size(&mut self, mut value: clif::Value, ty: ir::ty::Ty) -> clif::Value {
        if let Some(val) = self.cache.size_of.get(&ty) {
            return *val;
        }

        let mut changed = false;

        for i in 0..self.body.generic_params.len() {
            let count = ty.var_count(self.db, GenericVar(0, i as u8));

            if count > 0 {
                let stride = self.get_vwt_field(ty, 2).load(self);

                if count > 1 {
                    let stride = self.bcx.ins().imul_imm(stride, count as i64);

                    value = self.bcx.ins().iadd(value, stride);
                } else {
                    value = self.bcx.ins().iadd(value, stride);
                }

                changed = true;
            }
        }

        if changed {
            self.cache.size_of.insert(ty, value);
        }

        value
    }

    pub fn dynamic_offset(&mut self, ptr: Pointer, layout: &TyAndLayout, field: usize) -> Pointer {
        match layout.ty.lookup(self.db).kind {
            | typ::Tuple(ref fields) => self.dynamic_offset_internal(ptr, fields, layout, field),
            | typ::Def(id, ref subst) => {
                let subst = subst.as_deref().unwrap_or(&[]);
                let info = id.lookup(self.db);
                let body = info.body.as_ref().unwrap();
                let fields = match body {
                    | TypeDefBody::Struct { fields } => fields.iter().map(|f| f.ty.subst(self.db, subst, 0)).collect::<Vec<_>>(),
                    | _ => unreachable!(),
                };

                self.dynamic_offset_internal(ptr, &fields, layout, field)
            },
            | _ => unreachable!(),
        }
    }

    fn dynamic_offset_internal(&mut self, mut ptr: Pointer, fields: &[Ty], layout: &TyAndLayout, field: usize) -> Pointer {
        if let Fields::Arbitrary { offsets, memory_index } = &layout.fields {
            let inverse_index = ir::layout::invert_mapping(memory_index);
            let mut offset = Size::ZERO;

            for (i, j) in inverse_index.into_iter().enumerate() {
                if j == field {
                    break;
                }

                offset = offsets[i];

                for g in 0..self.body.generic_params.len() {
                    let count = fields[j].var_count(self.db, GenericVar(0, g as u8));

                    if count > 0 {
                        let stride = self.get_vwt_field(fields[j], 2).load(self);

                        if count > 1 {
                            let stride = self.bcx.ins().imul_imm(stride, count as i64);

                            ptr = ptr.offset_value(self, stride);
                        } else {
                            ptr = ptr.offset_value(self, stride);
                        }
                    }
                }
            }

            ptr.offset_i64(self, offset.bytes() as i64)
        } else {
            ptr.offset_i64(self, layout.fields.offset(field).bytes() as i64)
        }
    }
}
