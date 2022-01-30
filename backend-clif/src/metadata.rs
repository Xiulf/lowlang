use super::*;
use ir::{
    layout::{Fields, Size, TyAndLayout},
    ty::typ,
    Flags, TypeDefBody,
};

impl<'a, 'db, 'ctx> BodyCtx<'a, 'db, 'ctx> {
    pub fn get_type_metadata(&mut self, ty: ir::ty::Ty) -> Val {
        let lookup = ty.lookup(self.db);

        if let typ::Var(var) = lookup.kind {
            if var.depth() == 0 {
                return self.generic_params[var.idx()].clone();
            } else {
                unimplemented!();
            }
        } else if lookup.flags.is_set(Flags::TRIVIAL) {
            let layout = self.db.layout_of(ty);
            let size = layout.size.bytes();

            if let Some(val) = self.get_trivial_meta(size) {
                return val;
            }
        }

        todo!();
        // let middle = self.cx.middle();
        // let id = {
        //     let info = middle.type_infos().get(&middle, ty);
        //     dbg!(&info);
        //     info.alloc(&middle)
        // };
        // let gv = self.cx.module.declare_data_in_func(id, &mut self.cx.ctx.func);
        // let ptr_type = self.module.target_config().pointer_type();
        // let ty = self.typ().ptr(self.db);
        // let layout = self.db.layout_of(ty);
        // let value = self.bcx.ins().global_value(ptr_type, gv);

        // Val::new_val(value, layout)
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
