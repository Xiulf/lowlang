use ir::{
    layout::{Fields, Size, TyAndLayout},
    ty::typ,
    TypeDefBody,
};

use super::*;

impl<'a, 'ctx> BodyCtx<'a, 'ctx> {
    pub fn find_type_metadata(&mut self, ty: ir::ty::Ty) -> Option<Val> {
        if let ir::ty::typ::Var(var) = ty.lookup(self.db).kind {
            if var.depth() == 0 {
                Some(self.generic_params[var.idx()].clone())
            } else {
                unimplemented!();
            }
        } else {
            // for now assume the type is trivial
            let layout = self.db.layout_of(ty);
            let size = layout.size.bytes();

            self.get_trivial_meta(size)
        }
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
