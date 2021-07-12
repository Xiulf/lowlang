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
}
