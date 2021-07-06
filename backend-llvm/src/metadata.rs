use crate::BodyCtx;
use inkwell::values;

impl<'a, 'ctx> BodyCtx<'a, 'ctx> {
    pub fn find_type_metadata(&mut self, ty: ir::ty::Ty) -> Option<values::PointerValue<'ctx>> {
        if let ir::ty::typ::Var(var) = ty.lookup(self.db).kind {
            if var.depth() == 0 {
                Some(self.generic_params[var.idx()])
            } else {
                unimplemented!();
            }
        } else {
            // for now assume the type is trivial.
            let layout = self.db.layout_of(ty);
            let size = layout.size.bytes();

            self.cx.get_trivial_meta(size)
        }
    }
}
