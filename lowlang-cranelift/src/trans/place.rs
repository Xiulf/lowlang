use crate::{FunctionCtx, Backend};
use crate::ptr::Pointer;
use crate::place::Place;
use lowlang_syntax as syntax;
use cranelift_codegen::ir::InstBuilder;

impl<'a, 't, 'l, B: Backend> FunctionCtx<'a, 't, 'l, B> {
    pub fn trans_place(&mut self, place: &syntax::Place) -> Place<'t, 'l> {
        let mut res = match &place.base {
            syntax::PlaceBase::Local(id) => self.locals[id],
            syntax::PlaceBase::Global(id) => {
                let (data_id, layout) = self.data_ids[&id.id()];
                let local_data_id = self.module.declare_data_in_func(data_id, self.builder.func);
                let global_ptr = self.builder.ins().global_value(self.pointer_type, local_data_id);

                Place::new_ref(Pointer::addr(global_ptr), layout)
            },
        };

        for elem in &place.elems {
            match elem {
                syntax::PlaceElem::Deref => res = res.deref(self),
                syntax::PlaceElem::Field(idx) => res = res.field(self, *idx),
                syntax::PlaceElem::ConstIndex(idx) => res = res.const_index(self, *idx),
                syntax::PlaceElem::Index(place) => {
                    let idx = self.trans_place(place);
                    let idx = idx.to_value(self).load_scalar(self);

                    res = res.index(self, idx);
                },
            }
        }

        res
    }
}
