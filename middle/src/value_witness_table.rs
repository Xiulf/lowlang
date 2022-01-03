use std::lazy::OnceCell;

use crate::BackendMethods;
use ir::db::IrDatabase;
use ir::layout::Integer;
use ir::ty::{Signature, Ty};
use rustc_hash::FxHashMap;

pub struct ValueWitnessTables<B: BackendMethods> {
    vwt_ty: OnceCell<Ty>,
    ty_to_data: FxHashMap<Ty, B::DataId>,
    data_to_vwt: FxHashMap<B::DataId, ValueWitnessTable<B>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueWitnessTable<B: BackendMethods> {
    pub size: usize,
    pub align: usize,
    pub stride: usize,
    pub copy_fn: B::FuncId,
    pub move_fn: B::FuncId,
    pub drop_fn: B::FuncId,
}

impl<B: BackendMethods> ValueWitnessTables<B> {
    pub fn vwt_ty(&self, db: &dyn IrDatabase) -> Ty {
        *self.vwt_ty.get_or_init(|| {
            let int = Ty::int(db, Integer::ISize, false);
            let copy_fn = Signature::new().param(db, int).param(db, int).param(db, int).to_ty(db);
            let drop_fn = Signature::new().param(db, int).param(db, int).to_ty(db);

            Ty::tuple(db, [int, int, int, copy_fn, drop_fn])
        })
    }

    pub fn get(&self, b: &B, ty: Ty) -> B::DataId {
        todo!();
    }
}
