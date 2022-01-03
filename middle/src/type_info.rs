use std::lazy::OnceCell;

use crate::{BackendMethods, ValueWitnessTables};
use ir::db::IrDatabase;
use ir::layout::Integer;
use ir::ty::Ty;
use ir::Flags;
use rustc_hash::FxHashMap;

pub struct TypeInfos<B: BackendMethods> {
    type_info_ty: OnceCell<Ty>,
    ty_to_data: FxHashMap<Ty, B::DataId>,
    data_to_info: FxHashMap<B::DataId, TypeInfo<B>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeInfo<B: BackendMethods> {
    pub vwt: B::DataId,
    pub flags: Flags,
    pub generics: usize,
    pub fields: usize,
}

impl<B: BackendMethods> TypeInfos<B> {
    pub fn type_info_ty(&self, db: &dyn IrDatabase) -> Ty {
        *self.type_info_ty.get_or_init(|| {
            let int = Ty::int(db, Integer::ISize, false);

            Ty::tuple(db, [int, int])
        })
    }

    pub fn get(&mut self, b: &mut B, ty: Ty) -> B::DataId {
        // let vwt = b.value_witness_tables().get(b, ty);
        todo!();
    }
}

impl<B: BackendMethods> TypeInfo<B> {
    pub fn ty(&self, db: &dyn IrDatabase, infos: &TypeInfos<B>, vwts: &ValueWitnessTables<B>) -> Ty {
        let vwt = vwts.vwt_ty(db);
        let ty = infos.type_info_ty(db);
        let int = Ty::int(db, Integer::ISize, false);
        let mut fields = vec![vwt, int];

        for _ in 0..self.generics {
            fields.push(ty);
        }

        for _ in 0..self.fields {
            fields.push(int);
        }

        Ty::tuple(db, fields)
    }

    pub fn field_index(&self, idx: usize) -> usize {
        2 + self.generics + idx
    }
}
