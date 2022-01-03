use crate::{BackendMethods, ValueWitnessTables};
use ir::db::IrDatabase;
use ir::layout::{Fields, Integer};
use ir::ty::{typ, Ty};
use ir::{Flags, TypeDefBody};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::lazy::OnceCell;

#[derive(Default)]
pub struct TypeInfos<B: BackendMethods> {
    type_info_ty: OnceCell<Ty>,
    ty_to_data: RefCell<FxHashMap<Ty, B::DataId>>,
    data_to_info: RefCell<FxHashMap<B::DataId, TypeInfo<B>>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeInfo<B: BackendMethods> {
    pub vwt: B::DataId,
    pub flags: Flags,
    pub generics: usize,
    pub fields: Vec<usize>,
}

impl<B: BackendMethods> TypeInfos<B> {
    pub fn type_info_ty(&self, db: &dyn IrDatabase) -> Ty {
        *self.type_info_ty.get_or_init(|| {
            let int = Ty::int(db, Integer::ISize, false);

            Ty::tuple(db, [int, int])
        })
    }

    pub fn get(&self, b: &B, ty: Ty) -> B::DataId
    where
        B::DataId: Eq + std::hash::Hash,
    {
        {
            let borrow = self.ty_to_data.borrow();

            if let Some(&id) = borrow.get(&ty) {
                return id;
            }
        }

        let vwt = b.value_witness_tables().get(b, ty);
        let lookup = ty.lookup(b.db());
        let layout = b.db().layout_of(ty);
        let flags = lookup.flags;

        let generics = match lookup.kind {
            | typ::Generic(ref params, _) => params.len(),
            | typ::Def(id, None) => id.lookup(b.db()).generic_params.len(),
            | _ => 0,
        };

        let fields = match layout.fields {
            | Fields::Arbitrary { ref offsets, .. } => offsets.iter().map(|o| o.bytes() as usize).collect(),
            | _ => Vec::new(),
        };

        let info = TypeInfo { vwt, flags, generics, fields };
        let id = b.alloc_type_info(&info);

        self.ty_to_data.borrow_mut().insert(ty, id);
        self.data_to_info.borrow_mut().insert(id, info);

        id
    }
}

impl<B: BackendMethods> TypeInfo<B> {
    pub fn ty(&self, db: &dyn IrDatabase, infos: &TypeInfos<B>, vwts: &ValueWitnessTables<B>) -> Ty {
        let vwt = vwts.vwt_ty(db);
        let ty = infos.type_info_ty(db).ptr(db);
        let int = Ty::int(db, Integer::ISize, false);
        let mut fields = vec![vwt, int];

        for _ in 0..self.generics {
            fields.push(ty);
        }

        for _ in 0..self.fields.len() {
            fields.push(int);
        }

        Ty::tuple(db, fields)
    }

    #[inline]
    pub fn field_index(&self, idx: usize) -> usize {
        2 + self.generics + idx
    }

    #[inline]
    pub fn field_offset(&self, idx: usize) -> usize {
        self.fields[self.field_index(idx)]
    }
}
