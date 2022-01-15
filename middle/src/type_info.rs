use crate::{BackendMethods, ValueWitnessTables};
use ir::db::IrDatabase;
use ir::layout::{Fields, Integer};
use ir::ty::{Subst, Ty, typ};
use ir::Flags;
use rustc_hash::FxHashMap;
use std::cell::{RefCell, Ref};
use std::lazy::OnceCell;

pub struct TypeInfos<DataId> {
    type_info_ty: OnceCell<Ty>,
    ty_to_data: RefCell<FxHashMap<Ty, DataId>>,
    ty_to_info: RefCell<FxHashMap<Ty, TypeInfo<DataId>>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeInfo<DataId> {
    pub ty: Ty,
    pub vwt: DataId,
    pub flags: Flags,
    pub generics: Vec<Option<DataId>>,
    pub fields: Vec<u64>,
}

impl<DataId> Default for TypeInfos<DataId> {
    fn default() -> Self {
        Self {
            type_info_ty: OnceCell::new(),
            ty_to_data: RefCell::new(FxHashMap::default()),
            ty_to_info: RefCell::new(FxHashMap::default()),
        }
    }
}

impl<DataId> TypeInfos<DataId> {
    pub fn type_info_ty<FuncId>(&self, db: &dyn IrDatabase, vwts: &ValueWitnessTables<DataId, FuncId>) -> Ty {
        *self.type_info_ty.get_or_init(|| {
            let vwt_ptr = vwts.vwt_ty(db).ptr(db);
            let int = Ty::int(db, Integer::ISize, false);

            Ty::tuple(db, [vwt_ptr, int])
        })
    }

    pub fn get<B>(&self, b: &B, ty: Ty) -> Ref<TypeInfo<DataId>>
    where
        B: BackendMethods<DataId = DataId>,
        DataId: Copy,
    {
        {
            let borrow = self.ty_to_info.borrow();

            if borrow.contains_key(&ty) {
                return Ref::map(borrow, |map| &map[&ty]);
            }
        }

        let vwt = b.value_witness_tables().get(b, ty).alloc(b);
        let lookup = ty.lookup(b.db());
        let layout = b.db().layout_of(ty);
        let flags = lookup.flags;

        let generics = match lookup.kind {
            | typ::Generic(ref params, _) => vec![None; params.len()],
            | typ::Def(id, None) => vec![None; id.lookup(b.db()).generic_params.len()],
            | typ::Def(_, Some(ref args)) => args.iter().map(|arg| match *arg {
                Subst::Type(ty) => if b.db().layout_of(ty).abi.is_unsized() {
                    None
                } else {
                    Some(self.get(b, ty).alloc(b))
                },
                _ => todo!(),
            }).collect(),
            | _ => Vec::new(),
        };

        let fields = match layout.fields {
            | Fields::Arbitrary { ref offsets, .. } => offsets.iter().map(|o| o.bytes()).collect(),
            | _ => Vec::new(),
        };

        let info = TypeInfo { ty, vwt, flags, generics, fields };

        self.ty_to_info.borrow_mut().insert(ty, info);

        let borrow = self.ty_to_info.borrow();

        Ref::map(borrow, |map| &map[&ty])
    }
}

impl<DataId> TypeInfo<DataId> {
    pub fn alloc<B>(&self, b: &B) -> DataId
    where
        B: BackendMethods<DataId = DataId>,
        DataId: Copy,
    {
        if let Some(&id) = b.type_infos().ty_to_data.borrow().get(&self.ty) {
            return id;
        }

        let id = b.alloc_type_info(self);

        b.type_infos().ty_to_data.borrow_mut().insert(self.ty, id);
        id
    }

    pub fn is_complete(&self) -> bool {
        self.generics.iter().all(Option::is_some)
    }

    pub fn ty<FuncId>(&self, db: &dyn IrDatabase, infos: &TypeInfos<DataId>, vwts: &ValueWitnessTables<DataId, FuncId>) -> Ty {
        let vwt = vwts.vwt_ty(db);
        let ty = infos.type_info_ty(db, vwts).ptr(db);
        let int = Ty::int(db, Integer::ISize, false);
        let mut fields = vec![vwt, int];

        for _ in 0..self.generics.len() {
            fields.push(ty);
        }

        for _ in 0..self.fields.len() {
            fields.push(int);
        }

        Ty::tuple(db, fields)
    }

    #[inline]
    pub fn field_index(&self, idx: usize) -> usize {
        2 + self.generics.len() + idx
    }

    #[inline]
    pub fn field_offset(&self, idx: usize) -> u64 {
        self.fields[idx]
    }
}
