use crate::BackendMethods;
use ir::Flags;
use ir::db::IrDatabase;
use ir::layout::Integer;
use ir::ty::{Signature, Ty};
use rustc_hash::FxHashMap;
use std::cell::{Ref, RefCell};
use std::lazy::OnceCell;

pub struct ValueWitnessTables<DataId, FuncId> {
    vwt_ty: OnceCell<Ty>,
    ty_to_data: RefCell<FxHashMap<Ty, DataId>>,
    ty_to_vwt: RefCell<FxHashMap<Ty, ValueWitnessTable<FuncId>>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueWitnessTable<FuncId> {
    pub ty: Ty,
    pub size: u64,
    pub align: u64,
    pub stride: u64,
    pub copy_fn: FuncId,
    pub move_fn: FuncId,
    pub drop_fn: FuncId,
}

impl<DataId, FuncId> Default for ValueWitnessTables<DataId, FuncId> {
    fn default() -> Self {
        Self {
            vwt_ty: OnceCell::new(),
            ty_to_data: RefCell::new(FxHashMap::default()),
            ty_to_vwt: RefCell::new(FxHashMap::default()),
        }
    }
}

impl<DataId, FuncId> ValueWitnessTables<DataId, FuncId> {
    pub fn vwt_ty(&self, db: &dyn IrDatabase) -> Ty {
        *self.vwt_ty.get_or_init(|| {
            let int = Ty::int(db, Integer::ISize, false);
            let copy_fn = Signature::new().param(db, int).param(db, int).param(db, int).to_ty(db);
            let drop_fn = Signature::new().param(db, int).param(db, int).to_ty(db);

            Ty::tuple(db, [int, int, int, copy_fn, copy_fn, drop_fn])
        })
    }

    pub fn get<B>(&self, b: &B, ty: Ty) -> Ref<ValueWitnessTable<FuncId>>
    where
        B: BackendMethods<DataId = DataId, FuncId = FuncId>,
    {
        {
            let borrow = self.ty_to_vwt.borrow();

            if borrow.contains_key(&ty) {
                return Ref::map(borrow, |map| &map[&ty]);
            }
        }

        let b = unsafe { &mut *(b as *const B as *mut B) };
        let layout = b.db().layout_of(ty);
        let size = layout.size.bytes();
        let align = layout.align.bytes();
        let stride = layout.stride.bytes();
        let copy_fn = copy_fn(b, ty);
        let move_fn = move_fn(b, ty);
        let drop_fn = drop_fn(b, ty);
        let vwt = ValueWitnessTable {
            ty,
            size,
            align,
            stride,
            copy_fn,
            move_fn,
            drop_fn,
        };

        self.ty_to_vwt.borrow_mut().insert(ty, vwt);

        let borrow = self.ty_to_vwt.borrow();

        Ref::map(borrow, |map| &map[&ty])
    }
}

impl<FuncId> ValueWitnessTable<FuncId> {
    pub fn alloc<B>(&self, b: &B) -> B::DataId
    where
        B: BackendMethods<FuncId = FuncId>,
    {
        if let Some(&id) = b.value_witness_tables().ty_to_data.borrow().get(&self.ty) {
            return id;
        }

        let id = b.alloc_value_witness_table(self);

        b.value_witness_tables().ty_to_data.borrow_mut().insert(self.ty, id);
        id
    }
}

fn copy_fn<B: BackendMethods>(b: &mut B, ty: Ty) -> B::FuncId {
    if ty.lookup(b.db()).flags.is_set(Flags::TRIVIAL) {
        b.copy_trivial()
    } else {
        todo!()
    }
}

fn move_fn<B: BackendMethods>(b: &mut B, ty: Ty) -> B::FuncId {
    if ty.lookup(b.db()).flags.is_set(Flags::TRIVIAL) {
        b.move_trivial()
    } else {
        todo!()
    }
}

fn drop_fn<B: BackendMethods>(b: &mut B, ty: Ty) -> B::FuncId {
    if ty.lookup(b.db()).flags.is_set(Flags::TRIVIAL) {
        b.drop_trivial()
    } else {
        todo!()
    }
}
