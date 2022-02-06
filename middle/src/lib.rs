use ir::{
    db::IrDatabase,
    layout::{Align, Size},
    ty::{Ty, TypeKind},
    Flags, Linkage, TypeDefId,
};
use mangling::mangle;
use rustc_hash::FxHashMap;
use std::hash::Hash;

pub mod fns;

pub trait Backend {
    type DataId: Copy + Eq + Hash;
    type FuncId: Copy + Eq + Hash;
    type Value: Clone;

    fn import_data(&mut self, name: &str) -> Self::DataId;
    fn import_fn(&mut self, name: &str, nparams: usize) -> Self::FuncId;

    fn mk_fn(&mut self, name: &str, export: bool, nparams: usize, build: impl FnOnce(&mut dyn fns::FnBuilder<Self>)) -> Self::FuncId;

    fn copy_trivial(&mut self) -> Self::FuncId;
    fn move_trivial(&mut self) -> Self::FuncId;
    fn copy_move_nop(&mut self) -> Self::FuncId;
    fn drop_nop(&mut self) -> Self::FuncId;
}

pub struct State<B: Backend> {
    type_infos: Vec<TypeInfo<B>>,
    value_witness_tables: Vec<ValueWitnessTable<B>>,
    ty_to_ti: FxHashMap<Ty, TypeInfoId>,
    def_to_ti: FxHashMap<TypeDefId, TypeInfoId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeInfoId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueWitnessTableId(usize);

pub enum TypeInfo<B: Backend> {
    External(B::DataId),
    Trivial {
        vwt: ValueWitnessTableId,
    },
    Concrete {
        vwt: ValueWitnessTableId,
    },
    Generic {
        copy_fn: B::FuncId,
        move_fn: B::FuncId,
        drop_fn: B::FuncId,
        mk_generics: B::FuncId,
        mk_vwt: B::FuncId,
        mk_info: B::FuncId,
        generic_count: usize,
        info_field_count: usize,
    },
}

pub struct ValueWitnessTable<B: Backend> {
    pub size: Size,
    pub align: Align,
    pub stride: Size,
    pub copy_fn: B::FuncId,
    pub move_fn: B::FuncId,
    pub drop_fn: B::FuncId,
}

impl<B: Backend> Default for State<B> {
    fn default() -> Self {
        Self {
            type_infos: Vec::new(),
            value_witness_tables: Vec::new(),
            ty_to_ti: FxHashMap::default(),
            def_to_ti: FxHashMap::default(),
        }
    }
}

impl<B: Backend> State<B> {
    pub fn register_types(&mut self, backend: &mut B, db: &dyn IrDatabase, module: &ir::Module) {
        for local_type in &module.types {
            self.register_type(backend, db, &module.name, local_type.linkage, local_type.id);
        }
    }

    pub fn info_of(&mut self, backend: &mut B, db: &dyn IrDatabase, ty: Ty) -> TypeInfoId {
        if let Some(id) = self.ty_to_ti.get(&ty) {
            return *id;
        }

        let lookup = ty.lookup(db);

        if lookup.flags.is_set(Flags::TRIVIAL) {
            let layout = db.layout_of(ty);

            self.alloc_trivial(backend, layout.size, layout.align)
        } else {
            match lookup.kind {
                | TypeKind::Box(..) => todo!(),
                | TypeKind::Def(id, _) => self.def_to_ti[&id],
                | _ => unimplemented!(),
            }
        }
    }

    pub fn register_type(&mut self, backend: &mut B, db: &dyn IrDatabase, module: &str, linkage: Linkage, id: TypeDefId) {
        let def = id.lookup(db);
        let ty = Ty::def(db, id, None);
        let layout = db.layout_of(ty);

        if linkage == Linkage::Import {
            if def.generic_params.is_empty() {
                let name = mangle(format!("{}#{}", module, def.name).as_bytes());
                let data_id = backend.import_data(&name);
                let info = self.alloc_info(TypeInfo::External(data_id));

                self.def_to_ti.insert(id, info);
            } else {
                let mut import = |name: &str, nparams: usize| {
                    let name = mangle(format!("{}#{}.{}", module, def.name, name).as_bytes());
                    backend.import_fn(&name, nparams)
                };

                let copy_fn = import("copy", 3);
                let move_fn = import("move", 3);
                let drop_fn = import("drop", 2);
                let mk_generics = import("generics", def.generic_params.len() + 1);
                let mk_vwt = import("vwt", 2);
                let mk_info = import("info", 3);
                let info = self.alloc_info(TypeInfo::Generic {
                    copy_fn,
                    move_fn,
                    drop_fn,
                    mk_generics,
                    mk_vwt,
                    mk_info,
                    generic_count: def.generic_params.len(),
                    info_field_count: 2 + def.generic_params.len(),
                });

                self.def_to_ti.insert(id, info);
            }
        } else if def.is_trivial(db) {
            let info = self.alloc_trivial(backend, layout.size, layout.align);

            self.def_to_ti.insert(id, info);
        } else if def.generic_params.is_empty() {
            let copy_fn = self.generate_def_copy(backend, db, module, linkage, &def, &layout);
            let move_fn = self.generate_def_move(backend, db, module, linkage, &def, &layout);
            let drop_fn = self.generate_def_drop(backend, db, module, linkage, &def, &layout);
            let vwt = self.alloc_vwt(ValueWitnessTable {
                size: layout.size,
                align: layout.align,
                stride: layout.stride,
                copy_fn,
                move_fn,
                drop_fn,
            });

            let info = self.alloc_info(TypeInfo::Concrete { vwt });

            self.def_to_ti.insert(id, info);
        } else {
            let copy_fn = self.generate_def_copy(backend, db, module, linkage, &def, &layout);
            let move_fn = self.generate_def_move(backend, db, module, linkage, &def, &layout);
            let drop_fn = self.generate_def_drop(backend, db, module, linkage, &def, &layout);
            let mk_generics = self.generate_mk_generics(backend, db, module, linkage, &def, &layout);
            let mk_vwt = self.generate_mk_vwt(backend, db, module, linkage, &def, &layout, copy_fn, move_fn, drop_fn);
            let mk_info = self.generate_mk_info(backend, db, module, linkage, &def, &layout);
            let info = self.alloc_info(TypeInfo::Generic {
                mk_generics,
                mk_vwt,
                mk_info,
                copy_fn,
                move_fn,
                drop_fn,
                generic_count: def.generic_params.len(),
                info_field_count: 2 + def.generic_params.len(),
            });

            self.def_to_ti.insert(id, info);
        }
    }

    fn alloc_trivial(&mut self, backend: &mut B, size: Size, align: Align) -> TypeInfoId {
        let find_ti = |ti: &TypeInfo<B>| match *ti {
            | TypeInfo::Trivial { vwt } => {
                let vwt = &self.value_witness_tables[vwt.0];
                vwt.size == size && vwt.align == align
            },
            | _ => false,
        };

        if let Some(idx) = self.type_infos.iter().position(find_ti) {
            TypeInfoId(idx)
        } else {
            let vwt = self.trivial_vwt(backend, size, align);

            self.alloc_info(TypeInfo::Trivial { vwt })
        }
    }

    fn trivial_vwt(&mut self, backend: &mut B, size: Size, align: Align) -> ValueWitnessTableId {
        let vwt = if size == Size::ZERO {
            ValueWitnessTable {
                size,
                align,
                stride: Size::ZERO,
                copy_fn: backend.copy_move_nop(),
                move_fn: backend.copy_move_nop(),
                drop_fn: backend.drop_nop(),
            }
        } else {
            ValueWitnessTable {
                size,
                align,
                stride: size.align_to(align),
                copy_fn: backend.copy_trivial(),
                move_fn: backend.move_trivial(),
                drop_fn: backend.drop_nop(),
            }
        };

        self.alloc_vwt(vwt)
    }

    fn alloc_info(&mut self, info: TypeInfo<B>) -> TypeInfoId {
        let id = TypeInfoId(self.type_infos.len());

        self.type_infos.push(info);
        id
    }

    fn alloc_vwt(&mut self, vwt: ValueWitnessTable<B>) -> ValueWitnessTableId {
        let id = ValueWitnessTableId(self.value_witness_tables.len());

        self.value_witness_tables.push(vwt);
        id
    }
}

impl<B: Backend> std::ops::Index<TypeInfoId> for State<B> {
    type Output = TypeInfo<B>;

    fn index(&self, index: TypeInfoId) -> &Self::Output {
        &self.type_infos[index.0]
    }
}

impl<B: Backend> std::ops::Index<ValueWitnessTableId> for State<B> {
    type Output = ValueWitnessTable<B>;

    fn index(&self, index: ValueWitnessTableId) -> &Self::Output {
        &self.value_witness_tables[index.0]
    }
}
