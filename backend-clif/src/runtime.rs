use super::*;
use ir::{layout::Integer, ty::{Signature, Ty}};
use std::lazy::OnceCell;

#[derive(Default)]
pub(super) struct RuntimeDefs {
    // runtime/gen_alloc
    gen_alloc: OnceCell<clif::FuncId>,
    gen_free: OnceCell<clif::FuncId>,

    // runtime/metadata
    vwt: OnceCell<Ty>,
    typ: OnceCell<Ty>,
    trivial_metas: OnceCell<clif::DataId>,

    pub(super) copy_trivial: OnceCell<clif::FuncId>,
    pub(super) move_trivial: OnceCell<clif::FuncId>,
    pub(super) drop_trivial: OnceCell<clif::FuncId>,
}

impl<'db, 'ctx> CodegenCtx<'db, 'ctx> {
    pub fn gen_alloc(&mut self) -> clif::FuncId {
        let module = &mut self.module;

        *self.runtime_defs.gen_alloc.get_or_init(|| {
            let size_t = module.target_config().pointer_type();
            let mut sig = module.make_signature();

            sig.params.push(clif::AbiParam::new(size_t));
            sig.returns.push(clif::AbiParam::new(size_t));
            sig.returns.push(clif::AbiParam::new(size_t));

            module.declare_function("gen_alloc", clif::Linkage::Import, &sig).unwrap()
        })
    }

    pub fn gen_free(&mut self) -> clif::FuncId {
        let module = &mut self.module;

        *self.runtime_defs.gen_free.get_or_init(|| {
            let size_t = module.target_config().pointer_type();
            let mut sig = module.make_signature();

            sig.params.push(clif::AbiParam::new(size_t));
            sig.params.push(clif::AbiParam::new(size_t));

            module.declare_function("gen_free", clif::Linkage::Import, &sig).unwrap()
        })
    }

    pub fn trivial_metas(&mut self) -> clif::DataId {
        let module = &mut self.module;

        *self
            .runtime_defs
            .trivial_metas
            .get_or_init(|| module.declare_data("TRIVIAL_METAS", clif::Linkage::Import, false, false).unwrap())
    }

    pub fn vwt(&self) -> Ty {
        *self.runtime_defs.vwt.get_or_init(|| {
            let vwt = ir::TypeDef::declare(self.db, "ValueWitnessTable");
            let typ = ir::TypeDef::declare(self.db, "Type");
            let opaque = ir::TypeDef::declare(self.db, "Opaque");
            let opaque = Ty::def(self.db, opaque, None).ptr(self.db);
            let size_t = Ty::int(self.db, Integer::ISize, false);
            let typ_ptr = Ty::def(self.db, typ, None).ptr(self.db);
            let copy_ty = Signature::new().param(self.db, opaque).param(self.db, opaque).param(self.db, typ_ptr).to_ty(self.db);
            let drop_ty = Signature::new().param(self.db, opaque).param(self.db, typ_ptr).to_ty(self.db);
            let fields = [
                ir::TypeDefField {
                    name: "size".into(),
                    ty: size_t,
                },
                ir::TypeDefField {
                    name: "align".into(),
                    ty: size_t,
                },
                ir::TypeDefField {
                    name: "stride".into(),
                    ty: size_t,
                },
                ir::TypeDefField {
                    name: "copy".into(),
                    ty: copy_ty,
                },
                ir::TypeDefField {
                    name: "move".into(),
                    ty: copy_ty,
                },
                ir::TypeDefField {
                    name: "drop".into(),
                    ty: drop_ty,
                },
            ];

            vwt.lookup(self.db).define_struct(fields);

            let ty = Ty::def(self.db, vwt, None).flag(self.db, ir::Flags::C_REPR);

            self.init_typ(typ, ty);
            ty
        })
    }

    pub fn typ(&self) -> Ty {
        self.vwt();
        *self.runtime_defs.typ.get().unwrap()
    }

    fn init_typ(&self, typ: ir::TypeDefId, vwt: Ty) {
        let byte = Ty::int(self.db, Integer::I8, false);
        let vwt = vwt.ptr(self.db);
        let fields = [ir::TypeDefField { name: "vwt".into(), ty: vwt }, ir::TypeDefField {
            name: "flags".into(),
            ty: byte,
        }];

        typ.lookup(self.db).define_struct(fields);

        let ty = Ty::def(self.db, typ, None).flag(self.db, ir::Flags::C_REPR);

        self.runtime_defs.typ.set(ty).unwrap();
    }
}
