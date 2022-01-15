use super::*;
use ::middle::{TypeInfos, ValueWitnessTables};
use std::lazy::OnceCell;

#[derive(Default)]
pub(super) struct RuntimeDefs {
    pub(super) type_infos: TypeInfos<clif::DataId>,
    pub(super) value_witness_tables: ValueWitnessTables<clif::DataId, clif::FuncId>,

    // runtime/gen_alloc
    gen_alloc: OnceCell<clif::FuncId>,
    gen_free: OnceCell<clif::FuncId>,

    // runtime/metadata
    trivial_metas: OnceCell<clif::DataId>,

    pub(super) copy_trivial: OnceCell<clif::FuncId>,
    pub(super) move_trivial: OnceCell<clif::FuncId>,
    pub(super) drop_trivial: OnceCell<clif::FuncId>,
}

impl<'ctx> CodegenCtx<'ctx> {
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
}
