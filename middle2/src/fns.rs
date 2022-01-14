use super::{State, Backend};
use mangling::mangle;
use ir::{
    TypeDef, TypeDefBody, Linkage, Flags,
    db::IrDatabase,
    layout::{TyAndLayout, Fields},
};

pub trait FnBuilder<B: Backend> {
    fn param(&self, n: usize) -> B::Value;

    fn offset(&mut self, ptr: B::Value, offset: B::Value) -> B::Value;
    fn offset_u64(&mut self, ptr: B::Value, offset: u64) -> B::Value;

    fn memcpy(&mut self, dst: B::Value, src: B::Value, bytes: u64);

    fn ret(&mut self);
}

impl<B: Backend> State<B> {
    pub(super) fn generate_def_copy(
        &mut self,
        backend: &mut B,
        db: &dyn IrDatabase,
        module: &str,
        linkage: Linkage,
        def: &TypeDef,
        layout: &TyAndLayout,
    ) -> B::FuncId {
        let name = mangle(format!("{}#{}.copy", module, def.name).as_bytes());
        let export = linkage == Linkage::Export;
        let body = def.body.as_ref().expect("Opaque types cannot be copied");
        let backend_ptr = backend as *mut B;

        backend.mk_fn(&name, export, 3, |fx| {
            let mut dst = fx.param(0);
            let mut src = fx.param(1);

            match body {
                TypeDefBody::Struct { fields } => {
                    if let Fields::Arbitrary { offsets, memory_index } = &layout.fields {
                        let indices = ir::layout::invert_mapping(memory_index);

                        for i in indices {
                            if fields[i].ty.lookup(db).flags.is_set(Flags::TRIVIAL) {
                                let layout = db.layout_of(fields[i].ty);

                                fx.memcpy(dst.clone(), src.clone(), layout.size.bytes());
                                dst = fx.offset_u64(dst, layout.stride.bytes());
                                src = fx.offset_u64(src, layout.stride.bytes());
                            } else {
                                //todo!();
                            }
                        }

                        fx.ret();
                    } else {
                        unreachable!();
                    }
                },
                TypeDefBody::Enum { variants } => todo!(),
                TypeDefBody::Union { .. } => unreachable!("Unions must be trivial types"),
            }
        })
    }

    pub(super) fn generate_def_move(&mut self, backend: &mut B, db: &dyn IrDatabase, module: &str, linkage: Linkage, def: &TypeDef) -> B::FuncId {
        let name = mangle(format!("{}#{}.move", module, def.name).as_bytes());
        let export = linkage == Linkage::Export;
        let body = def.body.as_ref().expect("Opaque types cannot be moved");

        backend.mk_fn(&name, export, 3, |fx| match body {
            TypeDefBody::Struct { fields } => {},
            TypeDefBody::Enum { variants } => todo!(),
            TypeDefBody::Union { .. } => unreachable!("Unions must be trivial types"),
        })
    }

    pub(super) fn generate_def_drop(&mut self, backend: &mut B, db: &dyn IrDatabase, module: &str, linkage: Linkage, def: &TypeDef) -> B::FuncId {
        let name = mangle(format!("{}#{}.drop", module, def.name).as_bytes());
        let export = linkage == Linkage::Export;
        let body = def.body.as_ref().expect("Opaque types cannot be dropped");

        backend.mk_fn(&name, export, 2, |fx| match body {
            TypeDefBody::Struct { fields } => {},
            TypeDefBody::Enum { variants } => todo!(),
            TypeDefBody::Union { .. } => unreachable!("Unions must be trivial types"),
        })
    }
}