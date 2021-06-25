use super::CodegenCtx;
use inkwell::{types, values, AddressSpace};
use std::lazy::OnceCell;

#[derive(Default)]
pub(super) struct RuntimeDefs<'ctx> {
    // runtime/gen_alloc
    gen_alloc: OnceCell<values::FunctionValue<'ctx>>,
    gen_free: OnceCell<values::FunctionValue<'ctx>>,

    // runtime/metadata
    vwt: OnceCell<types::StructType<'ctx>>,
    type_: OnceCell<types::StructType<'ctx>>,
    copy_trivial: OnceCell<values::FunctionValue<'ctx>>,
    move_trivial: OnceCell<values::FunctionValue<'ctx>>,
    drop_trivial: OnceCell<values::FunctionValue<'ctx>>,
    trivial_metas: OnceCell<values::PointerValue<'ctx>>,
}

impl<'ctx> CodegenCtx<'ctx> {
    pub fn gen_alloc(&self) -> values::FunctionValue<'ctx> {
        *self.runtime_defs.gen_alloc.get_or_init(|| {
            let size_t = self.context.ptr_sized_int_type(&self.target_data, None);
            let unit = self.context.opaque_struct_type("UNIT").ptr_type(AddressSpace::Generic);
            let ret = self.context.struct_type(&[unit.into(), size_t.into()], false);
            let sig = ret.fn_type(&[size_t.into()], false);

            self.module.add_function("gen_alloc", sig, None)
        })
    }

    pub fn gen_free(&self) -> values::FunctionValue<'ctx> {
        *self.runtime_defs.gen_free.get_or_init(|| {
            let size_t = self.context.ptr_sized_int_type(&self.target_data, None);
            let unit = self.context.opaque_struct_type("UNIT").ptr_type(AddressSpace::Generic);
            let void = self.context.void_type();
            let sig = void.fn_type(&[unit.into(), size_t.into()], false);

            self.module.add_function("gen_free", sig, None)
        })
    }

    pub fn vwt(&self) -> types::StructType<'ctx> {
        *self.runtime_defs.vwt.get_or_init(|| {
            let size_t = self.context.ptr_sized_int_type(&self.target_data, None);
            let opaque = self.context.opaque_struct_type("Opaque").ptr_type(AddressSpace::Generic);
            let void = self.context.void_type();
            let type_ = self.context.opaque_struct_type("Type");
            let type_ptr = type_.ptr_type(AddressSpace::Generic);
            let copy_ty = void
                .fn_type(&[opaque.into(), opaque.into(), type_ptr.into()], false)
                .ptr_type(AddressSpace::Generic);
            let drop_ty = void.fn_type(&[opaque.into(), type_ptr.into()], false).ptr_type(AddressSpace::Generic);
            let vwt = self.context.struct_type(
                &[size_t.into(), size_t.into(), size_t.into(), copy_ty.into(), copy_ty.into(), drop_ty.into()],
                false,
            );

            self.init_type(type_, vwt);

            vwt
        })
    }

    pub fn type_(&self) -> types::StructType<'ctx> {
        self.vwt();
        *self.runtime_defs.type_.get().unwrap()
    }

    fn init_type(&self, type_: types::StructType<'ctx>, vwt: types::StructType<'ctx>) {
        let byte = self.context.i8_type();
        let vwt = vwt.ptr_type(AddressSpace::Generic);

        type_.set_body(&[vwt.into(), byte.into()], false);

        self.runtime_defs.type_.set(type_).unwrap();
    }

    pub fn copy_trivial(&self) -> values::FunctionValue<'ctx> {
        *self.runtime_defs.copy_trivial.get_or_init(|| {
            let void = self.context.void_type();
            let type_ = self.type_().ptr_type(AddressSpace::Generic);
            let opaque = self.context.opaque_struct_type("Opaque").ptr_type(AddressSpace::Generic);
            let sig = void.fn_type(&[opaque.into(), opaque.into(), type_.into()], false);

            self.module.add_function("copy_trivial", sig, None)
        })
    }

    pub fn move_trivial(&self) -> values::FunctionValue<'ctx> {
        *self.runtime_defs.move_trivial.get_or_init(|| {
            let void = self.context.void_type();
            let type_ = self.type_().ptr_type(AddressSpace::Generic);
            let opaque = self.context.opaque_struct_type("Opaque").ptr_type(AddressSpace::Generic);
            let sig = void.fn_type(&[opaque.into(), opaque.into(), type_.into()], false);

            self.module.add_function("move_trivial", sig, None)
        })
    }

    pub fn drop_trivial(&self) -> values::FunctionValue<'ctx> {
        *self.runtime_defs.drop_trivial.get_or_init(|| {
            let void = self.context.void_type();
            let type_ = self.type_().ptr_type(AddressSpace::Generic);
            let opaque = self.context.opaque_struct_type("Opaque").ptr_type(AddressSpace::Generic);
            let sig = void.fn_type(&[opaque.into(), type_.into()], false);

            self.module.add_function("drop_trivial", sig, None)
        })
    }

    pub fn trivial_metas(&self) -> values::PointerValue<'ctx> {
        *self.runtime_defs.trivial_metas.get_or_init(|| {
            let type_ = self.type_();
            let ty = type_.array_type(6);
            let global = self.module.add_global(ty, None, "TRIVIAL_METAS");

            global.as_pointer_value()
        })
    }

    pub fn get_trivial_meta(&self, size: u64) -> Option<values::PointerValue<'ctx>> {
        let trivial_metas = self.trivial_metas();
        let size_t = self.context.ptr_sized_int_type(&self.target_data, None);
        let zero = size_t.const_int(0, false);

        match size {
            | 0 => unsafe { Some(self.builder.build_gep(trivial_metas, &[zero, zero], "")) },
            | 1 => {
                let idx = size_t.const_int(1, false);

                unsafe { Some(self.builder.build_gep(trivial_metas, &[zero, idx], "")) }
            },
            | 2 => {
                let idx = size_t.const_int(2, false);

                unsafe { Some(self.builder.build_gep(trivial_metas, &[zero, idx], "")) }
            },
            | 4 => {
                let idx = size_t.const_int(3, false);

                unsafe { Some(self.builder.build_gep(trivial_metas, &[zero, idx], "")) }
            },
            | 8 => {
                let idx = size_t.const_int(4, false);

                unsafe { Some(self.builder.build_gep(trivial_metas, &[zero, idx], "")) }
            },
            | 16 => {
                let idx = size_t.const_int(5, false);

                unsafe { Some(self.builder.build_gep(trivial_metas, &[zero, idx], "")) }
            },
            | _ => None,
        }
    }
}
