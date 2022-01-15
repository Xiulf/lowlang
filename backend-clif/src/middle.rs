use crate::clif;
use cranelift::{
    codegen::{
        binemit::{NullStackMapSink, NullTrapSink},
        ir::Endianness,
    },
    prelude::{isa::TargetIsa, AbiParam, Value},
};
use cranelift_module::{DataContext, DataId, FuncId, Linkage, Module};
use cranelift_object::ObjectModule;
use ir::{db::IrDatabase, layout::Align};
use middle::{BackendMethods, FnBuilder, TypeInfos, ValueWitnessTables};
use std::lazy::OnceCell;
use target_lexicon::PointerWidth;

pub struct MiddleCtx<'module> {
    pub(crate) db: &'module dyn IrDatabase,
    pub(crate) module: &'module mut ObjectModule,
    pub(crate) ctx: clif::Context,
    pub(crate) fcx: clif::FunctionBuilderContext,
    pub(crate) type_infos: &'module TypeInfos<clif::DataId>,
    pub(crate) value_witness_tables: &'module ValueWitnessTables<clif::DataId, clif::FuncId>,

    pub(crate) copy_trivial: &'module OnceCell<FuncId>,
    pub(crate) move_trivial: &'module OnceCell<FuncId>,
    pub(crate) drop_trivial: &'module OnceCell<FuncId>,
}

pub struct FnCtx<'ctx, 'module> {
    cx: &'ctx mut MiddleCtx<'module>,
    bcx: clif::FunctionBuilder<'ctx>,
    params: Vec<Value>,
    func: FuncId,
}

fn push_int(target: &dyn TargetIsa, bytes: &mut Vec<u8>, int: u64) {
    match (target.endianness(), target.pointer_width()) {
        | (Endianness::Big, PointerWidth::U16) => bytes.extend((int as u16).to_be_bytes()),
        | (Endianness::Big, PointerWidth::U32) => bytes.extend((int as u32).to_be_bytes()),
        | (Endianness::Big, PointerWidth::U64) => bytes.extend((int as u64).to_be_bytes()),
        | (Endianness::Little, PointerWidth::U16) => bytes.extend((int as u16).to_le_bytes()),
        | (Endianness::Little, PointerWidth::U32) => bytes.extend((int as u32).to_le_bytes()),
        | (Endianness::Little, PointerWidth::U64) => bytes.extend((int as u64).to_le_bytes()),
    }
}

impl<'module> BackendMethods for MiddleCtx<'module> {
    type DataId = DataId;
    type FuncId = FuncId;
    type FnBuilder<'ctx>
    where
        'module: 'ctx,
    = FnCtx<'ctx, 'module>;

    fn db(&self) -> &dyn ir::db::IrDatabase {
        self.db
    }

    fn type_infos(&self) -> &middle::TypeInfos<clif::DataId> {
        &self.type_infos
    }

    fn value_witness_tables(&self) -> &middle::ValueWitnessTables<clif::DataId, clif::FuncId> {
        &self.value_witness_tables
    }

    fn alloc_type_info(&self, type_info: &middle::TypeInfo<clif::DataId>) -> Self::DataId {
        let module = unsafe { &mut *(self.module as *const _ as *mut ObjectModule) };
        let id = module.declare_anonymous_data(false, false).unwrap();
        let target = module.isa();
        let mut dcx = DataContext::new();
        let mut bytes = Vec::new();

        push_int(target, &mut bytes, 0);
        push_int(target, &mut bytes, type_info.flags.0 as u64);

        for _ in 0..type_info.generics.len() {
            push_int(target, &mut bytes, 0);
        }

        for &offset in &type_info.fields {
            push_int(target, &mut bytes, offset);
        }

        let ptr_size = target.pointer_bytes() as u32;
        let vwt = module.declare_data_in_data(type_info.vwt, &mut dcx);

        dcx.write_data_addr(0, vwt, 0);

        for (i, &gen) in type_info.generics.iter().enumerate() {
            if let Some(id) = gen {
                let gv = module.declare_data_in_data(id, &mut dcx);

                dcx.write_data_addr((i + 2) as u32 * ptr_size, gv, 0);
            }
        }

        dcx.define(bytes.into_boxed_slice());
        module.define_data(id, &dcx).unwrap();

        id
    }

    fn alloc_value_witness_table(&self, vwt: &middle::ValueWitnessTable<clif::FuncId>) -> Self::DataId {
        let module = unsafe { &mut *(self.module as *const _ as *mut ObjectModule) };
        let id = module.declare_anonymous_data(false, false).unwrap();
        let target = module.isa();
        let mut dcx = DataContext::new();
        let mut bytes = Vec::new();

        push_int(target, &mut bytes, vwt.size);
        push_int(target, &mut bytes, vwt.align);
        push_int(target, &mut bytes, vwt.stride);
        push_int(target, &mut bytes, 0);
        push_int(target, &mut bytes, 0);
        push_int(target, &mut bytes, 0);

        let ptr_size = target.pointer_bytes() as u32;
        let copy_fn = module.declare_func_in_data(vwt.copy_fn, &mut dcx);
        let move_fn = module.declare_func_in_data(vwt.move_fn, &mut dcx);
        let drop_fn = module.declare_func_in_data(vwt.drop_fn, &mut dcx);

        dcx.write_function_addr(3 * ptr_size, copy_fn);
        dcx.write_function_addr(4 * ptr_size, move_fn);
        dcx.write_function_addr(5 * ptr_size, drop_fn);

        dcx.define(bytes.into_boxed_slice());
        module.define_data(id, &dcx).unwrap();

        id
    }

    fn create_vwt_fn<'a>(&'a mut self, nparams: usize) -> Self::FnBuilder<'a> {
        let ptr_ty = self.module.target_config().pointer_type();
        let mut sig = self.module.make_signature();
        for _ in 0..nparams {
            sig.params.push(AbiParam::new(ptr_ty));
        }
        let func = unsafe { &mut *(&mut self.ctx.func as *mut _) };
        let fcx = unsafe { &mut *(&mut self.fcx as *mut _) };
        let mut bcx = clif::FunctionBuilder::new(func, fcx);
        let block = bcx.create_block();
        bcx.switch_to_block(block);
        let params = (0..nparams).map(|_| bcx.append_block_param(block, ptr_ty)).collect();
        let func = self.module.declare_anonymous_function(&sig).unwrap();

        FnCtx { cx: self, bcx, params, func }
    }

    fn copy_trivial(&mut self) -> Self::FuncId {
        let module = &mut self.module;

        *self.copy_trivial.get_or_init(|| {
            let ptr_type = module.target_config().pointer_type();
            let mut sig = module.make_signature();

            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(ptr_type));

            module.declare_function("copy_trivial", Linkage::Import, &sig).unwrap()
        })
    }

    fn move_trivial(&mut self) -> Self::FuncId {
        let module = &mut self.module;

        *self.move_trivial.get_or_init(|| {
            let ptr_type = module.target_config().pointer_type();
            let mut sig = module.make_signature();

            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(ptr_type));

            module.declare_function("move_trivial", Linkage::Import, &sig).unwrap()
        })
    }

    fn drop_trivial(&mut self) -> Self::FuncId {
        let module = &mut self.module;

        *self.drop_trivial.get_or_init(|| {
            let ptr_type = module.target_config().pointer_type();
            let mut sig = module.make_signature();

            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(ptr_type));

            module.declare_function("drop_trivial", Linkage::Import, &sig).unwrap()
        })
    }
}

impl<'ctx, 'module> FnBuilder<'ctx, MiddleCtx<'module>> for FnCtx<'ctx, 'module> {
    type Value = Value;

    fn get_param(&self, param: usize) -> Self::Value {
        self.params[param]
    }

    fn memcopy(&mut self, src: Self::Value, dst: Self::Value, len: Result<u64, Self::Value>) {
        let config = self.cx.module.target_config();

        match len {
            | Ok(bytes) => {
                let align = Align::from_bytes(bytes);
                let align = align.bytes() as u8;

                self.bcx
                    .emit_small_memory_copy(config, dst, src, bytes, align, align, true, clif::MemFlags::new());
            },
            | Err(value) => {
                self.bcx.call_memcpy(config, dst, src, value);
            },
        }
    }

    fn memmove(&mut self, src: Self::Value, dst: Self::Value, len: Result<u64, Self::Value>) {
        let config = self.cx.module.target_config();

        match len {
            | Ok(bytes) => {
                let align = Align::from_bytes(bytes);
                let align = align.bytes() as u8;

                self.bcx
                    .emit_small_memory_copy(config, dst, src, bytes, align, align, false, clif::MemFlags::new());
            },
            | Err(value) => {
                self.bcx.call_memmove(config, dst, src, value);
            },
        }
    }

    fn finish(self) -> <MiddleCtx<'module> as BackendMethods>::FuncId {
        self.cx
            .module
            .define_function(self.func, &mut self.cx.ctx, &mut NullTrapSink {}, &mut NullStackMapSink {})
            .unwrap();
        self.func
    }
}
