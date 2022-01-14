use super::*;
use ::middle2::{Backend, fns::FnBuilder};

pub type State = ::middle2::State<MiddleCtx>;

pub struct MiddleCtx {
    module: *mut clif::ObjectModule,
}

struct FnCtx<'module, 'ctx> {
    bcx: clif::FunctionBuilder<'ctx>,
    module: &'module mut clif::ObjectModule,
    params: Vec<clif::Value>,
}

impl MiddleCtx {
    pub(super) fn new(module: &mut clif::ObjectModule) -> Self {
         Self { module }
    }

    #[inline]
    fn module(&mut self) -> &mut clif::ObjectModule {
        unsafe { &mut *self.module }
    }
}

impl Backend for MiddleCtx {
    type DataId = clif::DataId;
    type FuncId = clif::FuncId;
    type Value = clif::Value;

    fn import_data(&mut self, name: &str) -> Self::DataId {
        self.module().declare_data(name, clif::Linkage::Import, false, false).unwrap()
    }

    fn import_fn(&mut self, name: &str, nparams: usize) -> Self::FuncId {
        let mut sig = self.module().make_signature();
        let ptr_type = self.module().target_config().pointer_type();

        sig.params = (0..nparams).map(|_| clif::AbiParam::new(ptr_type)).collect();

        self.module().declare_function(name, clif::Linkage::Import, &sig).unwrap()
    }

    fn mk_fn(&mut self, name: &str, export: bool, nparams: usize, f: impl FnOnce(&mut dyn FnBuilder<Self>)) -> Self::FuncId {
        let mut ctx = self.module().make_context();
        let mut fcx = clif::FunctionBuilderContext::new();
        let mut sig = self.module().make_signature();
        let ptr_type = self.module().target_config().pointer_type();

        sig.params = (0..nparams).map(|_| clif::AbiParam::new(ptr_type)).collect();

        let id = self.module().declare_function(name, if export {
            clif::Linkage::Export
        } else {
            clif::Linkage::Local
        }, &sig).unwrap();

        ctx.func.signature = sig;

        let mut fcx = FnCtx {
            bcx: clif::FunctionBuilder::new(&mut ctx.func, &mut fcx),
            module: unsafe { &mut *self.module },
            params: Vec::new(),
        };

        let entry = fcx.bcx.create_block();

        fcx.bcx.switch_to_block(entry);
        fcx.bcx.append_block_params_for_function_params(entry);
        fcx.params = fcx.bcx.block_params(entry).to_vec();

        f(&mut fcx);

        fcx.bcx.seal_all_blocks();
        fcx.bcx.finalize();

        eprintln!("{}:", name);
        eprintln!("{}", ctx.func);

        self.module().define_function(id, &mut ctx, &mut clif::NullTrapSink {}, &mut clif::NullStackMapSink {}).unwrap();

        id
    }

    fn copy_trivial(&mut self) -> Self::FuncId {
        todo!()
    }

    fn move_trivial(&mut self) -> Self::FuncId {
        todo!()
    }

    fn copy_move_nop(&mut self) -> Self::FuncId {
        todo!()
    }

    fn drop_nop(&mut self) -> Self::FuncId {
        todo!()
    }
}

impl<'module, 'ctx> FnBuilder<MiddleCtx> for FnCtx<'module, 'ctx> {
    fn param(&self, n: usize) -> clif::Value {
        self.params[n]
    }

    fn offset(&mut self, ptr: clif::Value, value: clif::Value) -> clif::Value {
        self.bcx.ins().iadd(ptr, value)
    }

    fn offset_u64(&mut self, ptr: clif::Value, value: u64) -> clif::Value {
        self.bcx.ins().iadd_imm(ptr, value as i64)
    }

    fn memcpy(&mut self, dst: clif::Value, src: clif::Value, bytes: u64) {
        let config = self.module.target_config();
        let align = ir::layout::Align::from_bytes(bytes).bytes() as u8;

        self.bcx.emit_small_memory_copy(config, dst, src, bytes, align, align, true, clif::MemFlags::new());
    }

    fn ret(&mut self) {
        self.bcx.ins().return_(&[]);
    }
}