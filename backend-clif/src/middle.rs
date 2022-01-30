use std::collections::HashMap;

use super::*;
use ::middle::{fns::FnBuilder, Backend};
use ir::ty::{Ty, TypeKind};

pub type State<'db> = ::middle::State<MiddleCtx<'db>>;

pub struct MiddleCtx<'db> {
    db: &'db dyn IrDatabase,
    module: *mut clif::ObjectModule,
}

struct FnCtx<'module, 'ctx> {
    bcx: clif::FunctionBuilder<'ctx>,
    db: &'module dyn IrDatabase,
    module: &'module mut clif::ObjectModule,
    params: Vec<clif::Value>,
    info_cache: HashMap<Ty, clif::Value>,
    load_cache: HashMap<(clif::Value, i32), clif::Value>,
    sig_cache: HashMap<usize, cranelift::codegen::ir::SigRef>,
}

impl<'db> MiddleCtx<'db> {
    pub(super) fn new(db: &'db dyn IrDatabase, module: &mut clif::ObjectModule) -> Self {
        Self { db, module }
    }

    #[inline]
    fn module(&mut self) -> &mut clif::ObjectModule {
        unsafe { &mut *self.module }
    }
}

impl<'module, 'ctx> FnCtx<'module, 'ctx> {
    fn new(bcx: clif::FunctionBuilder<'ctx>, db: &'module dyn IrDatabase, module: &'module mut clif::ObjectModule) -> Self {
        Self {
            bcx,
            db,
            module,
            params: Vec::new(),
            info_cache: HashMap::new(),
            load_cache: HashMap::new(),
            sig_cache: HashMap::new(),
        }
    }
}

impl<'db> Backend for MiddleCtx<'db> {
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

        let id = self
            .module()
            .declare_function(name, if export { clif::Linkage::Export } else { clif::Linkage::Local }, &sig)
            .unwrap();

        ctx.func.signature = sig;
        ctx.func.name = clif::ExternalName::user(0, id.as_u32());
        ctx.func.collect_debug_info();

        let mut fx = FnCtx::new(clif::FunctionBuilder::new(&mut ctx.func, &mut fcx), self.db, unsafe { &mut *self.module });
        let entry = fx.bcx.create_block();

        fx.bcx.switch_to_block(entry);
        fx.bcx.append_block_params_for_function_params(entry);
        fx.params = fx.bcx.block_params(entry).to_vec();

        f(&mut fx);

        fx.bcx.seal_all_blocks();
        fx.bcx.finalize();

        ctx.compute_cfg();
        ctx.compute_domtree();
        ctx.eliminate_unreachable_code(self.module().isa()).unwrap();
        ctx.dce(self.module().isa()).unwrap();
        ctx.domtree.clear();

        eprintln!("{}:", name);
        eprintln!("{}", ctx.func);

        self.module()
            .define_function(id, &mut ctx, &mut clif::NullTrapSink {}, &mut clif::NullStackMapSink {})
            .unwrap();

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

impl<'module, 'ctx> FnBuilder<MiddleCtx<'module>> for FnCtx<'module, 'ctx> {
    fn ptr_size(&self) -> i32 {
        self.module.target_config().pointer_bytes() as i32
    }

    fn param(&self, n: usize) -> clif::Value {
        self.params[n]
    }

    fn type_info(&mut self, ty: ir::ty::Ty, info: clif::Value) -> clif::Value {
        if let Some(value) = self.info_cache.get(&ty) {
            return *value;
        }

        let value = if let TypeKind::Var(var) = ty.lookup(self.db).kind {
            let ptr_size = self.ptr_size() as i32;

            self.load(info, (2 + var.idx() as i32) * ptr_size)
        } else {
            todo!()
        };

        self.info_cache.insert(ty, value);
        value
    }

    fn stack_alloc(&mut self, size: u64) -> clif::Value {
        let ptr_type = self.module.target_config().pointer_type();
        let slot = self.bcx.create_stack_slot(clif::StackSlotData {
            kind: clif::StackSlotKind::ExplicitSlot,
            size: size as u32,
            offset: None,
        });

        self.bcx.ins().stack_addr(ptr_type, slot, 0)
    }

    fn const_int(&mut self, int: u64) -> clif::Value {
        let ptr_type = self.module.target_config().pointer_type();

        self.bcx.ins().iconst(ptr_type, int as i64)
    }

    fn fn_addr(&mut self, id: clif::FuncId) -> clif::Value {
        let ptr_type = self.module.target_config().pointer_type();
        let func_ref = self.module.declare_func_in_func(id, &mut self.bcx.func);

        self.bcx.ins().func_addr(ptr_type, func_ref)
    }

    fn load(&mut self, ptr: clif::Value, offset: i32) -> clif::Value {
        if let Some(value) = self.load_cache.get(&(ptr, offset)) {
            return *value;
        }

        let ptr_type = self.module.target_config().pointer_type();
        let value = self.bcx.ins().load(ptr_type, clif::MemFlags::trusted(), ptr, offset);

        self.load_cache.insert((ptr, offset), value);
        value
    }

    fn store(&mut self, ptr: clif::Value, offset: i32, value: clif::Value) {
        self.bcx.ins().store(clif::MemFlags::trusted(), value, ptr, offset);
    }

    fn add(&mut self, a: clif::Value, b: clif::Value) -> clif::Value {
        self.bcx.ins().iadd(a, b)
    }

    fn mul(&mut self, a: clif::Value, b: clif::Value) -> clif::Value {
        self.bcx.ins().imul(a, b)
    }

    fn offset(&mut self, ptr: clif::Value, value: clif::Value) -> clif::Value {
        self.bcx.ins().iadd(ptr, value)
    }

    fn offset_u64(&mut self, ptr: clif::Value, value: u64) -> clif::Value {
        self.bcx.ins().iadd_imm(ptr, value as i64)
    }

    fn memcopy(&mut self, dst: clif::Value, src: clif::Value, bytes: u64) {
        let config = self.module.target_config();
        let align = ir::layout::Align::from_bytes(bytes).bytes() as u8;

        self.bcx
            .emit_small_memory_copy(config, dst, src, bytes, align, align, true, clif::MemFlags::new());
    }

    fn memmove(&mut self, dst: clif::Value, src: clif::Value, bytes: u64) {
        let config = self.module.target_config();
        let align = ir::layout::Align::from_bytes(bytes).bytes() as u8;

        self.bcx
            .emit_small_memory_copy(config, dst, src, bytes, align, align, false, clif::MemFlags::new());
    }

    fn gt(&mut self, a: clif::Value, b: clif::Value) -> clif::Value {
        self.bcx.ins().icmp(clif::IntCC::UnsignedGreaterThan, a, b)
    }

    fn conditional(&mut self, condition: clif::Value, a: clif::Value, b: clif::Value) -> clif::Value {
        let ptr_type = self.module.target_config().pointer_type();
        let next = self.bcx.create_block();
        let ret = self.bcx.append_block_param(next, ptr_type);

        self.bcx.ins().brz(condition, next, &[b]);
        self.bcx.ins().jump(next, &[a]);
        self.bcx.switch_to_block(next);

        ret
    }

    fn call(&mut self, fn_ptr: clif::Value, args: &[clif::Value]) {
        let sig = if let Some(sig) = self.sig_cache.get(&args.len()) {
            *sig
        } else {
            let ptr_type = self.module.target_config().pointer_type();
            let mut sig = self.module.make_signature();

            sig.params = args.iter().map(|_| clif::AbiParam::new(ptr_type)).collect();

            let sig = self.bcx.import_signature(sig);

            self.sig_cache.insert(args.len(), sig);
            sig
        };

        self.bcx.ins().call_indirect(sig, fn_ptr, args);
    }

    fn ret(&mut self) {
        self.bcx.ins().return_(&[]);
    }
}
