mod pass;
mod ptr;
mod ty;
mod value;

use arena::{ArenaMap, Idx};
use cranelift_module::Module;
use ir::db::IrDatabase;
use std::io::Write;
use tempfile::NamedTempFile;

mod clif {
    pub use cranelift::codegen::binemit::{NullRelocSink, NullStackMapSink, NullTrapSink};
    pub use cranelift::codegen::*;
    pub use cranelift::frontend::*;
    pub use cranelift::prelude::*;
    pub use cranelift_module::*;
    pub use cranelift_object::{ObjectBuilder, ObjectModule};
}

#[no_mangle]
pub fn compile_module(db: &dyn IrDatabase, ir: &ir::Module, object_file: &mut NamedTempFile) {
    with_codegen_ctx(db, ir, |mut ctx| {
        for (id, func) in ir.funcs.iter() {
            ctx.declare_func(id, func);
        }

        for (id, func) in ir.funcs.iter() {
            if let Some(body) = func.body {
                ctx.lower_body(id, body);
            }
        }

        let product = ctx.module.finish();
        let bytes = product.emit().unwrap();

        object_file.write_all(&bytes).unwrap();
    })
}

pub struct CodegenCtx<'ctx> {
    db: &'ctx dyn IrDatabase,
    ir: &'ctx ir::Module,
    ctx: &'ctx mut clif::Context,
    fcx: &'ctx mut clif::FunctionBuilderContext,
    module: clif::ObjectModule,
    func_ids: ArenaMap<Idx<ir::Func>, (clif::FuncId, clif::Signature)>,
}

pub struct BodyCtx<'a, 'ctx> {
    cx: &'a mut CodegenCtx<'ctx>,
    bcx: clif::FunctionBuilder<'ctx>,
    func: Idx<ir::Func>,
    body: &'ctx ir::Body,
    blocks: ArenaMap<Idx<ir::BlockData>, clif::Block>,
    vars: ArenaMap<Idx<ir::VarInfo>, clif::Value>,
}

impl<'a, 'ctx> std::ops::Deref for BodyCtx<'a, 'ctx> {
    type Target = CodegenCtx<'ctx>;

    fn deref(&self) -> &Self::Target {
        self.cx
    }
}

pub fn with_codegen_ctx<T>(db: &dyn IrDatabase, ir: &ir::Module, f: impl FnOnce(CodegenCtx) -> T) -> T {
    let mut ctx = clif::Context::new();
    let mut fcx = clif::FunctionBuilderContext::new();
    let triple = db.triple();
    let flags_builder = clif::settings::builder();
    let flags = clif::settings::Flags::new(flags_builder);
    let isa = clif::isa::lookup((*triple).clone()).unwrap().finish(flags);
    let builder = clif::ObjectBuilder::new(isa, ir.name.as_bytes(), clif::default_libcall_names()).unwrap();
    let module = clif::ObjectModule::new(builder);
    let ctx = CodegenCtx {
        db,
        ir,
        ctx: &mut ctx,
        fcx: &mut fcx,
        module,
        func_ids: ArenaMap::default(),
    };

    f(ctx)
}

impl<'ctx> CodegenCtx<'ctx> {
    pub fn declare_func(&mut self, id: Idx<ir::Func>, func: &ir::Func) {
        let sig = self.ty_as_sig(func.sig);
        let new_id = self
            .module
            .declare_function(
                &func.name,
                match func.linkage {
                    | ir::Linkage::Import => clif::Linkage::Import,
                    | ir::Linkage::Export => clif::Linkage::Export,
                    | ir::Linkage::Local => clif::Linkage::Local,
                },
                &sig,
            )
            .unwrap();

        self.func_ids.insert(id, (new_id, sig));
    }

    fn lower_body(&mut self, id: Idx<ir::Func>, body: ir::BodyId) {
        let func = unsafe { &mut *(&mut self.ctx.func as *mut _) };
        let fcx = unsafe { &mut *(self.fcx as *mut _) };
        let bcx = clif::FunctionBuilder::new(func, fcx);

        BodyCtx {
            bcx,
            func: id,
            body: &self.ir[body],
            cx: self,
            blocks: ArenaMap::default(),
            vars: ArenaMap::default(),
        }
        .lower();
    }
}

impl<'a, 'ctx> BodyCtx<'a, 'ctx> {
    fn lower(&mut self) {
        let (id, sig) = self.func_ids[self.func].clone();
        let (_, sig2) = self.ir.funcs[self.func].sig.get_sig(self.db);
        let ptr_type = self.module.target_config().pointer_type();
        let entry = self.bcx.create_block();

        self.bcx.func.signature = sig;
        self.bcx.switch_to_block(entry);

        for ret in &sig2.rets {
            let layout = self.db.layout_of(ret.ty);

            match self.pass_mode(&layout) {
                | pass::PassMode::NoPass => {},
                | pass::PassMode::ByVal(t) => {
                    let val = self.bcx.append_block_param(entry, t);
                },
                | pass::PassMode::ByValPair(a, b) => {
                    let a = self.bcx.append_block_param(entry, a);
                    let b = self.bcx.append_block_param(entry, b);
                },
                | pass::PassMode::ByRef { .. } => {
                    let val = self.bcx.append_block_param(entry, ptr_type);
                },
            }
        }
    }
}
