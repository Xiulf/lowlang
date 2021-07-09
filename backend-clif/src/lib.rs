mod intrinsic;
mod pass;
mod ptr;
mod ty;
mod value;

use arena::{ArenaMap, Idx};
use clif::InstBuilder;
use cranelift_module::Module;
use ir::db::IrDatabase;
use ptr::Pointer;
use std::io::Write;
use tempfile::NamedTempFile;
use value::Val;

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
    vars: ArenaMap<Idx<ir::VarInfo>, value::Val>,
    rets: Vec<Option<Pointer>>,
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
            rets: Vec::new(),
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

        let mut params = Vec::new();

        self.rets = sig2
            .rets
            .iter()
            .map(|ret| {
                if ret.flags.is_set(ir::Flags::OUT) {
                    let val = self.bcx.append_block_param(entry, ptr_type);

                    params.push(val);
                    None
                } else {
                    let layout = self.db.layout_of(ret.ty);

                    match self.pass_mode(&layout) {
                        | pass::PassMode::ByRef { .. } => {
                            let val = self.bcx.append_block_param(entry, ptr_type);

                            Some(Pointer::addr(val))
                        },
                        | _ => None,
                    }
                }
            })
            .collect();

        for param in &sig2.params {
            if param.flags.is_set(ir::Flags::IN) {
                let val = self.bcx.append_block_param(entry, ptr_type);

                params.push(val);
            } else {
                let layout = self.db.layout_of(param.ty);

                match self.pass_mode(&layout) {
                    | pass::PassMode::NoPass => {},
                    | pass::PassMode::ByVal(t) => {
                        let val = self.bcx.append_block_param(entry, t);

                        params.push(val);
                    },
                    | pass::PassMode::ByValPair(a, b) => {
                        let a = self.bcx.append_block_param(entry, a);
                        let b = self.bcx.append_block_param(entry, b);

                        params.push(a);
                        params.push(b);
                    },
                    | pass::PassMode::ByRef { .. } => {
                        let val = self.bcx.append_block_param(entry, ptr_type);

                        params.push(val);
                    },
                }
            }
        }

        for (idx, block) in self.body.blocks.iter() {
            let id = self.bcx.create_block();

            for &param in &block.params {
                let layout = self.db.layout_of(self.body[param].ty);
                let val = match self.pass_mode(&layout) {
                    | pass::PassMode::NoPass => Val::new_zst(layout),
                    | pass::PassMode::ByVal(t) => {
                        let val = self.bcx.append_block_param(id, t);

                        Val::new_val(val, layout)
                    },
                    | pass::PassMode::ByValPair(a, b) => {
                        let a = self.bcx.append_block_param(id, a);
                        let b = self.bcx.append_block_param(id, b);

                        Val::new_val_pair(a, b, layout)
                    },
                    | pass::PassMode::ByRef { .. } => {
                        let val = self.bcx.append_block_param(id, ptr_type);

                        Val::new_ref(Pointer::addr(val), layout)
                    },
                };

                self.vars.insert(param.0, val);
            }

            self.blocks.insert(idx, id);
        }

        self.bcx.ins().jump(self.blocks[ir::Block::ENTRY.0], &params);
        self.bcx.seal_block(entry);

        for (idx, block) in self.body.blocks.iter() {
            let bb = self.blocks[idx];

            self.lower_block(bb, ir::Block(idx), block);
        }

        self.bcx.seal_all_blocks();
        self.bcx.finalize();
        self.cx.ctx.compute_cfg();
        self.cx.ctx.compute_domtree();
        self.cx.ctx.eliminate_unreachable_code(self.cx.module.isa()).unwrap();

        eprintln!("{}", self.bcx.func);

        self.cx
            .module
            .define_function(id, self.cx.ctx, &mut clif::NullTrapSink {}, &mut clif::NullStackMapSink {})
            .unwrap();
        self.cx.ctx.clear();
    }

    fn lower_block(&mut self, bb: clif::Block, _id: ir::Block, block: &ir::BlockData) {
        self.bcx.switch_to_block(bb);

        for instr in &block.instrs {
            self.lower_instr(instr);
        }

        self.lower_term(block.term.as_ref().unwrap());
    }

    fn lower_term(&mut self, term: &ir::Term) {
        match *term {
            | ir::Term::Unreachable => {
                self.bcx.ins().trap(clif::TrapCode::UnreachableCodeReached);
            },
            | ir::Term::Return { ref vals } => {
                let vals = vals
                    .iter()
                    .zip(self.rets.clone())
                    .filter_map(|(v, r)| match r {
                        | Some(ptr) => {
                            self.vars[v.0].clone().store_to(self, ptr);
                            None
                        },
                        | None => Some(self.value_for_ret(self.vars[v.0].clone())),
                    })
                    .flatten()
                    .collect::<Vec<_>>();

                self.bcx.ins().return_(&vals);
            },
            | ir::Term::Br { ref to } => {
                let args = to.args.iter().flat_map(|a| self.value_for_arg(self.vars[a.0].clone())).collect::<Vec<_>>();

                self.bcx.ins().jump(self.blocks[to.block.0], &args);
            },
            | ir::Term::Switch { pred, ref cases, ref default } => {
                let mut switch = clif::Switch::new();
                let pred = self.vars[pred.0].clone().load(self);

                for case in cases {
                    switch.set_entry(case.val, self.blocks[case.to.block.0]);
                }

                switch.emit(&mut self.bcx, pred, self.blocks[default.block.0]);
            },
        }
    }

    fn lower_instr(&mut self, instr: &ir::Instr) {
        let ptr_type = self.module.target_config().pointer_type();

        match *instr {
            | ir::Instr::Load { ret, addr } => {
                let addr = self.vars[addr.0].clone().load(self);
                let layout = self.db.layout_of(self.body[ret].ty);
                let res = Val::new_ref(Pointer::addr(addr), layout);

                self.vars.insert(ret.0, res);
            },
            | ir::Instr::ConstInt { ret, val } => {
                let layout = self.db.layout_of(self.body[ret].ty);
                let val = Val::new_const(self, val, layout);

                self.vars.insert(ret.0, val);
            },
            | ir::Instr::FuncRef { ret, func } => {
                let (id, _) = self.func_ids[func.0];
                let func = self.cx.module.declare_func_in_func(id, &mut self.cx.ctx.func);
                let val = self.bcx.ins().func_addr(ptr_type, func);
                let layout = self.db.layout_of(self.body[ret].ty);

                self.vars.insert(ret.0, Val::new_val(val, layout));
            },
            | ir::Instr::Apply {
                ref rets,
                func,
                ref args,
                subst: _,
            } => {
                let mut ret_ptrs = Vec::new();
                let mut sig = self.cx.module.make_signature();

                for &ret in rets {
                    let layout = self.db.layout_of(self.body[ret].ty);

                    match self.pass_mode(&layout) {
                        | pass::PassMode::NoPass => {},
                        | pass::PassMode::ByVal(t) => {
                            sig.returns.push(clif::AbiParam::new(t));
                        },
                        | pass::PassMode::ByValPair(a, b) => {
                            sig.returns.push(clif::AbiParam::new(a));
                            sig.returns.push(clif::AbiParam::new(b));
                        },
                        | pass::PassMode::ByRef { .. } => {
                            let ss = self.bcx.create_stack_slot(clif::StackSlotData {
                                kind: clif::StackSlotKind::ExplicitSlot,
                                size: layout.size.bytes() as u32,
                                offset: None,
                            });

                            ret_ptrs.push(Pointer::stack(ss));
                            sig.params.push(clif::AbiParam::new(ptr_type));
                        },
                    }
                }

                let args = args
                    .iter()
                    .flat_map(|arg| match self.pass_mode(self.vars[arg.0].layout()) {
                        | pass::PassMode::NoPass => pass::EmptySinglePair::Empty,
                        | pass::PassMode::ByVal(t) => {
                            sig.params.push(clif::AbiParam::new(t));
                            pass::EmptySinglePair::Single(self.vars[arg.0].clone().load(self))
                        },
                        | pass::PassMode::ByValPair(a, b) => {
                            sig.params.push(clif::AbiParam::new(a));
                            sig.params.push(clif::AbiParam::new(b));

                            let (a, b) = self.vars[arg.0].clone().load_pair(self);

                            pass::EmptySinglePair::Pair(a, b)
                        },
                        | pass::PassMode::ByRef { .. } => {
                            sig.params.push(clif::AbiParam::new(ptr_type));

                            match self.vars[arg.0].clone().on_stack(self) {
                                | (ptr, None) => pass::EmptySinglePair::Single(ptr.get_addr(self)),
                                | (ptr, Some(meta)) => pass::EmptySinglePair::Pair(ptr.get_addr(self), meta),
                            }
                        },
                    })
                    .collect::<Vec<_>>();

                let args = ret_ptrs.iter().map(|p| p.get_addr(self)).chain(args).collect::<Vec<_>>();
                let func = self.vars[func.0].clone().load(self);
                let sig = self.bcx.import_signature(sig);
                let call = self.bcx.ins().call_indirect(sig, func, &args);
                let mut res = self.bcx.inst_results(call).to_vec().into_iter();
                let mut ret_ptrs = ret_ptrs.into_iter();

                for &ret in rets {
                    let layout = self.db.layout_of(self.body[ret].ty);

                    match self.pass_mode(&layout) {
                        | pass::PassMode::NoPass => {
                            self.vars.insert(ret.0, Val::new_zst(layout));
                        },
                        | pass::PassMode::ByVal(_) => {
                            self.vars.insert(ret.0, Val::new_val(res.next().unwrap(), layout));
                        },
                        | pass::PassMode::ByValPair(_, _) => {
                            self.vars.insert(ret.0, Val::new_val_pair(res.next().unwrap(), res.next().unwrap(), layout));
                        },
                        | pass::PassMode::ByRef { .. } => {
                            self.vars.insert(ret.0, Val::new_ref(ret_ptrs.next().unwrap(), layout));
                        },
                    }
                }
            },
            | ir::Instr::Intrinsic {
                ref rets, ref name, ref args, ..
            } => {
                let args = args.iter().map(|a| self.vars[a.0].clone()).collect();

                self.lower_intrinsic(name.as_str(), rets, args);
            },
            | _ => unimplemented!("{}", instr.display(self.db, self.body)),
        }
    }
}
