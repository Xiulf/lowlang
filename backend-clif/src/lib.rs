#![feature(once_cell)]

mod intrinsic;
mod metadata;
mod middle2;
mod pass;
mod ptr;
mod runtime;
mod ty;
mod value;

use arena::{ArenaMap, Idx};
use clif::InstBuilder;
use cranelift_module::Module;
use ir::db::IrDatabase;
use ir::layout::Abi;
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
        let mut mcx = ctx.middle_ctx();

        ctx.middle.register_types(&mut mcx, db, ir);

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
    middle: middle2::State,
    func_ids: ArenaMap<Idx<ir::Func>, (clif::FuncId, clif::Signature)>,
    runtime_defs: runtime::RuntimeDefs,
}

pub struct BodyCtx<'a, 'ctx> {
    cx: &'a mut CodegenCtx<'ctx>,
    bcx: clif::FunctionBuilder<'ctx>,
    func: Idx<ir::Func>,
    body: &'ctx ir::Body,
    generic_params: Vec<Val>,
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
        middle: ::middle2::State::default(),
        func_ids: ArenaMap::default(),
        runtime_defs: runtime::RuntimeDefs::default(),
    };

    f(ctx)
}

impl<'ctx> CodegenCtx<'ctx> {
    pub fn middle_ctx(&mut self) -> middle2::MiddleCtx {
        middle2::MiddleCtx::new(&mut self.module)
    }

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
            generic_params: Vec::new(),
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
        let (generics, sig2) = self.ir.funcs[self.func].sig.get_sig(self.db);
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

        for gen in generics {
            match gen {
                | ir::ty::GenericParam::Type => {
                    let val = self.bcx.append_block_param(entry, ptr_type);
                    let layout = self.db.layout_of(self.cx.typ());
                    let val = Val::new_ref(Pointer::addr(val), layout);

                    self.generic_params.push(val);
                },
                | _ => unimplemented!(),
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

        eprintln!("{}", self.ir.funcs[self.func].name);
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
                            self.vars[v.0].clone().store_to(self, ptr, clif::MemFlags::trusted());
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
            | ir::Instr::StackAlloc { ret, ty } => {
                let layout = self.db.layout_of(ty);
                let ss = self.bcx.create_stack_slot(clif::StackSlotData {
                    kind: clif::StackSlotKind::ExplicitSlot,
                    size: layout.size.bytes() as u32,
                    offset: None,
                });

                let layout = self.db.layout_of(self.body[ret].ty);

                self.vars.insert(ret.0, Val::new_addr(Pointer::stack(ss), layout));
            },
            | ir::Instr::StackFree { addr: _ } => {},
            | ir::Instr::Load { ret, addr } => {
                let ptr = self.vars[addr.0].clone().as_ptr();
                let layout = self.db.layout_of(self.body[ret].ty);
                let res = Val::new_ref(ptr, layout);

                self.vars.insert(ret.0, res);
            },
            | ir::Instr::Store { val, addr } => {
                let ptr = self.vars[addr.0].clone().as_ptr();
                let val = self.vars[val.0].clone();

                val.store_to(self, ptr, clif::MemFlags::new());
            },
            | ir::Instr::CopyAddr { old, new, flags: _ } => {
                let elem_ty = self.body[old].ty.pointee(self.db).unwrap();
                let old = self.vars[old.0].clone();
                let new = self.vars[new.0].clone();

                if let ir::ty::typ::Var(var) = elem_ty.lookup(self.db).kind {
                    let typ = self.generic_params[var.idx()].clone();
                    let vwt = typ.clone().field(self, 0).deref(self);
                    let copy = vwt.field(self, 3);
                    let sig = self.ty_as_sig(copy.layout().ty);
                    let sig = self.bcx.import_signature(sig);
                    let copy = copy.load(self);
                    let typ = typ.as_ref().0.get_addr(self);
                    let old = old.load(self);
                    let new = new.load(self);

                    self.bcx.ins().call_indirect(sig, copy, &[new, old, typ]);
                } else {
                    let align = old.layout().align.bytes();
                    let size = old.layout().size.bytes();

                    self.emit_memcpy(new.as_ptr(), old.as_ptr(), size, align, true, clif::MemFlags::new());
                }
            },
            | ir::Instr::ConstInt { ret, val } => {
                let layout = self.db.layout_of(self.body[ret].ty);
                let val = Val::new_const(self, val, layout);

                self.vars.insert(ret.0, val);
            },
            | ir::Instr::FuncRef { ret, func } => {
                let (id, _) = self.func_ids[func.0];
                let func = self.cx.module.declare_func_in_func(id, &mut self.cx.ctx.func);
                let layout = self.db.layout_of(self.body[ret].ty);

                self.vars.insert(ret.0, Val::new_func(func, layout));
            },
            | ir::Instr::Tuple { ret, ref vals } => {
                let layout = self.db.layout_of(self.body[ret].ty);
                let res = match &layout.abi {
                    | Abi::Scalar(_) => {
                        assert_eq!(vals.len(), 1);
                        let val = self.vars[vals[0].0].clone().load(self);

                        Val::new_val(val, layout)
                    },
                    | Abi::ScalarPair(_, _) => {
                        assert!(vals.len() <= 2);

                        let (a, b) = if vals.len() == 1 {
                            self.vars[vals[0].0].clone().load_pair(self)
                        } else {
                            let a = self.vars[vals[0].0].clone().load(self);
                            let b = self.vars[vals[1].0].clone().load(self);
                            (a, b)
                        };

                        Val::new_val_pair(a, b, layout)
                    },
                    | _ => {
                        let ss = self.bcx.create_stack_slot(clif::StackSlotData {
                            kind: clif::StackSlotKind::ExplicitSlot,
                            size: layout.size.bytes() as u32,
                            offset: None,
                        });

                        let res = Val::new_ref(Pointer::stack(ss), layout);

                        for (i, val) in vals.iter().enumerate() {
                            let (ptr, _) = res.clone().field(self, i).as_ref();

                            self.vars[val.0].clone().store_to(self, ptr, clif::MemFlags::trusted());
                        }

                        res
                    },
                };

                self.vars.insert(ret.0, res);
            },
            | ir::Instr::TupleExtract { ret, tuple, field } => {
                let field = self.vars[tuple.0].clone().field(self, field);

                self.vars.insert(ret.0, field);
            },
            | ir::Instr::TupleInsert { tuple, field, val } => {
                let tuple_val = self.vars[tuple.0].clone();

                match &tuple_val.layout().abi {
                    | Abi::Scalar(_) => {
                        assert_eq!(field, 0);

                        let val = self.vars[val.0].clone();

                        self.vars.insert(tuple.0, val);
                    },
                    | Abi::ScalarPair(_, _) => {
                        assert!(field < 2);

                        let layout = tuple_val.layout().clone();
                        let val = if field == 0 {
                            let (_, b) = tuple_val.load_pair(self);
                            let a = self.vars[val.0].clone().load(self);

                            Val::new_val_pair(a, b, layout)
                        } else {
                            let (a, _) = tuple_val.load_pair(self);
                            let b = self.vars[val.0].clone().load(self);

                            Val::new_val_pair(a, b, layout)
                        };

                        self.vars.insert(tuple.0, val);
                    },
                    | _ => {
                        let field = tuple_val.field(self, field);
                        let (ptr, _) = field.as_ref();

                        self.vars[val.0].clone().store_to(self, ptr, clif::MemFlags::new());
                    },
                }
            },
            | ir::Instr::TupleAddr { ret, tuple, field } => {
                let layout = self.db.layout_of(self.body[tuple].ty).pointee(self.db);
                let offset = layout.fields.offset(field).bytes() as i64;
                let ptr = self.vars[tuple.0].as_ptr();
                let layout = self.db.layout_of(layout.field(self.db, field).ty.ptr(self.db));
                let addr = Val::new_addr(ptr.offset_i64(self, offset), layout);

                self.vars.insert(ret.0, addr);
            },
            | ir::Instr::Struct { ret, ty, fields: ref vals } => {
                if let ir::ty::typ::Def(id, _) = ty.lookup(self.db).kind {
                    let def = id.lookup(self.db);

                    if let Some(ir::TypeDefBody::Struct { ref fields } | ir::TypeDefBody::Union { ref fields }) = def.body {
                        let layout = self.db.layout_of(ty);
                        let res = match &layout.abi {
                            | Abi::Scalar(_) => {
                                assert_eq!(fields.len(), 1);
                                let val = self.vars[(vals[0].1).0].clone().load(self);

                                Val::new_val(val, layout)
                            },
                            | Abi::ScalarPair(_, _) => {
                                assert!(fields.len() <= 2);

                                let (a, b) = if vals.len() == 1 {
                                    self.vars[(vals[0].1).0].clone().load_pair(self)
                                } else {
                                    let a = self.vars[(vals[0].1).0].clone().load(self);
                                    let b = self.vars[(vals[1].1).0].clone().load(self);

                                    if let Some(0) = fields.iter().position(|f| f.name == vals[0].0) {
                                        (a, b)
                                    } else {
                                        (b, a)
                                    }
                                };

                                Val::new_val_pair(a, b, layout)
                            },
                            | _ => {
                                let ss = self.bcx.create_stack_slot(clif::StackSlotData {
                                    kind: clif::StackSlotKind::ExplicitSlot,
                                    size: layout.size.bytes() as u32,
                                    offset: None,
                                });

                                let res = Val::new_ref(Pointer::stack(ss), layout);

                                for (name, val) in vals {
                                    let idx = fields.iter().position(|f| f.name == *name).unwrap();
                                    let (ptr, _) = res.clone().field(self, idx).as_ref();

                                    self.vars[val.0].clone().store_to(self, ptr, clif::MemFlags::trusted());
                                }

                                res
                            },
                        };

                        self.vars.insert(ret.0, res);
                    }
                }
            },
            | ir::Instr::StructExtract { ret, struc, ref field } => {
                let struc = self.vars[struc.0].clone();

                if let ir::ty::typ::Def(id, _) = struc.layout().ty.lookup(self.db).kind {
                    let def = id.lookup(self.db);

                    match def.body {
                        | Some(ir::TypeDefBody::Struct { ref fields }) => {
                            let field = fields.iter().position(|f| f.name == *field).unwrap();
                            let field = struc.field(self, field);

                            self.vars.insert(ret.0, field);
                        },
                        | Some(ir::TypeDefBody::Union { .. }) => {
                            let layout = self.db.layout_of(self.body[ret].ty);

                            self.vars.insert(ret.0, struc.cast(layout));
                        },
                        | _ => unreachable!(),
                    }
                }
            },
            | ir::Instr::StructInsert { struc, ref field, val } => {
                let struct_val = self.vars[struc.0].clone();

                if let ir::ty::typ::Def(id, _) = struct_val.layout().ty.lookup(self.db).kind {
                    let def = id.lookup(self.db);

                    match def.body {
                        | Some(ir::TypeDefBody::Struct { ref fields }) => match &struct_val.layout().abi {
                            | Abi::Scalar(_) => {
                                let val = self.vars[val.0].clone();

                                self.vars.insert(struc.0, val);
                            },
                            | Abi::ScalarPair(_, _) => {
                                let field = fields.iter().position(|f| f.name == *field).unwrap();
                                let layout = struct_val.layout().clone();
                                let val = if field == 0 {
                                    let (_, b) = struct_val.load_pair(self);
                                    let a = self.vars[val.0].clone().load(self);

                                    Val::new_val_pair(a, b, layout)
                                } else {
                                    let (a, _) = struct_val.load_pair(self);
                                    let b = self.vars[val.0].clone().load(self);

                                    Val::new_val_pair(a, b, layout)
                                };

                                self.vars.insert(struc.0, val);
                            },
                            | _ => {
                                let field = fields.iter().position(|f| f.name == *field).unwrap();
                                let field = struct_val.field(self, field);
                                let (ptr, _) = field.as_ref();

                                self.vars[val.0].clone().store_to(self, ptr, clif::MemFlags::new());
                            },
                        },
                        | Some(ir::TypeDefBody::Union { .. }) => match &struct_val.layout().abi {
                            | Abi::Aggregate { .. } => {
                                let (ptr, _) = struct_val.as_ref();

                                self.vars[val.0].clone().store_to(self, ptr, clif::MemFlags::new());
                            },
                            | _ => {
                                let val = self.vars[val.0].clone();

                                self.vars.insert(struc.0, val);
                            },
                        },
                        | _ => unreachable!(),
                    }
                }
            },
            | ir::Instr::StructAddr { ret, struc, ref field } => {
                let layout = self.db.layout_of(self.body[struc].ty).pointee(self.db);

                if let ir::ty::typ::Def(id, _) = layout.ty.lookup(self.db).kind {
                    let def = id.lookup(self.db);

                    match def.body {
                        | Some(ir::TypeDefBody::Struct { ref fields }) => {
                            let field = fields.iter().position(|f| f.name == *field).unwrap();
                            let ptr = self.vars[struc.0].as_ptr();
                            let ptr = if layout.abi.is_unsized() {
                                self.dynamic_offset(ptr, &layout, field)
                            } else {
                                ptr.offset_i64(self, layout.fields.offset(field).bytes() as i64)
                            };

                            let layout = self.db.layout_of(self.body[ret].ty);
                            let addr = Val::new_addr(ptr, layout);

                            self.vars.insert(ret.0, addr)
                        },
                        | Some(ir::TypeDefBody::Union { .. }) => {
                            let layout = self.db.layout_of(self.body[ret].ty);
                            let addr = self.vars[struc.0].clone().cast(layout);

                            self.vars.insert(ret.0, addr);
                        },
                        | _ => unreachable!(),
                    }
                }
            },
            | ir::Instr::Apply {
                ref rets,
                func,
                ref args,
                ref subst,
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

                            match self.vars[arg.0].clone().as_ref() {
                                | (ptr, None) => pass::EmptySinglePair::Single(ptr.get_addr(self)),
                                | (ptr, Some(meta)) => pass::EmptySinglePair::Pair(ptr.get_addr(self), meta),
                            }
                        },
                    })
                    .collect::<Vec<_>>();

                let gen_args = subst
                    .iter()
                    .flat_map(|sub| match *sub {
                        | ir::ty::Subst::Type(t) => {
                            sig.params.push(clif::AbiParam::new(ptr_type));
                            pass::EmptySinglePair::Single(self.find_type_metadata(t).unwrap().load(self))
                        },
                        | _ => unimplemented!(),
                    })
                    .collect::<Vec<_>>();

                let args = ret_ptrs.iter().map(|p| p.get_addr(self)).chain(args).chain(gen_args).collect::<Vec<_>>();

                let call = if let Some(func) = self.vars[func.0].as_func() {
                    self.bcx.ins().call(func, &args)
                } else {
                    let func = self.vars[func.0].clone().load(self);
                    let sig = self.bcx.import_signature(sig);

                    self.bcx.ins().call_indirect(sig, func, &args)
                };

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

    fn emit_memcpy(&mut self, dst: Pointer, src: Pointer, mut size: u64, mut align: u64, non_overlapping: bool, mut flags: clif::MemFlags) {
        const THRESHOLD: u64 = 4;

        let mut offset = 0;
        let mut registers = Vec::new();

        while size != 0 {
            let access_size = (size as i64 & -(size as i64)) as u64;
            let access_size = access_size.max(align);
            let (access_size, int_type) = if access_size <= 8 {
                (access_size, clif::Type::int((access_size * 8) as u16).unwrap())
            } else {
                (8, clif::types::I64)
            };

            let rest = size % access_size;
            let load_and_store_amount = size / access_size;

            if load_and_store_amount > THRESHOLD {
                let ptr_type = self.module.target_config().pointer_type();
                let size_value = self.bcx.ins().iconst(ptr_type, size as i64);
                let dst = dst.get_addr(self);
                let src = src.get_addr(self);

                if non_overlapping {
                    self.bcx.call_memcpy(self.cx.module.target_config(), dst, src, size_value);
                } else {
                    self.bcx.call_memmove(self.cx.module.target_config(), dst, src, size_value);
                }

                return;
            }

            flags.set_aligned();
            registers.reserve(load_and_store_amount as usize);

            for i in 0..load_and_store_amount {
                let offset = (access_size * i) as i32 + offset;

                registers.push((src.offset(self, offset).load(self, int_type, flags), offset));
            }

            size = rest;
            align = rest;
            offset += (access_size * load_and_store_amount) as i32;
        }

        for (value, offset) in registers {
            dst.offset(self, offset).store(self, value, flags);
        }
    }
}
