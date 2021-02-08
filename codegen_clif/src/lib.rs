mod pass;
mod ptr;
mod value;

use cranelift::codegen::ir::InstBuilder;
use cranelift::codegen::settings::Configurable as _;
use cranelift::codegen::{binemit, isa, settings};
use cranelift::codegen::{ir as clif, Context};
use cranelift::frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{default_libcall_names, DataId, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use index_vec::IndexVec;
use ptr::*;
use std::collections::HashMap;
use std::io::Write;
use target_lexicon::Triple;
use tempfile::NamedTempFile;
use value::*;

pub struct Assembly {
    #[used]
    file: NamedTempFile,
}

#[no_mangle]
pub fn run_codegen(module: &ir::Module) -> Assembly {
    let mut flags_builder = settings::builder();

    flags_builder.set("opt_level", "speed_and_size").unwrap();

    let flags = settings::Flags::new(flags_builder);
    let isa = isa::lookup(Triple::host()).unwrap().finish(flags);
    let builder = ObjectBuilder::new(isa, "test", default_libcall_names()).unwrap();
    let mut mcx = ModuleCtx::new(ObjectModule::new(builder));

    mcx.trans_module(module);

    let bytes = mcx.module.finish().emit().unwrap();
    let mut file = NamedTempFile::new_in(".").unwrap();

    file.write_all(&bytes).unwrap();

    Assembly { file }
}

pub struct ModuleCtx<M: Module> {
    module: M,
    context: Context,
    funcs: HashMap<ir::DefId, (FuncId, clif::Signature)>,
    globals: HashMap<ir::DefId, DataId>,
}

pub struct FunctionCtx<'mcx, M: Module> {
    module: &'mcx mut M,
    funcs: &'mcx mut HashMap<ir::DefId, (FuncId, clif::Signature)>,
    globals: &'mcx mut HashMap<ir::DefId, DataId>,
    bcx: FunctionBuilder<'mcx>,
    blocks: IndexVec<ir::Block, clif::Block>,
    vars: HashMap<ir::Var, Value>,
}

impl<M: Module> ModuleCtx<M> {
    pub fn new(module: M) -> Self {
        ModuleCtx {
            context: module.make_context(),
            funcs: HashMap::new(),
            globals: HashMap::new(),
            module,
        }
    }

    pub fn trans_module(&mut self, module: &ir::Module) {
        for def in &module.defs {
            if let ir::DeclKind::Def(ty) = &def.kind {
                let mut ty = ty;

                while let ir::Type::Forall(_, ret) = &ty.kind {
                    ty = &**ret;
                }

                if let ir::Type::Func(sig) = &ty.kind {
                    let sig = clif_sig(&self.module, sig);
                    let func = self
                        .module
                        .declare_function(
                            &def.name,
                            match &def.linkage {
                                | ir::Linkage::Export => Linkage::Export,
                                | ir::Linkage::Import => Linkage::Import,
                                | ir::Linkage::Local => Linkage::Local,
                            },
                            &sig,
                        )
                        .unwrap();

                    self.funcs.insert(def.id, (func, sig));
                } else {
                    let global = self
                        .module
                        .declare_data(
                            &def.name,
                            match &def.linkage {
                                | ir::Linkage::Export => Linkage::Export,
                                | ir::Linkage::Import => Linkage::Import,
                                | ir::Linkage::Local => Linkage::Local,
                            },
                            true,
                            false,
                        )
                        .unwrap();

                    self.globals.insert(def.id, global);
                }
            }
        }

        for (def, body) in &module.bodies {
            let func = self.funcs[def].0;
            let sig = self.funcs[def].1.clone();
            let mut fcx = FunctionBuilderContext::new();
            let bcx = FunctionBuilder::new(&mut self.context.func, &mut fcx);
            let mut fx = FunctionCtx::new(&mut self.module, &mut self.funcs, &mut self.globals, bcx);

            fx.bcx.func.signature = sig;
            fx.trans_body(body);

            self.context.compute_cfg();
            self.context.compute_domtree();
            self.context.eliminate_unreachable_code(self.module.isa()).unwrap();
            self.context.dce(self.module.isa()).unwrap();
            self.context.domtree.clear();

            println!("{}", self.context.func);

            self.module.define_function(func, &mut self.context, &mut binemit::NullTrapSink {}).unwrap();
            self.module.clear_context(&mut self.context);
        }
    }
}

fn clif_type(module: &impl Module, layout: &ir::layout::TyLayout) -> Option<clif::Type> {
    if let ir::layout::Abi::Scalar(s) = &layout.abi {
        Some(scalar_ty(module, s))
    } else {
        None
    }
}

fn scalar_ty(module: &impl Module, scalar: &ir::layout::Scalar) -> clif::Type {
    use ir::layout::{Integer, Primitive};
    match &scalar.value {
        | Primitive::Int(Integer::I8, _) => clif::types::I8,
        | Primitive::Int(Integer::I16, _) => clif::types::I16,
        | Primitive::Int(Integer::I32, _) => clif::types::I32,
        | Primitive::Int(Integer::I64, _) => clif::types::I64,
        | Primitive::Int(Integer::I128, _) => clif::types::I128,
        | Primitive::Bool => clif::types::B8,
        | Primitive::F32 => clif::types::F32,
        | Primitive::F64 => clif::types::F64,
        | Primitive::Pointer => module.target_config().pointer_type(),
    }
}

fn clif_sig(module: &impl Module, ir::Signature { params, rets }: &ir::Signature) -> clif::Signature {
    let mut sig = module.make_signature();
    let ptr_type = module.target_config().pointer_type();

    for ret in rets {
        let layout = ir::layout::layout_of(ret, module.isa().triple());

        match pass::pass_mode(module, &layout) {
            | pass::PassMode::NoPass => {},
            | pass::PassMode::ByVal(ty) => sig.returns.push(clif::AbiParam::new(ty)),
            | pass::PassMode::ByRef => sig.params.push(clif::AbiParam::new(ptr_type)),
        }
    }

    for param in params {
        let layout = ir::layout::layout_of(param, module.isa().triple());

        match pass::pass_mode(module, &layout) {
            | pass::PassMode::NoPass => {},
            | pass::PassMode::ByVal(ty) => sig.params.push(clif::AbiParam::new(ty)),
            | pass::PassMode::ByRef => sig.params.push(clif::AbiParam::new(ptr_type)),
        }
    }

    sig
}

impl<'mcx, M: Module> FunctionCtx<'mcx, M> {
    pub fn new(
        module: &'mcx mut M,
        funcs: &'mcx mut HashMap<ir::DefId, (FuncId, clif::Signature)>,
        globals: &'mcx mut HashMap<ir::DefId, DataId>,
        bcx: FunctionBuilder<'mcx>,
    ) -> Self {
        FunctionCtx {
            module,
            funcs,
            globals,
            bcx,
            blocks: IndexVec::new(),
            vars: HashMap::new(),
        }
    }

    pub fn trans_body(&mut self, body: &ir::Body) {
        self.blocks = (0..body.blocks.len()).map(|_| self.bcx.create_block()).collect();

        for block in &body.blocks {
            let clif_block = self.blocks[block.id];

            self.bcx.switch_to_block(clif_block);

            for &param in &block.params {
                let ty = &body.vars[param].ty;
                let layout = ir::layout::layout_of(ty, self.module.isa().triple());
                let val = match pass::pass_mode(&self.module, &layout) {
                    | pass::PassMode::NoPass => Value::new_unit(layout),
                    | pass::PassMode::ByVal(ty) => {
                        let val = self.bcx.append_block_param(clif_block, ty);

                        Value::new_val(val, layout)
                    },
                    | pass::PassMode::ByRef => {
                        let ptr_type = self.module.target_config().pointer_type();
                        let val = self.bcx.append_block_param(clif_block, ptr_type);
                        let ptr = Pointer::addr(val);

                        Value::new_ref(ptr, layout)
                    },
                };

                self.vars.insert(param, val);
            }

            for instr in &block.instrs {
                self.trans_instr(instr, body);
            }

            self.trans_term(&block.term, body);
        }

        self.bcx.seal_all_blocks();
        self.bcx.finalize();
    }

    pub fn trans_term(&mut self, term: &ir::Term, body: &ir::Body) {
        match term {
            | ir::Term::Unset => unreachable!(),
            | ir::Term::Abort => {
                self.bcx.ins().trap(clif::TrapCode::User(0));
            },
            | ir::Term::Br(to, args) => {
                let to = self.blocks[*to];
                let args = args
                    .iter()
                    .filter_map(|a| {
                        let val = self.vars[a].clone();

                        match pass::pass_mode(&self.module, &val.layout) {
                            | pass::PassMode::NoPass => None,
                            | pass::PassMode::ByRef => None,
                            | pass::PassMode::ByVal(_) => Some(val.load_scalar(self)),
                        }
                    })
                    .collect::<Vec<_>>();

                self.bcx.ins().jump(to, &args);
            },
            | ir::Term::BrIf(op, then, else_, args) => {
                let bool = ir::layout::TyLayout::BOOL;
                let op = self.trans_op(op, move || bool).load_scalar(self);
                let then = self.blocks[*then];
                let else_ = self.blocks[*else_];
                let args = args
                    .iter()
                    .filter_map(|a| {
                        let val = self.vars[a].clone();

                        match pass::pass_mode(&self.module, &val.layout) {
                            | pass::PassMode::NoPass => None,
                            | pass::PassMode::ByRef => None,
                            | pass::PassMode::ByVal(_) => Some(val.load_scalar(self)),
                        }
                    })
                    .collect::<Vec<_>>();

                self.bcx.ins().brz(op, else_, &args);
                self.bcx.ins().jump(then, &args);
            },
            | ir::Term::Return(vals) => {
                let triple = self.module.isa().triple().clone();
                let vals = vals
                    .iter()
                    .zip(&body.rets)
                    .filter_map(|(v, r)| {
                        let val = self.trans_op(v, || ir::layout::layout_of(r, &triple));

                        match pass::pass_mode(&self.module, &val.layout) {
                            | pass::PassMode::NoPass => None,
                            | pass::PassMode::ByRef => None,
                            | pass::PassMode::ByVal(_) => Some(val.load_scalar(self)),
                        }
                    })
                    .collect::<Vec<_>>();

                self.bcx.ins().return_(&vals);
            },
        }
    }

    pub fn trans_instr(&mut self, instr: &ir::Instr, body: &ir::Body) {
        match &instr.kind {
            | ir::instr::Const { res, const_ } => {
                let ty = &body.vars[*res].ty;
                let layout = ir::layout::layout_of(ty, self.module.isa().triple());
                let val = self.trans_const(const_, layout);

                self.vars.insert(*res, val);
            },
            | ir::instr::Load { res, ptr } => {
                let ptr = self.vars[ptr].clone();
                let val = ptr.deref(self);

                self.vars.insert(*res, val);
            },
            | ir::instr::Offset { res, ptr, by } => {
                let ptr = self.vars[ptr].clone();
                let lyt = ptr.layout.clone();
                let by = self.trans_op(by, || ptr.layout.clone()).load_scalar(self);
                let by = self.bcx.ins().imul_imm(by, lyt.pointee(self.module.isa().triple()).size.bytes() as i64);
                let ptr = ptr.load_scalar(self);
                let val = self.bcx.ins().iadd(ptr, by);
                let val = Value::new_val(val, lyt);

                self.vars.insert(*res, val);
            },
            | ir::instr::Sub { res, lhs, rhs } => {
                let lhs = self.vars[lhs].clone();
                let lyt = lhs.layout.clone();
                let rhs = self.trans_op(rhs, || lhs.layout.clone()).load_scalar(self);
                let lhs = lhs.load_scalar(self);
                let val = self.bcx.ins().isub(lhs, rhs);
                let val = Value::new_val(val, lyt);

                self.vars.insert(*res, val);
            },
            | ir::instr::Call { rets, func, args } => {
                let ret_modes = rets
                    .iter()
                    .map(|r| {
                        let ty = &body.vars[*r].ty;
                        let layout = ir::layout::layout_of(ty, self.module.isa().triple());

                        (pass::pass_mode(&self.module, &layout), layout)
                    })
                    .collect::<Vec<_>>();

                let ret_ptrs = ret_modes
                    .iter()
                    .filter_map(|(mode, lyt)| match mode {
                        | pass::PassMode::ByRef => {
                            let ss = clif::StackSlotData {
                                kind: clif::StackSlotKind::ExplicitSlot,
                                size: lyt.size.bytes() as u32,
                                offset: None,
                            };

                            let ss = self.bcx.create_stack_slot(ss);

                            Some(Pointer::stack(ss))
                        },
                        | _ => None,
                    })
                    .collect::<Vec<_>>();

                let args = ret_ptrs
                    .iter()
                    .map(|ptr| ptr.get_addr(self))
                    .collect::<Vec<_>>()
                    .into_iter()
                    .chain(args.iter().map(|arg| {
                        let val = self.trans_op(arg, || unimplemented!());

                        val.load_scalar(self)
                    }))
                    .collect::<Vec<_>>();

                let call = if let ir::Operand::Const(ir::Const::Addr(id)) = func {
                    let func = self.module.declare_func_in_func(self.funcs[id].0, &mut self.bcx.func);

                    self.bcx.ins().call(func, &args)
                } else {
                    unimplemented!();
                    // let func = self.trans_op(func).load_scalar(self);
                    //
                    // self.bcx.ins().call_indirect(sig, func, &args)
                };

                let mut res = self.bcx.inst_results(call).into_iter();
                let mut ret_ptrs = ret_ptrs.into_iter();

                for (&ret, (mode, lyt)) in rets.iter().zip(ret_modes) {
                    let val = match mode {
                        | pass::PassMode::NoPass => Value::new_unit(lyt),
                        | pass::PassMode::ByRef => Value::new_ref(ret_ptrs.next().unwrap(), lyt),
                        | pass::PassMode::ByVal(_) => Value::new_val(*res.next().unwrap(), lyt),
                    };

                    self.vars.insert(ret, val);
                }
            },
            | _ => unimplemented!("{}", instr),
        }
    }

    pub fn trans_op(&mut self, op: &ir::Operand, lyt: impl FnOnce() -> ir::layout::TyLayout) -> Value {
        match op {
            | ir::Operand::Var(var) => self.vars[var].clone(),
            | ir::Operand::Const(const_) => self.trans_const(const_, lyt()),
        }
    }

    pub fn trans_const(&mut self, const_: &ir::Const, layout: ir::layout::TyLayout) -> Value {
        match const_ {
            | ir::Const::Undefined => match pass::pass_mode(&self.module, &layout) {
                | pass::PassMode::NoPass => Value::new_unit(layout),
                | pass::PassMode::ByVal(_) => Value::new_const(0, self, layout),
                | pass::PassMode::ByRef => {
                    let ss = clif::StackSlotData {
                        kind: clif::StackSlotKind::ExplicitSlot,
                        size: layout.size.bytes() as u32,
                        offset: None,
                    };

                    let ss = self.bcx.create_stack_slot(ss);

                    Value::new_ref(Pointer::stack(ss), layout)
                },
            },
            | ir::Const::Scalar(s) => Value::new_const(*s, self, layout),
            | _ => unimplemented!("{}", const_),
        }
    }
}
