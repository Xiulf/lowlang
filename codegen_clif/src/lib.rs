use cranelift::codegen::ir::InstBuilder;
use cranelift::codegen::{binemit, isa, settings};
use cranelift::codegen::{ir as clif, Context};
use cranelift::frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{default_libcall_names, DataId, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use index_vec::IndexVec;
use std::collections::HashMap;
use target_lexicon::Triple;

#[no_mangle]
pub fn run(module: &ir::Module) {
    let flags_builder = settings::builder();
    let flags = settings::Flags::new(flags_builder);
    let isa = isa::lookup(Triple::host()).unwrap().finish(flags);
    let builder = ObjectBuilder::new(isa, "test", default_libcall_names()).unwrap();
    let mut mcx = ModuleCtx::new(ObjectModule::new(builder));

    mcx.trans_module(module);
}

pub struct ModuleCtx<M: Module> {
    module: M,
    context: Context,
    funcs: HashMap<ir::DefId, FuncId>,
    globals: HashMap<ir::DefId, DataId>,
}

pub struct FunctionCtx<'mcx, M: Module> {
    module: &'mcx mut M,
    funcs: &'mcx mut HashMap<ir::DefId, FuncId>,
    globals: &'mcx mut HashMap<ir::DefId, DataId>,
    bcx: FunctionBuilder<'mcx>,
    blocks: IndexVec<ir::Block, clif::Block>,
    vars: HashMap<ir::Var, clif::Value>,
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

                if let ir::Type::Func(ir::Signature { params, rets }) = &ty.kind {
                    let mut sig = self.module.make_signature();

                    for ret in rets {}

                    for param in params {}

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

                    self.funcs.insert(def.id, func);
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
            let func = self.funcs[def];
            let mut fcx = FunctionBuilderContext::new();
            let bcx = FunctionBuilder::new(&mut self.context.func, &mut fcx);
            let mut fx = FunctionCtx::new(&mut self.module, &mut self.funcs, &mut self.globals, bcx);

            fx.trans_body(body);
            fx.module.define_function(func, &mut self.context, &mut binemit::NullTrapSink {}).unwrap();
        }
    }

    pub fn clif_type(&self, ty: &ir::Type) -> Option<clif::Type> {
        None
    }
}

impl<'mcx, M: Module> FunctionCtx<'mcx, M> {
    pub fn new(
        module: &'mcx mut M,
        funcs: &'mcx mut HashMap<ir::DefId, FuncId>,
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
        self.blocks = body.blocks.iter().map(|bb| self.bcx.create_block()).collect();

        for block in &body.blocks {
            let clif_block = self.blocks[block.id];

            for param in &block.params {
                let val = self.bcx.append_block_param(clif_block, _);

                self.vars.insert(*param, val);
            }

            for instr in &block.instrs {
                self.trans_instr(instr, body);
            }

            self.trans_term(&block.term);
        }

        self.bcx.seal_all_blocks();
        self.bcx.finalize();

        println!("{}", self.bcx.func);
    }

    pub fn trans_term(&mut self, term: &ir::Term) {
        match term {
            | ir::Term::Unset => unreachable!(),
            | ir::Term::Abort => {
                self.bcx.ins().trap(clif::TrapCode::User(0));
            },
            | ir::Term::Br(to, _) => {
                let to = self.blocks[*to];

                self.bcx.ins().jump(to, &[]);
            },
            | ir::Term::BrIf(op, then, else_, _) => {
                let op = self.trans_op(op);
                let then = self.blocks[*then];
                let else_ = self.blocks[*else_];

                self.bcx.ins().brz(op, else_, &[]);
                self.bcx.ins().jump(then, &[]);
            },
            | ir::Term::Return(vals) => {
                // let vals = vals.iter().map(|v| self.tans_op(v)).collect::<Vec<_>>();

                self.bcx.ins().return_(&[]);
            },
        }
    }

    pub fn trans_instr(&mut self, instr: &ir::Instr, body: &ir::Body) {
        match &instr.kind {
            | _ => unimplemented!("{}", instr),
        }
    }

    pub fn trans_op(&mut self, op: &ir::Operand) -> clif::Value {
        unimplemented!();
    }
}
