use index_vec::IndexVec;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::{types, values, AddressSpace};
use std::collections::HashMap;

pub fn run(module: &ir::Module) {
    let context = Context::create();
    let mut mcx = ModuleCtx::new(&context, "test");

    mcx.trans_module(module);
}

pub struct ModuleCtx<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
}

pub struct FunctionCtx<'ctx, 'mcx> {
    mcx: &'mcx mut ModuleCtx<'ctx>,
    bcx: Builder<'ctx>,
    func: values::FunctionValue<'ctx>,
    blocks: IndexVec<ir::Block, BasicBlock<'ctx>>,
}

impl<'ctx> std::ops::Deref for FunctionCtx<'ctx, '_> {
    type Target = ModuleCtx<'ctx>;

    fn deref(&self) -> &Self::Target {
        self.mcx
    }
}

impl<'ctx> std::ops::DerefMut for FunctionCtx<'ctx, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.mcx
    }
}

impl<'ctx> ModuleCtx<'ctx> {
    pub fn new(context: &'ctx Context, name: impl AsRef<str>) -> Self {
        ModuleCtx {
            module: context.create_module(name.as_ref()),
            context,
        }
    }

    pub fn trans_module(&mut self, module: &ir::Module) {
        let mut funcs = HashMap::new();
        let mut globals = HashMap::new();

        for def in &module.defs {
            if let ir::DeclKind::Def(ty) = &def.kind {
                let mut ty = ty;

                while let ir::Type::Forall(_, ret) = ty {
                    ty = &**ret;
                }

                if let ir::Type::Func(_) = ty {
                    let func = self.module.add_function(
                        &def.name,
                        self.ll_func_type(ty),
                        Some(match &def.linkage {
                            | ir::Linkage::Export => Linkage::External,
                            | ir::Linkage::Import => Linkage::AvailableExternally,
                            | ir::Linkage::Local => Linkage::Internal,
                        }),
                    );

                    funcs.insert(def.id, func);
                } else {
                    let global = self.module.add_global(self.ll_basic_type(ty), Some(AddressSpace::Global), &def.name);

                    globals.insert(def.id, global);
                }
            }
        }

        for (def, body) in &module.bodies {
            let func = funcs[def];
            let mut fx = FunctionCtx::new(self, func);

            fx.trans_body(body);
        }
    }

    pub fn ll_func_type(&self, ty: &ir::Type) -> types::FunctionType<'ctx> {
        match ty {
            | ir::Type::Func(ir::Signature { params, rets }) => {
                use types::BasicType as _;
                let params = params.iter().map(|t| self.ll_basic_type(t)).collect::<Vec<_>>();

                match rets.len() {
                    | 0 => self.context.void_type().fn_type(&params, false),
                    | 1 => self.ll_basic_type(&rets[0]).fn_type(&params, false),
                    | _ => {
                        let tys = rets.iter().map(|t| self.ll_basic_type(t)).collect::<Vec<_>>();

                        self.context.struct_type(&tys, false).fn_type(&params, false)
                    },
                }
            },
            | _ => unreachable!(),
        }
    }

    pub fn ll_basic_type(&self, ty: &ir::Type) -> types::BasicTypeEnum<'ctx> {
        use types::BasicType as _;

        match ty {
            | ir::Type::Int(1, _) => self.context.bool_type().into(),
            | ir::Type::Int(8, _) => self.context.i8_type().into(),
            | ir::Type::Int(16, _) => self.context.i16_type().into(),
            | ir::Type::Int(32, _) => self.context.i32_type().into(),
            | ir::Type::Int(64, _) => self.context.i64_type().into(),
            | ir::Type::Int(128, _) => self.context.i128_type().into(),
            | ir::Type::Int(_, _) => unreachable!(),
            | ir::Type::Float(16) => self.context.f16_type().into(),
            | ir::Type::Float(32) => self.context.f32_type().into(),
            | ir::Type::Float(64) => self.context.f64_type().into(),
            | ir::Type::Float(128) => self.context.f128_type().into(),
            | ir::Type::Float(_) => unreachable!(),
            | ir::Type::Ptr(to) => self.ll_basic_type(to).ptr_type(AddressSpace::Generic).into(),
            | ir::Type::Box(_) => unimplemented!(),
            | ir::Type::Tuple(tys) => {
                let tys = tys.iter().map(|t| self.ll_basic_type(t)).collect::<Vec<_>>();

                self.context.struct_type(&tys, false).into()
            },
            | ir::Type::Forall(_, ty) => self.ll_basic_type(ty),
            | ir::Type::Var(var) => self.context.opaque_struct_type(&var.to_string()).into(),
            | ir::Type::Def(_) => unimplemented!(),
            | ir::Type::Func(_) => unreachable!(),
        }
    }
}

impl<'ctx, 'mcx> FunctionCtx<'ctx, 'mcx> {
    pub fn new(mcx: &'mcx mut ModuleCtx<'ctx>, func: values::FunctionValue<'ctx>) -> Self {
        FunctionCtx {
            bcx: mcx.context.create_builder(),
            blocks: IndexVec::new(),
            func,
            mcx,
        }
    }

    pub fn trans_body(&mut self, body: &ir::Body) {
        self.blocks = body
            .blocks
            .iter()
            .map(|bb| self.context.append_basic_block(self.func, &bb.id.to_string()))
            .collect();

        for block in &body.blocks {
            for instr in &block.instrs {
                self.trans_instr(instr);
            }

            self.trans_term(&block.term);
        }

        self.func.print_to_stderr();
    }

    pub fn trans_term(&mut self, term: &ir::Term) {
        match term {
            | ir::Term::Unset => unreachable!(),
            | ir::Term::Abort => {
                self.bcx.build_unreachable();
            },
            | ir::Term::Br(to, _) => {},
            | ir::Term::BrIf(op, then, else_, _) => {},
            | ir::Term::Return(vals) => {},
        }
    }

    pub fn trans_instr(&mut self, instr: &ir::Instr) {
        match instr.name.as_str() {}
    }
}
