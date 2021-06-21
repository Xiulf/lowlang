#![feature(once_cell)]

pub mod ty;

use arena::{ArenaMap, Idx};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::object_file::ObjectFile;
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{InitializationConfig, Target, TargetData, TargetMachine, TargetTriple};
use inkwell::values;
use inkwell::IntPredicate;
use std::io::Write as _;
use std::lazy::OnceCell;
use tempfile::NamedTempFile;
use ty::*;

pub fn compile_module(ir: &ir::Module) -> NamedTempFile {
    with_codegen_ctx(ir, |mut ctx| {
        for (id, func) in ctx.ir.funcs.iter() {
            ctx.declare_func(id, func);
        }

        for (id, func) in ctx.ir.funcs.iter() {
            if let Some(body) = func.body {
                let val = ctx.funcs[id];

                ctx.lower_body(val, body);
            }
        }

        let pass_builder = PassManagerBuilder::create();
        let pass_manager = PassManager::create(());

        pass_builder.set_optimization_level(inkwell::OptimizationLevel::Default);
        pass_builder.populate_module_pass_manager(&pass_manager);
        pass_manager.run_on(&ctx.module);

        ctx.module.print_to_stderr();

        let buffer = ctx
            .target_machine
            .write_to_memory_buffer(&ctx.module, inkwell::targets::FileType::Object)
            .unwrap();

        let mut file = NamedTempFile::new().unwrap();

        file.write_all(buffer.as_slice()).unwrap();
        file
    })
}

pub struct CodegenCtx<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    target_machine: TargetMachine,
    target_data: TargetData,
    ir: &'ctx ir::Module,
    funcs: ArenaMap<Idx<ir::Func>, values::FunctionValue<'ctx>>,

    gen_alloc: OnceCell<values::FunctionValue<'ctx>>,
    gen_free: OnceCell<values::FunctionValue<'ctx>>,
    gen_generation: OnceCell<values::FunctionValue<'ctx>>,
}

pub struct BodyCtx<'a, 'ctx> {
    cx: &'a CodegenCtx<'ctx>,
    func: values::FunctionValue<'ctx>,
    body: &'ctx ir::Body,
    blocks: ArenaMap<Idx<ir::BlockData>, BasicBlock<'ctx>>,
    params: ArenaMap<Idx<ir::VarInfo>, values::PointerValue<'ctx>>,
    vars: ArenaMap<Idx<ir::VarInfo>, values::BasicValueEnum<'ctx>>,
    any_vars: ArenaMap<Idx<ir::VarInfo>, values::AnyValueEnum<'ctx>>,
}

pub fn with_codegen_ctx<T>(ir: &ir::Module, f: impl FnOnce(CodegenCtx) -> T) -> T {
    Target::initialize_native(&InitializationConfig::default()).unwrap();

    let host_triple = TargetMachine::get_default_triple();
    let host_cpu = TargetMachine::get_host_cpu_name();
    let host_features = TargetMachine::get_host_cpu_features();
    let target = Target::from_triple(&host_triple).unwrap();
    let target_machine = target
        .create_target_machine(
            &host_triple,
            host_cpu.to_str().unwrap(),
            host_features.to_str().unwrap(),
            inkwell::OptimizationLevel::Aggressive,
            inkwell::targets::RelocMode::PIC,
            inkwell::targets::CodeModel::Small,
        )
        .unwrap();

    let target_data = target_machine.get_target_data();
    let context = Context::create();
    let module = context.create_module(&ir.name);
    let builder = context.create_builder();
    let ctx = CodegenCtx {
        context: &context,
        module,
        builder,
        target_machine,
        target_data,
        ir,
        funcs: ArenaMap::default(),

        gen_alloc: OnceCell::new(),
        gen_free: OnceCell::new(),
        gen_generation: OnceCell::new(),
    };

    f(ctx)
}

impl<'ctx> CodegenCtx<'ctx> {
    pub fn declare_func(&mut self, id: Idx<ir::Func>, func: &ir::Func) {
        let ty = func.sig.as_func_type(self);
        let val = self.module.add_function(&func.name, ty, None);

        self.funcs.insert(id, val);
    }

    fn lower_body(&mut self, func: values::FunctionValue<'ctx>, body: ir::BodyId) {
        BodyCtx {
            func,
            body: &self.ir[body],
            cx: self,
            blocks: ArenaMap::default(),
            params: ArenaMap::default(),
            vars: ArenaMap::default(),
            any_vars: ArenaMap::default(),
        }
        .lower()
        .unwrap();
    }
}

impl<'a, 'ctx> BodyCtx<'a, 'ctx> {
    fn lower(&mut self) -> Option<()> {
        let entry = self.cx.context.append_basic_block(self.func, "entry");

        self.cx.builder.position_at_end(entry);

        for (id, block) in self.body.blocks.iter() {
            let name = ir::Block(id).to_string();
            let bb = self.cx.context.append_basic_block(self.func, &name);

            self.blocks.insert(id, bb);

            for &param in &block.params {
                let ty = self.body[param].ty.as_basic_type(self.cx);
                let ptr = self.cx.builder.build_alloca(ty, "");

                self.params.insert(param.0, ptr.into());
            }
        }

        let entry = &self.body[ir::Block::ENTRY];

        for (i, param) in entry.params.iter().enumerate() {
            let ptr = self.params[param.0];
            let val = self.func.get_nth_param(i as u32)?;

            self.cx.builder.build_store(ptr, val);
        }

        self.cx.builder.build_unconditional_branch(self.blocks[ir::Block::ENTRY.0]);

        for (id, block) in self.body.blocks.iter() {
            let bb = self.blocks[id];

            self.lower_block(bb, ir::Block(id), block);
        }

        Some(())
    }

    fn lower_block(&mut self, bb: BasicBlock<'ctx>, id: ir::Block, block: &ir::BlockData) -> Option<()> {
        self.cx.builder.position_at_end(bb);

        for param in &block.params {
            let ptr = self.params[param.0];
            let val = self.cx.builder.build_load(ptr, "");

            self.vars.insert(param.0, val);
        }

        for instr in &block.instrs {
            self.lower_instr(instr)?;
        }

        self.lower_term(block.term.as_ref()?);

        Some(())
    }

    fn lower_term(&mut self, term: &ir::Term) {
        match term {
            | ir::Term::Unreachable => {
                self.cx.builder.build_unreachable();
            },
            | ir::Term::Return { vals: ops } => {
                let vals = ops.iter().map(|v| self.vars[v.0]).collect::<Vec<_>>();

                match vals.len() {
                    | 0 => self.cx.builder.build_return(None),
                    | 1 => self.cx.builder.build_return(Some(&vals[0])),
                    | _ => self.cx.builder.build_aggregate_return(&vals),
                };
            },
            | ir::Term::Br { to } => {
                for (param, arg) in self.body[to.block].params.iter().zip(&to.args) {
                    let ptr = self.params[param.0];
                    let val = self.vars[arg.0];

                    self.cx.builder.build_store(ptr, val);
                }

                self.cx.builder.build_unconditional_branch(self.blocks[to.block.0]);
            },
            | ir::Term::Switch { pred, cases, default } => {
                let pred = self.vars[pred.0].into_int_value();
                let pred_ty = pred.get_type();
                let bb = self.cx.builder.get_insert_block().unwrap();
                let cases = cases
                    .iter()
                    .map(|case| {
                        let val = pred_ty.const_int(case.val as u64, true);

                        if case.to.args.is_empty() {
                            (val, self.blocks[case.to.block.0])
                        } else {
                            unimplemented!();
                        }
                    })
                    .collect::<Vec<_>>();

                let def = if default.args.is_empty() {
                    self.blocks[default.block.0]
                } else {
                    unimplemented!();
                };

                self.cx.builder.build_switch(pred, def, &cases);
            },
        }
    }

    fn gen_alloc(&self) -> values::FunctionValue<'ctx> {
        *self.cx.gen_alloc.get_or_init(|| {
            let size_t = self.cx.context.ptr_sized_int_type(&self.cx.target_data, None);
            let unit = self.cx.context.opaque_struct_type("UNIT").ptr_type(inkwell::AddressSpace::Generic);
            let ret = self.cx.context.struct_type(&[unit.into(), size_t.into()], false);
            let sig = ret.fn_type(&[size_t.into()], false);

            self.cx.module.add_function("gen_alloc", sig, None)
        })
    }

    fn gen_free(&self) -> values::FunctionValue<'ctx> {
        *self.cx.gen_free.get_or_init(|| {
            let size_t = self.cx.context.ptr_sized_int_type(&self.cx.target_data, None);
            let unit = self.cx.context.opaque_struct_type("UNIT").ptr_type(inkwell::AddressSpace::Generic);
            let void = self.cx.context.void_type();
            let sig = void.fn_type(&[unit.into(), size_t.into()], false);

            self.cx.module.add_function("gen_free", sig, None)
        })
    }

    fn lower_instr(&mut self, instr: &ir::Instr) -> Option<()> {
        match *instr {
            | ir::Instr::StackAlloc { ret, ref ty } => {
                let ty = ty.as_basic_type(self.cx);
                let val = self.cx.builder.build_alloca(ty, "");

                self.vars.insert(ret.0, val.into());
            },
            | ir::Instr::StackFree { .. } => {},
            | ir::Instr::BoxAlloc { ret, ref ty } => {
                use inkwell::types::BasicType;
                let ty = ty.as_basic_type(self.cx);
                let size = ty.size_of().unwrap();
                let gen_alloc = self.gen_alloc();
                let call = self.cx.builder.build_call(gen_alloc, &[size.into()], "");
                let val = call.try_as_basic_value().unwrap_left();

                self.vars.insert(ret.0, val);
            },
            | ir::Instr::BoxFree { boxed } => {
                if let ir::ty::typ::Box(of) = self.body[boxed].ty.lookup().kind {
                    use inkwell::types::BasicType;
                    let ty = of.as_basic_type(self.cx);
                    let size = ty.size_of().unwrap();
                    let gen_free = self.gen_free();
                    let val = self.vars[boxed.0].into_struct_value();
                    let ptr = self.cx.builder.build_extract_value(val, 0, "").unwrap();

                    self.cx.builder.build_call(gen_free, &[ptr, size.into()], "");
                } else {
                    unreachable!();
                }
            },
            | ir::Instr::ConstInt { ret, val } => {
                let ty = self.body[ret].ty.as_basic_type(self.cx);
                let val = ty.into_int_type().const_int(val as u64, true);

                self.vars.insert(ret.0, val.into());
            },
            | ir::Instr::FuncRef { ret, func } => {
                let val = self.cx.funcs[func.0];

                self.any_vars.insert(ret.0, val.into());
            },
            | ir::Instr::Tuple { ret, ref vals } => {
                let tys = vals.iter().map(|v| self.body[*v].ty.as_basic_type(self.cx)).collect::<Vec<_>>();
                let tuple = self.cx.context.struct_type(&tys, false);
                let tuple = self.cx.builder.build_alloca(tuple, "");
                let mut tuple = self.cx.builder.build_load(tuple, "").into_struct_value();

                for (i, val) in vals.iter().enumerate() {
                    let val = self.vars[val.0];
                    let new = self.cx.builder.build_insert_value(tuple, val, i as u32, "");

                    tuple = new.unwrap().into_struct_value();
                }

                self.vars.insert(ret.0, tuple.into());
            },
            | ir::Instr::TupleExtract { ret, tuple, field } => {
                let tuple = self.vars[tuple.0].into_struct_value();
                let val = self.cx.builder.build_extract_value(tuple, field as u32, "").unwrap();

                self.vars.insert(ret.0, val);
            },
            | ir::Instr::TupleInsert { tuple, field, val } => {
                let tuple_ = self.vars[tuple.0].into_struct_value();
                let val = self.vars[val.0];
                let new = self.cx.builder.build_insert_value(tuple_, val, field as u32, "");

                self.vars.insert(tuple.0, new.unwrap().into_struct_value().into());
            },
            | ir::Instr::Apply { ref rets, func, ref args, .. } => {
                let args = args.iter().map(|a| self.vars[a.0]).collect::<Vec<_>>();
                let res = match self.any_vars.get(func.0) {
                    | Some(val) => {
                        let func = val.into_function_value();

                        self.cx.builder.build_call(func, &args, "")
                    },
                    | None => {
                        use std::convert::TryFrom;
                        use values::CallableValue;
                        let func = self.vars[func.0].into_pointer_value();
                        let func = CallableValue::try_from(func).unwrap();

                        self.cx.builder.build_call(func, &args, "")
                    },
                };

                let res = res.try_as_basic_value();

                match rets.len() {
                    | 0 => assert!(res.is_right()),
                    | 1 => self.vars.insert(rets[0].0, res.left()?),
                    | _ => {
                        let struc = res.left()?.into_struct_value();

                        for (i, ret) in rets.iter().enumerate() {
                            let val = self.cx.builder.build_extract_value(struc, i as u32, "")?;

                            self.vars.insert(ret.0, val);
                        }
                    },
                }
            },
            | ir::Instr::Intrinsic { ref rets, ref name, ref args } => match name.as_str() {
                | "add_i32" => {
                    let lhs = self.vars[args[0].0].into_int_value();
                    let rhs = self.vars[args[1].0].into_int_value();
                    let res = self.cx.builder.build_int_add(lhs, rhs, "");

                    self.vars.insert(rets[0].0, res.into());
                },
                | "sub_i32" => {
                    let lhs = self.vars[args[0].0].into_int_value();
                    let rhs = self.vars[args[1].0].into_int_value();
                    let res = self.cx.builder.build_int_sub(lhs, rhs, "");

                    self.vars.insert(rets[0].0, res.into());
                },
                | "mul_i32" => {
                    let lhs = self.vars[args[0].0].into_int_value();
                    let rhs = self.vars[args[1].0].into_int_value();
                    let res = self.cx.builder.build_int_mul(lhs, rhs, "");

                    self.vars.insert(rets[0].0, res.into());
                },
                | "div_i32" => {
                    let lhs = self.vars[args[0].0].into_int_value();
                    let rhs = self.vars[args[1].0].into_int_value();
                    let res = self.cx.builder.build_int_signed_div(lhs, rhs, "");

                    self.vars.insert(rets[0].0, res.into());
                },
                | "rem_i32" => {
                    let lhs = self.vars[args[0].0].into_int_value();
                    let rhs = self.vars[args[1].0].into_int_value();
                    let res = self.cx.builder.build_int_signed_rem(lhs, rhs, "");

                    self.vars.insert(rets[0].0, res.into());
                },
                | "eq_i32" => {
                    let lhs = self.vars[args[0].0].into_int_value();
                    let rhs = self.vars[args[1].0].into_int_value();
                    let res = self.cx.builder.build_int_compare(IntPredicate::EQ, lhs, rhs, "");

                    self.vars.insert(rets[0].0, res.into());
                },
                | "ne_i32" => {
                    let lhs = self.vars[args[0].0].into_int_value();
                    let rhs = self.vars[args[1].0].into_int_value();
                    let res = self.cx.builder.build_int_compare(IntPredicate::EQ, lhs, rhs, "");

                    self.vars.insert(rets[0].0, res.into());
                },
                | "lt_i32" => {
                    let lhs = self.vars[args[0].0].into_int_value();
                    let rhs = self.vars[args[1].0].into_int_value();
                    let res = self.cx.builder.build_int_compare(IntPredicate::SLT, lhs, rhs, "");

                    self.vars.insert(rets[0].0, res.into());
                },
                | "le_i32" => {
                    let lhs = self.vars[args[0].0].into_int_value();
                    let rhs = self.vars[args[1].0].into_int_value();
                    let res = self.cx.builder.build_int_compare(IntPredicate::SLE, lhs, rhs, "");

                    self.vars.insert(rets[0].0, res.into());
                },
                | "gt_i32" => {
                    let lhs = self.vars[args[0].0].into_int_value();
                    let rhs = self.vars[args[1].0].into_int_value();
                    let res = self.cx.builder.build_int_compare(IntPredicate::SGT, lhs, rhs, "");

                    self.vars.insert(rets[0].0, res.into());
                },
                | "ge_i32" => {
                    let lhs = self.vars[args[0].0].into_int_value();
                    let rhs = self.vars[args[1].0].into_int_value();
                    let res = self.cx.builder.build_int_compare(IntPredicate::SGE, lhs, rhs, "");

                    self.vars.insert(rets[0].0, res.into());
                },
                | _ => unreachable!(),
            },
            | _ => unimplemented!("{}", instr.display(self.body)),
        }

        Some(())
    }
}
