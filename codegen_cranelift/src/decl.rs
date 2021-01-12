use crate::*;
use clif::Module;

impl<'ctx> DeclMethods<'ctx> for ClifBackend<'ctx> {
    type Backend = Self;

    fn declare_static(
        mcx: &mut ModuleCtx<'_, 'ctx, ClifBackend<'ctx>>,
        decl: &ir::Decl,
    ) -> clif::DataId {
        let linkage = match decl.linkage {
            ir::Linkage::Import => clif::Linkage::Import,
            ir::Linkage::Export => clif::Linkage::Export,
            ir::Linkage::Hidden => clif::Linkage::Hidden,
            ir::Linkage::Local => clif::Linkage::Local,
        };

        let data = mcx
            .module
            .declare_data(&decl.name, linkage, true, false)
            .unwrap();

        mcx.data_ids.insert(decl.id, data);

        data
    }

    fn declare_func(
        mcx: &mut ModuleCtx<'_, 'ctx, ClifBackend<'ctx>>,
        decl: &ir::Decl,
    ) -> clif::FuncId {
        let linkage = match decl.linkage {
            ir::Linkage::Import => clif::Linkage::Import,
            ir::Linkage::Export => clif::Linkage::Export,
            ir::Linkage::Hidden => clif::Linkage::Hidden,
            ir::Linkage::Local => clif::Linkage::Local,
        };

        let sig = mk_signature(mcx, &decl.ty.signature());
        let func = mcx
            .module
            .declare_function(&decl.name, linkage, &sig)
            .unwrap();

        mcx.func_ids.insert(decl.id, (func, sig));

        func
    }

    fn define_func(fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>, func: clif::FuncId) {
        fx.bcx.seal_all_blocks();
        fx.bcx.finalize();
        fx.ctx.compute_cfg();
        fx.ctx.compute_domtree();

        println!("{}", fx.module.declarations().get_function_decl(func).name);
        println!("{}", fx.bcx.func);

        fx.mcx
            .ctx
            .eliminate_unreachable_code(fx.mcx.module.isa())
            .unwrap();

        fx.mcx.ctx.dce(fx.mcx.module.isa()).unwrap();

        fx.mcx
            .module
            .define_function(
                func,
                &mut fx.mcx.ctx,
                &mut clif::codegen::binemit::NullTrapSink {},
            )
            .unwrap();

        fx.ctx.clear();
    }

    fn func_prologue(fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>) {
        use clif::InstBuilder;
        let sig = fx.func_ids[&fx.body.decl].1.clone();
        let start_block = fx.bcx.create_block();

        fx.bcx.func.signature = sig;
        fx.bcx.switch_to_block(start_block);

        for block in &fx.body.blocks {
            fx.blocks.insert(block.id, fx.bcx.create_block());
        }

        let ssa_map = analyze::analyze(fx);
        let ptr_ty = fx.module.target_config().pointer_type();

        for ret in fx.body.rets() {
            let layout = ir::layout::layout_of(&ret.ty, &fx.target);

            match abi::get_pass_mode(fx.mcx, &layout) {
                abi::PassMode::NoPass => {
                    fx.locals.insert(ret.id, place::Place::no_place(layout));
                }
                abi::PassMode::ByVal(_) | abi::PassMode::ByValPair(_, _) => {
                    let ssa = ssa_map[&ret.id] == analyze::SsaKind::Ssa;

                    local_place(fx, ret.id, layout, ssa);
                }
                abi::PassMode::ByRef { size: _ } => {
                    let val = fx.bcx.append_block_param(start_block, ptr_ty);

                    fx.locals.insert(
                        ret.id,
                        place::Place::new_ref(ptr::Pointer::addr(val), layout),
                    );
                }
            }
        }

        let vals = fx
            .body
            .args()
            .filter_map(|arg| {
                let layout = ir::layout::layout_of(&arg.ty, &fx.target);
                let value = match abi::get_pass_mode(fx.mcx, &layout) {
                    abi::PassMode::NoPass => return None,
                    abi::PassMode::ByVal(ty) => {
                        let param = fx.bcx.append_block_param(start_block, ty);

                        value::Value::new_val(param, layout.clone())
                    }
                    abi::PassMode::ByValPair(a, b) => {
                        let a = fx.bcx.append_block_param(start_block, a);
                        let b = fx.bcx.append_block_param(start_block, b);

                        value::Value::new_val_pair(a, b, layout.clone())
                    }
                    abi::PassMode::ByRef { size: Some(_) } => {
                        let param = fx.bcx.append_block_param(start_block, ptr_ty);

                        value::Value::new_ref(ptr::Pointer::addr(param), layout.clone())
                    }
                    abi::PassMode::ByRef { size: None } => {
                        let ptr = fx.bcx.append_block_param(start_block, ptr_ty);
                        // let meta = fx.bcx.append_block_param(start_block, fx.ptr_type);

                        // value::Value::new_ref_meta(ptr::Pointer::addr(ptr), meta, layout.clone())
                        value::Value::new_ref(ptr::Pointer::addr(ptr), layout.clone())
                    }
                };

                Some(value)
            })
            .collect::<Vec<_>>();

        for (arg, value) in fx.body.args().zip(vals) {
            let ssa = ssa_map[&arg.id] == analyze::SsaKind::Ssa;
            let place = if ssa {
                let place = if let ir::layout::Abi::ScalarPair(_, _) = value.layout.abi {
                    place::Place::new_var_pair(fx, value.layout.clone())
                } else {
                    place::Place::new_var(fx, value.layout.clone())
                };

                place.clone().store(fx, value);
                place
            } else {
                place::Place::new_ref(value.clone().on_stack(fx).0, value.layout)
            };

            fx.locals.insert(arg.id, place);
        }

        for local in &fx.body.locals {
            if let ir::LocalKind::Var | ir::LocalKind::Tmp = local.kind {
                let ssa = ssa_map[&local.id] == analyze::SsaKind::Ssa;
                let layout = ir::layout::layout_of(&local.ty, &fx.target);

                local_place(fx, local.id, layout, ssa);
            }
        }

        fx.bcx
            .ins()
            .jump(fx.blocks[&fx.body.blocks.first().unwrap().id], &[]);

        fn local_place<'ctx>(
            fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>,
            local: ir::Local,
            layout: ir::layout::TyLayout,
            ssa: bool,
        ) -> place::Place<'ctx> {
            let place = if ssa {
                if let ir::layout::Abi::ScalarPair(_, _) = layout.abi {
                    place::Place::new_var_pair(fx, layout)
                } else {
                    place::Place::new_var(fx, layout)
                }
            } else {
                place::Place::new_stack(fx, layout)
            };

            fx.locals.insert(local, place.clone());
            place
        }
    }
}
