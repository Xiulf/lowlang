mod place;
mod stmt;
mod term;
mod operand;
mod value;
mod cast;

use crate::FunctionCtx;
use syntax::layout::Layout;
use cranelift_module::{Backend, Module, Linkage, FuncId, DataId};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_codegen::ir::{AbiParam, Signature, ExternalName, InstBuilder};
use std::collections::BTreeMap;

pub fn translate<B: Backend>(mut module: Module<B>, package: &syntax::Package) -> B::Product {
    let mut func_ids = BTreeMap::new();
    let mut data_ids = BTreeMap::new();

    for (ext_id, ext) in &package.externs {
        match ext {
            syntax::Extern::Proc(name, sig) => {
                let sign = crate::pass::call_sig(&module, sig);
                let func = module.declare_function(name, Linkage::Import, &sign).unwrap();
                let rets = sig.2.iter().map(|r| r.layout()).collect();

                func_ids.insert(*ext_id, (func, sign, rets));
            },
            _ => unimplemented!(),
        }
    }

    for (glob_id, glob) in &package.globals {
        let data_id = module.declare_data(
            &glob.name,
            if glob.export { Linkage::Export } else { Linkage::Local },
            true,
            Some(glob.ty.layout().details().align as u8)
        ).unwrap();

        let mut data_ctx = cranelift_module::DataContext::new();

        if let Some(init) = &glob.init {
            data_ctx.define(init.clone());
        } else {
            data_ctx.define_zeroinit(glob.ty.layout().details().size);
        }

        module.define_data(data_id, &data_ctx).unwrap();
        data_ids.insert(*glob_id, (data_id, glob.ty.layout()));
    }

    for (body_id, body) in &package.bodies {
        let mut sig = module.make_signature();

        for ret in body.rets() {
            match crate::pass::pass_mode(&module, ret.ty.layout()) {
                crate::pass::PassMode::NoPass => {},
                crate::pass::PassMode::ByVal(ty) => sig.returns.push(AbiParam::new(ty)),
                crate::pass::PassMode::ByRef => sig.params.push(AbiParam::new(module.target_config().pointer_type())),
            }
        }

        for arg in body.args() {
            match crate::pass::pass_mode(&module, arg.ty.layout()) {
                crate::pass::PassMode::NoPass => {},
                crate::pass::PassMode::ByVal(ty) => sig.params.push(AbiParam::new(ty)),
                crate::pass::PassMode::ByRef => sig.params.push(AbiParam::new(module.target_config().pointer_type())),
            }
        }

        let func = module.declare_function(
            &body.name,
            if body.export { Linkage::Export } else { Linkage::Local },
            &sig,
        ).unwrap();

        let rets = body.rets().into_iter().map(|r| r.ty.layout()).collect();

        func_ids.insert(*body_id, (func, sig, rets));
    }

    let mut bytes_count = 0;

    for (body_id, body) in &package.bodies {
        trans_body(&mut module, &func_ids, &data_ids, body_id, body, &mut bytes_count);
    }

    module.finalize_definitions();
    module.finish()
}

fn trans_body(
    module: &mut Module<impl Backend>,
    func_ids: &BTreeMap<syntax::ItemId, (FuncId, Signature, Vec<Layout>)>,
    data_ids: &BTreeMap<syntax::ItemId, (DataId, Layout)>,
    body_id: &syntax::ItemId,
    body: &syntax::Body,
    bytes_count: &mut usize,
) {
    let (func, sig, _) = &func_ids[body_id];
    let mut ctx = module.make_context();
    let mut func_ctx = FunctionBuilderContext::new();

    ctx.func.signature = sig.clone();
    ctx.func.name = ExternalName::user(0, func.as_u32());

    let mut builder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);
    let start_ebb = builder.create_ebb();

    builder.switch_to_block(start_ebb);

    let blocks = body.blocks.iter().map(|(id, _)| (*id, builder.create_ebb())).collect();
    let mut fx = FunctionCtx {
        pointer_type: module.target_config().pointer_type(),
        module,
        builder,
        body,
        func_ids,
        data_ids,
        blocks,
        locals: BTreeMap::new(),
        bytes_count,
    };

    let ssa_map = crate::analyze::analyze(&fx);

    fn local_place(
        fx: &mut FunctionCtx<impl Backend>,
        id: syntax::LocalId,
        layout: Layout,
        ssa: bool
    ) -> crate::place::Place {
        let place = if ssa {
            crate::place::Place::new_var(fx, id, layout)
        } else {
            crate::place::Place::new_stack(fx, layout)
        };

        fx.locals.insert(id, place);
        place
    }

    for ret in body.rets() {
        let layout = ret.ty.layout();

        match crate::pass::pass_mode(fx.module, layout) {
            crate::pass::PassMode::NoPass => {
                fx.locals.insert(ret.id, crate::place::Place {
                    kind: crate::place::PlaceKind::NoPlace,
                    layout,
                });
            },
            crate::pass::PassMode::ByVal(_) => {
                let ssa = ssa_map[&ret.id] == crate::analyze::SsaKind::Ssa;

                local_place(&mut fx, ret.id, layout, ssa);
            },
            crate::pass::PassMode::ByRef => {
                let ret_param = fx.builder.append_ebb_param(start_ebb, fx.pointer_type);

                fx.locals.insert(ret.id, crate::place::Place {
                    kind: crate::place::PlaceKind::Addr(crate::ptr::Pointer::addr(ret_param)),
                    layout,
                });
            },
        }
    }

    for arg in body.args() {
        let layout = arg.ty.layout();
        let value = crate::pass::value_for_param(&mut fx, start_ebb, layout);
        let ssa = ssa_map[&arg.id] == crate::analyze::SsaKind::Ssa;
        let place = local_place(&mut fx, arg.id, layout, ssa);

        if let Some(value) = value {
            place.store(&mut fx, value);
        }
    }
    
    for (id, decl) in body.locals.iter() {
        match &decl.kind {
            syntax::LocalKind::Var |
            syntax::LocalKind::Tmp => {
                local_place(&mut fx, *id, decl.ty.layout(), ssa_map[id] == crate::analyze::SsaKind::Ssa);
            },
            _ => {},
        }
    }
    
    let first_block = *fx.blocks.iter().next().unwrap().1;
    
    fx.builder.ins().jump(first_block, &[]);
    
    for (id, block) in &body.blocks {
        fx.builder.switch_to_block(fx.blocks[&id]);
        
        for stmt in &block.stmts {
            fx.trans_stmt(stmt);
        }
        
        fx.trans_term(&block.term);
    }

    fx.builder.seal_all_blocks();
    fx.builder.finalize();

    module.define_function(*func, &mut ctx).unwrap();
    module.clear_context(&mut ctx);
}
