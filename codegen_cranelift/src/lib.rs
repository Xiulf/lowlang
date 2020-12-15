#![feature(decl_macro)]

mod decl;
pub mod place;
pub mod ptr;
mod trans;
pub mod value;

use codegen::*;
use cranelift_object::ObjectModule;
use std::collections::HashMap;
use std::marker::PhantomData;

mod clif {
    pub use cranelift::codegen::ir;
    pub use cranelift::codegen::Context;
    pub use cranelift::frontend::*;
    pub use cranelift::prelude::*;
    pub use cranelift_module::{
        default_libcall_names, DataContext, DataId, FuncId, Linkage, Module,
    };
}

pub struct ClifBackend<'ctx> {
    func_ctx: *mut clif::FunctionBuilderContext,
    func_ids: HashMap<ir::DeclId, (clif::FuncId, clif::Signature)>,
    ssa_vars: u32,
    _marker: PhantomData<&'ctx cranelift::codegen::Context>,
}

impl<'ctx> ClifBackend<'ctx> {
    pub fn new() -> Self {
        ClifBackend {
            func_ctx: std::ptr::null_mut(),
            func_ids: HashMap::new(),
            ssa_vars: 0,
            _marker: PhantomData,
        }
    }
}

impl<'ctx> Backend<'ctx> for ClifBackend<'ctx> {
    type Module = ObjectModule;
    type Context = clif::Context;
    type Builder = clif::FunctionBuilder<'ctx>;
    type Func = clif::FuncId;
    type Static = clif::DataId;
    type Block = clif::Block;
    type Place = place::Place<'ctx>;
    type Value = value::Value<'ctx>;
    type Type = ClifType;

    fn create_module(&mut self, target: &target_lexicon::Triple) -> Self::Module {
        let flags_builder = clif::settings::builder();
        let flags = clif::settings::Flags::new(flags_builder);
        let isa = clif::isa::lookup(target.clone()).unwrap().finish(flags);

        let builder =
            cranelift_object::ObjectBuilder::new(isa, "test", clif::default_libcall_names())
                .unwrap();

        cranelift_object::ObjectModule::new(builder)
    }

    fn create_context(&mut self, module: &mut Self::Module) -> Self::Context {
        <Self::Module as clif::Module>::make_context(module)
    }

    fn create_builder(&mut self, ctx: &mut Self::Context) -> Self::Builder {
        self.func_ctx = Box::into_raw(Box::new(clif::FunctionBuilderContext::new()));

        let func: *mut _ = &mut ctx.func;

        clif::FunctionBuilder::new(unsafe { &mut *func }, unsafe { &mut *self.func_ctx })
    }

    fn finish(mcx: ModuleCtx<'_, 'ctx, Self>) -> obj_file::ObjectFile {
        let mut obj_file = obj_file::ObjectFile::new();
        let product = mcx.module.finish();
        let bytes = product.emit().unwrap();

        obj_file.write(&bytes);
        obj_file
    }
}

impl<'ctx> ClifBackend<'ctx> {
    pub(crate) fn next_ssa_var(&mut self) -> u32 {
        self.ssa_vars += 1;
        self.ssa_vars
    }
}

impl<'ctx> Drop for ClifBackend<'ctx> {
    fn drop(&mut self) {
        unsafe {
            Box::from_raw(self.func_ctx);
        }
    }
}

pub struct ClifType;

impl<'ctx> Type<'ctx> for ClifType {
    type Backend = ClifBackend<'ctx>;
    type Raw = clif::Type;

    fn ir_type(
        layout: &ir::layout::TyLayout,
        mcx: &ModuleCtx<'_, 'ctx, ClifBackend<'ctx>>,
    ) -> Option<Self::Raw> {
        if let ir::layout::Abi::Scalar(s) = &layout.abi {
            Some(Self::scalar_ty(s, mcx))
        } else {
            None
        }
    }

    fn ir_pair_type(
        layout: &ir::layout::TyLayout,
        mcx: &ModuleCtx<'_, 'ctx, ClifBackend<'ctx>>,
    ) -> Option<(Self::Raw, Self::Raw)> {
        if let ir::layout::Abi::ScalarPair(a, b) = &layout.abi {
            Some((Self::scalar_ty(a, mcx), Self::scalar_ty(b, mcx)))
        } else {
            None
        }
    }

    fn scalar_ty(
        scalar: &ir::layout::Scalar,
        mcx: &ModuleCtx<'_, 'ctx, ClifBackend<'ctx>>,
    ) -> Self::Raw {
        use clif::{types, Module};
        use ir::layout::{Integer, Primitive};
        match &scalar.value {
            Primitive::Int(int, _) => match int {
                Integer::I8 => types::I8,
                Integer::I16 => types::I16,
                Integer::I32 => types::I32,
                Integer::I64 => types::I64,
                Integer::I128 => types::I128,
            },
            Primitive::F32 => types::F32,
            Primitive::F64 => types::F64,
            Primitive::Pointer => mcx.module.target_config().pointer_type(),
        }
    }
}

pub fn mk_signature<'ctx>(
    mcx: &ModuleCtx<'_, 'ctx, ClifBackend<'ctx>>,
    sig: &ir::Signature,
) -> clif::Signature {
    let mut out = <ObjectModule as clif::Module>::make_signature(&mcx.module);
    let ptr_ty = <ObjectModule as clif::Module>::target_config(&mcx.module).pointer_type();

    for ret in &sig.rets {
        let layout = ir::layout::layout_of(ret, &mcx.target);

        match abi::get_pass_mode(mcx, &layout) {
            abi::PassMode::NoPass => {}
            abi::PassMode::ByVal(ty) => {
                out.returns.push(clif::AbiParam::new(ty));
            }
            abi::PassMode::ByValPair(a, b) => {
                out.returns.push(clif::AbiParam::new(a));
                out.returns.push(clif::AbiParam::new(b));
            }
            abi::PassMode::ByRef { size: _ } => {
                out.params.push(clif::AbiParam::new(ptr_ty));
            }
        }
    }

    for param in &sig.params {
        let layout = ir::layout::layout_of(param, &mcx.target);

        match abi::get_pass_mode(mcx, &layout) {
            abi::PassMode::NoPass => {}
            abi::PassMode::ByVal(ty) => {
                out.params.push(clif::AbiParam::new(ty));
            }
            abi::PassMode::ByValPair(a, b) => {
                out.params.push(clif::AbiParam::new(a));
                out.params.push(clif::AbiParam::new(b));
            }
            abi::PassMode::ByRef { size: _ } => {
                out.params.push(clif::AbiParam::new(ptr_ty));
            }
        }
    }

    out
}
