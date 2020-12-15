pub mod abi;
pub mod analyze;
pub mod obj_file;

use ir::layout::{Scalar, TyLayout};
use std::collections::HashMap;

pub trait Backend<'ctx>: Sized + DeclMethods<'ctx> + TransMethods<'ctx> + 'ctx {
    type Module;
    type Context: 'ctx;
    type Builder: 'ctx;
    type Func: Copy;
    type Static: Copy;
    type Block: Copy;
    type Place: Place<'ctx, Backend = Self> + Clone;
    type Value: Value<'ctx, Backend = Self> + Clone;
    type Type: Type<'ctx, Backend = Self>;

    fn create_module(&mut self) -> Self::Module;
    fn create_context(&mut self, module: &mut Self::Module) -> Self::Context;
    fn create_builder(&mut self, ctx: &mut Self::Context) -> Self::Builder;

    fn finish(mcx: ModuleCtx<'_, 'ctx, Self>) -> obj_file::ObjectFile;
}

pub trait DeclMethods<'ctx> {
    type Backend: Backend<'ctx> + 'ctx;

    fn declare_static(
        mcx: &mut ModuleCtx<'_, 'ctx, Self::Backend>,
        decl: &ir::Decl,
    ) -> <Self::Backend as Backend<'ctx>>::Static;

    fn declare_func(
        mcx: &mut ModuleCtx<'_, 'ctx, Self::Backend>,
        decl: &ir::Decl,
    ) -> <Self::Backend as Backend<'ctx>>::Func;

    fn define_static(
        mcx: &mut ModuleCtx<'_, 'ctx, Self::Backend>,
        id: <Self::Backend as Backend<'ctx>>::Static,
        bytes: Vec<u8>,
    );

    fn define_func(
        fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>,
        id: <Self::Backend as Backend<'ctx>>::Func,
    );

    fn func_prologue(fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>);
}

pub trait TransMethods<'ctx> {
    type Backend: Backend<'ctx> + 'ctx;

    fn switch_to_block(
        fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>,
        block: <Self::Backend as Backend<'ctx>>::Block,
    );

    fn trans_place(
        fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>,
        place: &ir::Place,
    ) -> <Self::Backend as Backend<'ctx>>::Place;

    fn trans_const(
        fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>,
        c: &ir::Const,
        into: Option<<Self::Backend as Backend<'ctx>>::Place>,
    ) -> <Self::Backend as Backend<'ctx>>::Value;

    fn trans_rvalue(
        fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>,
        place: <Self::Backend as Backend<'ctx>>::Place,
        rvalue: &ir::RValue,
    );

    fn trans_call(
        fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>,
        rets: Vec<<Self::Backend as Backend<'ctx>>::Place>,
        func: &ir::Operand,
        args: Vec<<Self::Backend as Backend<'ctx>>::Value>,
    );

    fn trans_term(fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>, term: &ir::Term);

    fn trans_op(
        fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>,
        op: &ir::Operand,
        into: Option<<Self::Backend as Backend<'ctx>>::Place>,
    ) -> <Self::Backend as Backend<'ctx>>::Value {
        match op {
            ir::Operand::Place(place) => {
                let place = Self::trans_place(fx, place);
                let value = place.to_value(fx);

                if let Some(into) = into {
                    into.store(fx, value.clone());
                }

                value
            }
            ir::Operand::Const(c) => Self::trans_const(fx, c, into),
        }
    }
}

pub trait Place<'ctx>: Sized {
    type Backend: Backend<'ctx>;

    fn layout(&self) -> &TyLayout;

    fn to_value(
        self,
        fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>,
    ) -> <Self::Backend as Backend<'ctx>>::Value;

    fn deref(self, fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>) -> Self;

    fn index(
        self,
        fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>,
        idx: <Self::Backend as Backend<'ctx>>::Value,
    ) -> Self;

    fn field(self, fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>, idx: usize) -> Self;

    fn store(
        self,
        fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>,
        from: <Self::Backend as Backend<'ctx>>::Value,
    );

    fn write_place_ref(self, fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>, dest: Self);

    fn downcast_variant(
        self,
        fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>,
        variant: usize,
    ) -> Self;
}

pub trait Value<'ctx>: Sized {
    type Backend: Backend<'ctx>;
    type Raw;

    fn layout(&self) -> &TyLayout;

    fn load_scalar(self, fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>) -> Self::Raw;

    fn load_scalar_pair(
        self,
        fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>,
    ) -> (Self::Raw, Self::Raw);

    fn cast(self, fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>, layout: TyLayout) -> Self;

    fn field(self, fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>, idx: usize) -> Self;

    fn deref(self, fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>) -> Self;
}

pub trait Type<'ctx>: Sized {
    type Backend: Backend<'ctx>;
    type Raw: Copy;

    fn ir_type(layout: &TyLayout, mcx: &ModuleCtx<'_, 'ctx, Self::Backend>) -> Option<Self::Raw>;

    fn ir_pair_type(
        layout: &TyLayout,
        mcx: &ModuleCtx<'_, 'ctx, Self::Backend>,
    ) -> Option<(Self::Raw, Self::Raw)>;

    fn scalar_ty(scalar: &Scalar, mcx: &ModuleCtx<'_, 'ctx, Self::Backend>) -> Self::Raw;
}

pub struct ModuleCtx<'ir, 'ctx, B: Backend<'ctx>> {
    pub backend: B,
    pub target: target_lexicon::Triple,
    pub module: B::Module,
    pub ctx: B::Context,
    pub ir: &'ir ir::Module,
}

pub struct FunctionCtx<'ir, 'ctx, 'mcx, B: Backend<'ctx>> {
    pub mcx: &'mcx mut ModuleCtx<'ir, 'ctx, B>,
    pub bcx: B::Builder,
    pub body: &'ir ir::Body,
    pub blocks: HashMap<ir::Block, B::Block>,
    pub locals: HashMap<ir::Local, B::Place>,
}

impl<'ir, 'ctx, B: Backend<'ctx>> ModuleCtx<'ir, 'ctx, B> {
    pub fn new(ir: &'ir ir::Module, target: target_lexicon::Triple, mut backend: B) -> Self {
        let mut module = backend.create_module();
        let ctx = backend.create_context(&mut module);

        ModuleCtx {
            backend,
            target,
            module,
            ctx,
            ir,
        }
    }

    pub fn build(mut self) -> obj_file::ObjectFile {
        let mut func_ids = Vec::new();
        let mut static_ids: Vec::new();
        let ir = self.ir;

        for decl in &ir.decls {
            if let ir::Type::Func(_) = &decl.ty {
                func_ids.push(B::declare_func(&mut self, decl));
            } else {
                static_ids.push(B::declare_static(&mut self, decl));
            }
        }

        B::finish(self)
    }

    pub fn ir_type(&self, layout: &TyLayout) -> Option<<B::Type as Type<'ctx>>::Raw> {
        <B::Type as Type<'ctx>>::ir_type(layout, self)
    }

    pub fn ir_pair_type(
        &self,
        layout: &TyLayout,
    ) -> Option<(<B::Type as Type<'ctx>>::Raw, <B::Type as Type<'ctx>>::Raw)> {
        <B::Type as Type<'ctx>>::ir_pair_type(layout, self)
    }

    pub fn scalar_ty(&self, scalar: &Scalar) -> <B::Type as Type<'ctx>>::Raw {
        <B::Type as Type<'ctx>>::scalar_ty(scalar, self)
    }
}

impl<'ir, 'ctx, B: Backend<'ctx>> std::ops::Deref for ModuleCtx<'ir, 'ctx, B> {
    type Target = B;

    fn deref(&self) -> &Self::Target {
        &self.backend
    }
}

impl<'ir, 'ctx, B: Backend<'ctx>> std::ops::DerefMut for ModuleCtx<'ir, 'ctx, B> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.backend
    }
}

impl<'ir, 'ctx, 'mcx, B: Backend<'ctx>> std::ops::Deref for FunctionCtx<'ir, 'ctx, 'mcx, B> {
    type Target = ModuleCtx<'ir, 'ctx, B>;

    fn deref(&self) -> &Self::Target {
        self.mcx
    }
}

impl<'ir, 'ctx, 'mcx, B: Backend<'ctx>> std::ops::DerefMut for FunctionCtx<'ir, 'ctx, 'mcx, B> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.mcx
    }
}
