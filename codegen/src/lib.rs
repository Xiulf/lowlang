pub mod abi;
pub mod analyze;
pub mod obj_file;

use ir::layout::{Scalar, TyLayout};
use std::collections::HashMap;

pub trait Backend<'ctx>:
    Sized
    + DeclMethods<'ctx, Backend = Self>
    + TransMethods<'ctx, Backend = Self>
    + ConstMethods<'ctx, Backend = Self>
    + 'ctx
{
    type Module;
    type Context: 'ctx;
    type Builder: 'ctx;
    type Func: Copy;
    type Static: Copy;
    type Block: Copy;
    type Place: Place<'ctx, Backend = Self> + Clone;
    type Value: Value<'ctx, Backend = Self> + Clone;
    type Type: Type<'ctx, Backend = Self>;

    fn create_module(&mut self, target: &target_lexicon::Triple) -> Self::Module;
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
        body: &ir::Body,
    ) {
        let vals = eval::evaluate(mcx.ir, body, &mcx.target);
        let ty = ir::const_type(mcx.ir, &vals[0]);
        let layout = ir::layout::layout_of(&ty, &mcx.target);

        Self::Backend::alloc_const(mcx, &vals[0], layout, Some(id));
    }

    fn define_func(
        fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>,
        id: <Self::Backend as Backend<'ctx>>::Func,
    );

    fn func_prologue(fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>);
}

pub trait ConstMethods<'ctx> {
    type Backend: Backend<'ctx> + 'ctx;

    fn alloc_const(
        mcx: &mut ModuleCtx<'_, 'ctx, Self::Backend>,
        c: &ir::Const,
        layout: TyLayout,
        data_id: Option<<Self::Backend as Backend<'ctx>>::Static>,
    ) -> <Self::Backend as Backend<'ctx>>::Static;
}

pub trait TransMethods<'ctx> {
    type Backend: Backend<'ctx> + 'ctx;

    fn switch_to_block(
        fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>,
        block: <Self::Backend as Backend<'ctx>>::Block,
    );

    fn trans_init(
        fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>,
        place: <Self::Backend as Backend<'ctx>>::Place,
    );

    fn trans_drop(
        fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>,
        place: <Self::Backend as Backend<'ctx>>::Place,
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

    fn trans_set_discr(
        fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>,
        place: <Self::Backend as Backend<'ctx>>::Place,
        val: u128,
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
        let mut module = backend.create_module(&target);
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
        let mut func_ids = HashMap::new();
        let mut static_ids = HashMap::new();
        let ir = self.ir;

        for decl in &ir.decls {
            if let ir::Type::Func(_) = &decl.ty {
                func_ids.insert(decl.id, B::declare_func(&mut self, decl));
            } else {
                static_ids.insert(decl.id, B::declare_static(&mut self, decl));
            }
        }

        for body in &ir.bodies {
            let decl = &ir.decls[body.decl];

            if let ir::Type::Func(_) = &decl.ty {
                let func_id = func_ids.remove(&decl.id).unwrap();
                let builder = B::create_builder(&mut self.backend, &mut self.ctx);
                let mut fx = FunctionCtx::new(&mut self, builder, body);

                B::func_prologue(&mut fx);

                for block in &body.blocks {
                    let block_id = fx.blocks[&block.id];

                    B::switch_to_block(&mut fx, block_id);

                    for stmt in &block.stmts {
                        match stmt {
                            ir::Stmt::Init(local) => {
                                let place = fx.locals[local].clone();

                                B::trans_init(&mut fx, place)
                            }
                            ir::Stmt::Drop(local) => {
                                let place = fx.locals[local].clone();

                                B::trans_drop(&mut fx, place)
                            }
                            ir::Stmt::Assign(place, rvalue) => {
                                let place = B::trans_place(&mut fx, place);

                                B::trans_rvalue(&mut fx, place, rvalue);
                            }
                            ir::Stmt::SetDiscr(place, val) => {
                                let place = B::trans_place(&mut fx, place);

                                B::trans_set_discr(&mut fx, place, *val);
                            }
                            ir::Stmt::Call(rets, func, args) => {
                                let rets = rets
                                    .iter()
                                    .map(|r| B::trans_place(&mut fx, r))
                                    .collect::<Vec<_>>();

                                let args = args
                                    .iter()
                                    .map(|a| B::trans_op(&mut fx, a, None))
                                    .collect::<Vec<_>>();

                                B::trans_call(&mut fx, rets, func, args);
                            }
                        }
                    }

                    B::trans_term(&mut fx, &block.term);
                }

                B::define_func(&mut fx, func_id);
            } else {
                let static_id = static_ids.remove(&decl.id).unwrap();

                B::define_static(&mut self, static_id, body);
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

impl<'ir, 'ctx, 'mcx, B: Backend<'ctx>> FunctionCtx<'ir, 'ctx, 'mcx, B> {
    pub fn new(
        mcx: &'mcx mut ModuleCtx<'ir, 'ctx, B>,
        bcx: B::Builder,
        body: &'ir ir::Body,
    ) -> Self {
        FunctionCtx {
            mcx,
            bcx,
            body,
            locals: HashMap::new(),
            blocks: HashMap::new(),
        }
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
