use crate::layout::TyAndLayout;
use crate::ty::{Ty, Type};
use crate::{Body, BodyId, Func, FuncId, TypeDef, TypeDefId};
use std::collections::HashMap;
use std::sync::Arc;
use target_lexicon::Triple;

#[salsa::query_group(IrDatabaseStorage)]
pub trait IrDatabase {
    #[salsa::input]
    fn triple(&self) -> Arc<Triple>;

    #[salsa::invoke(crate::layout::layout_of)]
    fn layout_of(&self, ty: Ty) -> TyAndLayout;

    #[salsa::interned]
    fn intern_func(&self, func: Arc<Func>) -> FuncId;

    #[salsa::interned]
    fn intern_body(&self, body: Arc<Body>) -> BodyId;

    #[salsa::interned]
    fn intern_typedef(&self, def: Arc<TypeDef>) -> TypeDefId;

    #[salsa::interned]
    fn intern_type(&self, ty: Arc<Type>) -> Ty;

    #[salsa::invoke(crate::intrinsics::intrinsics)]
    fn intrinsics(&self) -> Arc<HashMap<&'static str, Ty>>;
}
