pub mod builder;
mod printing;

use index_vec::IndexVec;
use std::collections::HashMap;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Module {
    subset: ModuleSubset,
    pub bodies: HashMap<DefId, Body>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct ModuleSubset {
    pub defs: IndexVec<DefId, Decl>,
    pub types: HashMap<DefId, TypeDef>,
}

index_vec::define_index_type! {
    pub struct DefId = u32;
    DEFAULT = DefId::from_raw_unchecked(0);
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Decl {
    pub id: DefId,
    pub name: String,
    pub linkage: Linkage,
    pub kind: DeclKind,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DeclKind {
    Def(Type),
    Type,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Linkage {
    Import,
    Export,
    Local,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeDef {
    pub variants: Vec<Variant>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant {
    pub tys: Vec<Type>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Body {
    pub vars: IndexVec<Var, Variable>,
    pub blocks: IndexVec<Block, BasicBlock>,
}

index_vec::define_index_type! {
    pub struct Var = u32;
    DEFAULT = Var::from_raw_unchecked(0);
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variable {
    pub id: Var,
    pub ty: Type,
}

index_vec::define_index_type! {
    pub struct Block = u32;
    DEFAULT = Block::from_raw_unchecked(0);
}

#[derive(Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BasicBlock {
    pub id: Block,
    pub params: Vec<Var>,
    pub instrs: Vec<Instr>,
    pub term: Term,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Instr {
    pub outputs: Vec<Var>,
    pub name: String,
    pub args: Vec<Operand>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Term {
    Unset,
    Abort,
    Return(Vec<Operand>),
    Br(Block, Vec<Var>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Operand {
    Var(Var),
    Block(Block),
    Const(Const),
    Type(Type),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Const {
    Undefined,
    Scalar(u128),
    Addr(DefId),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Int(u8, bool),
    Float(u8),
    Ptr(Box<Type>),
    Box(Box<Type>),
    Tuple(Vec<Type>),
    Func(Signature),
    Def(DefId),
    Var(TypeVar),
    Forall(Vec<TypeVar>, Box<Type>),
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVar(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Signature {
    pub params: Vec<Type>,
    pub rets: Vec<Type>,
}

impl Default for Term {
    fn default() -> Self {
        Term::Unset
    }
}

impl std::ops::Deref for Module {
    type Target = ModuleSubset;

    fn deref(&self) -> &Self::Target {
        &self.subset
    }
}

impl std::ops::DerefMut for Module {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.subset
    }
}

impl Into<Operand> for Var {
    fn into(self) -> Operand {
        Operand::Var(self)
    }
}

impl Into<Operand> for Block {
    fn into(self) -> Operand {
        Operand::Block(self)
    }
}

impl Into<Operand> for Const {
    fn into(self) -> Operand {
        Operand::Const(self)
    }
}

impl Into<Operand> for Type {
    fn into(self) -> Operand {
        Operand::Type(self)
    }
}
