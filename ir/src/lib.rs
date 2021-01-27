pub mod builder;
mod printing;

use index_vec::IndexVec;
use std::collections::HashMap;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub defs: IndexVec<DefId, Decl>,
    pub bodies: HashMap<DefId, Body>,
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
    pub instrs: Vec<Instr>,
    pub term: Term,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Instr {
    Const(Var, Const, Type),
    Load(Var, Var),
    Store(Var, Var),
    Call(Vec<Var>, Var, Vec<Var>),
    Offset(Var, Var, Var),
    Add(Var, Var, Var),
    Sub(Var, Var, Var),
    Mul(Var, Var, Var),
    Div(Var, Var, Var),
    Rem(Var, Var, Var),
    Eq(Var, Var, Var),
    Ne(Var, Var, Var),
    Lt(Var, Var, Var),
    Le(Var, Var, Var),
    Gt(Var, Var, Var),
    Ge(Var, Var, Var),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Term {
    Unset,
    Abort,
    Return,
    Br(Block),
    BrNz(Var, Block),
    BrZ(Var, Block),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Const {
    Undefined,
    Scalar(u128),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Int(u8, bool),
    Float(u8),
    Ptr(Box<Type>),
    Box(Box<Type>),
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
