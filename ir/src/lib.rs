pub mod builder;
mod display;
pub(crate) mod lexer;
pub mod parser;
pub mod visitor;

pub use builder::Builder;
use index_vec::IndexVec;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub decls: IndexVec<DeclId, Decl>,
    pub impls: IndexVec<ImplId, Impl>,
    pub bodies: IndexVec<BodyId, Body>,
}

index_vec::define_index_type! {
    pub struct DeclId = u32;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Decl {
    pub id: DeclId,
    pub linkage: Linkage,
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Linkage {
    Import,
    Export,
    Local,
    Hidden,
}

index_vec::define_index_type! {
    pub struct ImplId = u32;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Impl {
    pub id: ImplId,
    pub name: String,
    pub entries: Vec<ImplEntry>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImplEntry {
    Base(ImplId),
    Func(String, DeclId),
}

index_vec::define_index_type! {
    pub struct BodyId = u32;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Body {
    pub decl: DeclId,
    pub id: BodyId,
    pub locals: IndexVec<Local, LocalData>,
    pub blocks: IndexVec<Block, BlockData>,
}

index_vec::define_index_type! {
    pub struct Local = u32;
}

index_vec::define_index_type! {
    pub struct Block = u32;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocalData {
    pub id: Local,
    pub ty: Type,
    pub kind: LocalKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LocalKind {
    Ret,
    Arg,
    Var,
    Tmp,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Location {
    pub body: BodyId,
    pub block: Block,
    pub stmt: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockData {
    pub id: Block,
    pub stmts: Vec<Stmt>,
    pub term: Term,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Assign(Place, RValue),
    Call(Vec<Place>, Operand, Vec<Operand>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    Abort,
    Return,
    Jump(Block),
    Switch(Operand, Vec<u128>, Vec<Block>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RValue {
    Use(Operand),
    AddrOf(Place),
    Cast(Place, Type),
    Intrinsic(String, Vec<Operand>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operand {
    Place(Place),
    Const(Const),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Place {
    pub local: Local,
    pub elems: Vec<PlaceElem>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PlaceElem {
    Deref,
    Field(usize),
    Index(Operand),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Const {
    Undefined(Type),
    Scalar(u128, Type),
    Addr(DeclId),
    Tuple(Vec<Const>),
    Ptr(Box<Const>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    I8,
    I16,
    I32,
    I64,
    I128,
    F32,
    F64,
    Type(String),
    Vwt(String),
    Opaque(String),
    Ptr(Box<Type>),
    Tuple(Vec<Type>),
    Union(Vec<Type>),
    Func(Signature),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Signature {
    pub params: Vec<Type>,
    pub rets: Vec<Type>,
}

impl Body {
    pub fn gen_local(&self, gen: &str) -> Option<&LocalData> {
        self.locals.iter().find(|l| {
            if let Type::Ptr(to) = &l.ty {
                if let Type::Type(g) = &**to {
                    g == gen
                } else {
                    false
                }
            } else {
                false
            }
        })
    }
}

impl Place {
    pub fn new(local: Local) -> Self {
        Place {
            local,
            elems: Vec::new(),
        }
    }

    pub fn deref(mut self) -> Self {
        self.elems.push(PlaceElem::Deref);
        self
    }

    pub fn field(mut self, idx: usize) -> Self {
        self.elems.push(PlaceElem::Field(idx));
        self
    }

    pub fn index(mut self, idx: Operand) -> Self {
        self.elems.push(PlaceElem::Index(idx));
        self
    }
}

pub fn operand_type(module: &Module, body: &Body, op: &Operand) -> Type {
    match op {
        Operand::Place(place) => place_type(body, place),
        Operand::Const(c) => const_type(module, c),
    }
}

pub fn place_type(body: &Body, place: &Place) -> Type {
    let mut ty = body.locals[place.local].ty.clone();

    for elem in &place.elems {
        match elem {
            PlaceElem::Deref => match ty {
                Type::Ptr(to) => ty = (*to).clone(),
                _ => unreachable!(),
            },
            PlaceElem::Field(f) => match ty {
                Type::Tuple(tys) => ty = tys[*f].clone(),
                Type::Union(tys) => ty = tys[*f].clone(),
                _ => unreachable!(),
            },
            PlaceElem::Index(_) => unimplemented!(),
        }
    }

    ty
}

pub fn const_type(module: &Module, c: &Const) -> Type {
    match c {
        Const::Undefined(ty) => ty.clone(),
        Const::Scalar(_, ty) => ty.clone(),
        Const::Addr(decl) => module.decls[*decl].ty.clone(),
        Const::Ptr(to) => Type::Ptr(Box::new(const_type(module, to))),
        Const::Tuple(cs) => Type::Tuple(cs.iter().map(|c| const_type(module, c)).collect()),
    }
}

impl Type {
    pub fn signature(&self) -> Signature {
        match self {
            Type::Func(sig) => sig.clone(),
            _ => unreachable!(),
        }
    }
}
