pub mod builder;
mod display;
pub mod layout;
pub(crate) mod lexer;
pub mod parser;
pub mod visitor;

pub use builder::Builder;
use index_vec::IndexVec;
use std::collections::HashMap;

pub type Decls = IndexVec<DeclId, Decl>;
pub type Impls = IndexVec<ImplId, Impl>;
pub type Bodies = IndexVec<BodyId, Body>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub decls: Decls,
    pub impls: Impls,
    pub bodies: Bodies,
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
    pub attrs: Attrs,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Linkage {
    Import,
    Export,
    Local,
    Hidden,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Attrs {
    pub c_abi: bool,
}

impl Default for Attrs {
    fn default() -> Self {
        Attrs { c_abi: false }
    }
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
    Init(Local),
    Drop(Local),
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
    U8,
    U16,
    U32,
    U64,
    U128,
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
    pub fn new(id: BodyId, decl: DeclId) -> Self {
        Body {
            id,
            decl,
            locals: IndexVec::new(),
            blocks: IndexVec::new(),
        }
    }

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

    pub fn args(&self) -> impl Iterator<Item = &LocalData> {
        self.locals.iter().filter(|l| l.kind == LocalKind::Arg)
    }

    pub fn rets(&self) -> impl Iterator<Item = &LocalData> {
        self.locals.iter().filter(|l| l.kind == LocalKind::Ret)
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
        Const::Addr(decl) => {
            let ty = module.decls[*decl].ty.clone();

            if let Type::Func(_) = ty {
                ty
            } else {
                Type::Ptr(Box::new(ty))
            }
        }
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

impl Signature {
    pub fn find_type_instances(
        self,
        args: &[Operand],
        rets: &[Place],
        module: &Module,
        body: &Body,
    ) -> HashMap<String, Type> {
        let mut res = HashMap::new();

        fn rec(param: Type, arg: Type, res: &mut HashMap<String, Type>) {
            match (param, arg) {
                (Type::Opaque(t), arg) => {
                    res.insert(t, arg);
                }
                (Type::Ptr(param), Type::Ptr(arg)) => rec(*param, *arg, res),
                (_, _) => {}
            }
        }

        for (param, arg) in self.params.into_iter().zip(args) {
            let arg_ty = operand_type(module, body, arg);

            rec(param, arg_ty, &mut res);
        }

        for (ret, place) in self.rets.into_iter().zip(rets) {
            let place_ty = place_type(body, place);

            rec(ret, place_ty, &mut res);
        }

        res
    }
}
