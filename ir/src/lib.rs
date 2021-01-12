#![feature(iterator_fold_self)]

pub mod builder;
mod display;
pub mod graph;
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
    pub ty: Ty,
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
    pub ty: Ty,
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
    SetDiscr(Place, u128),
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
    GetDiscr(Place),
    Cast(Place, Ty),
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
    Downcast(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Const {
    Undefined(Ty),
    Scalar(u128, Ty),
    Addr(DeclId),
    Tuple(Vec<Const>),
    Ptr(Box<Const>),
    Variant(usize, Vec<Const>, Ty),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ty {
    pub info: TyInfo,
    pub kind: Type,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct TyInfo {
    pub abi: Option<layout::Abi>,
    pub valid_range: Option<std::ops::RangeInclusive<u128>>,
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
    Ptr(Box<Ty>),
    Box(Box<Ty>),
    Tuple(Vec<Ty>),
    Union(Vec<Ty>),
    Tagged(Vec<Ty>),
    Func(Signature),
    Discr(Box<Ty>),
    Recurse(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Signature {
    pub params: Vec<Ty>,
    pub rets: Vec<Ty>,
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
            if let Type::Ptr(to) = &l.ty.kind {
                if let Type::Type(g) = &to.kind {
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

impl Ty {
    pub fn new(kind: Type) -> Self {
        Ty {
            info: TyInfo::default(),
            kind,
        }
    }

    pub fn with_valid_range(mut self, range: std::ops::RangeInclusive<u128>) -> Self {
        self.info.valid_range = Some(range);
        self
    }

    pub fn with_abi(mut self, abi: layout::Abi) -> Self {
        self.info.abi = Some(abi);
        self
    }
}

impl std::ops::Deref for Ty {
    type Target = Type;

    fn deref(&self) -> &Self::Target {
        &self.kind
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

    pub fn downcast(mut self, idx: usize) -> Self {
        self.elems.push(PlaceElem::Downcast(idx));
        self
    }
}

pub fn operand_type(module: &Module, body: &Body, op: &Operand) -> Ty {
    match op {
        Operand::Place(place) => place_type(body, place),
        Operand::Const(c) => const_type(module, c),
    }
}

pub fn place_type(body: &Body, place: &Place) -> Ty {
    let mut ty = body.locals[place.local].ty.clone();
    let mut i = 0;

    while i < place.elems.len() {
        let elem = &place.elems[i];

        match elem {
            PlaceElem::Deref => match ty.kind {
                Type::Ptr(to) => ty = *to,
                Type::Box(to) => ty = *to,
                _ => unreachable!(),
            },
            PlaceElem::Field(f) => match ty.kind {
                Type::Tuple(mut tys) => ty = tys.swap_remove(*f),
                Type::Union(mut tys) => ty = tys.swap_remove(*f),
                Type::Box(to) => {
                    ty = *to;
                    continue;
                }
                _ => unreachable!(),
            },
            PlaceElem::Index(_) => unimplemented!(),
            PlaceElem::Downcast(v) => match ty.kind {
                Type::Tagged(mut tys) => ty = tys.swap_remove(*v),
                Type::Box(to) => {
                    ty = *to;
                    continue;
                }
                _ => unreachable!(),
            },
        }

        i += 1;
    }

    ty
}

pub fn const_type(module: &Module, c: &Const) -> Ty {
    match c {
        Const::Undefined(ty) => ty.clone(),
        Const::Scalar(_, ty) => ty.clone(),
        Const::Addr(decl) => {
            let ty = module.decls[*decl].ty.clone();

            if let Type::Func(_) = ty.kind {
                ty
            } else {
                Ty::new(Type::Ptr(Box::new(ty)))
            }
        }
        Const::Ptr(to) => Ty::new(Type::Ptr(Box::new(const_type(module, to)))),
        Const::Tuple(cs) => Ty::new(Type::Tuple(
            cs.iter().map(|c| const_type(module, c)).collect(),
        )),
        Const::Variant(_, _, ty) => ty.clone(),
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

impl Ty {
    pub fn access(&self) -> Self {
        let top = self.clone();

        self.replace(0, top)
    }

    pub fn replace(&self, i: usize, with: Self) -> Self {
        match &self.kind {
            Type::Recurse(j) if *j == i => with,
            Type::Recurse(j) => Ty {
                info: self.info.clone(),
                kind: Type::Recurse(*j - 1),
            },
            Type::Ptr(to) => Ty {
                info: self.info.clone(),
                kind: Type::Ptr(Box::new(to.replace(i + 1, with))),
            },
            Type::Box(to) => Ty {
                info: self.info.clone(),
                kind: Type::Box(Box::new(to.replace(i + 1, with))),
            },
            Type::Tuple(tys) => Ty {
                info: self.info.clone(),
                kind: Type::Tuple(tys.iter().map(|t| t.replace(i + 1, with.clone())).collect()),
            },
            Type::Union(tys) => Ty {
                info: self.info.clone(),
                kind: Type::Union(tys.iter().map(|t| t.replace(i + 1, with.clone())).collect()),
            },
            Type::Tagged(tys) => Ty {
                info: self.info.clone(),
                kind: Type::Tagged(tys.iter().map(|t| t.replace(i + 1, with.clone())).collect()),
            },
            Type::Discr(to) => Ty {
                info: self.info.clone(),
                kind: Type::Discr(Box::new(to.replace(i + 1, with))),
            },
            Type::Func(sig) => Ty {
                info: self.info.clone(),
                kind: Type::Func(Signature {
                    params: sig
                        .params
                        .iter()
                        .map(|t| t.replace(i + 1, with.clone()))
                        .collect(),
                    rets: sig
                        .rets
                        .iter()
                        .map(|t| t.replace(i + 1, with.clone()))
                        .collect(),
                }),
            },
            _ => self.clone(),
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
    ) -> HashMap<String, Ty> {
        let mut res = HashMap::new();

        fn rec(param: Ty, arg: Ty, res: &mut HashMap<String, Ty>) {
            match (param.kind, &arg.kind) {
                (Type::Opaque(t), _) => {
                    res.insert(t, arg);
                }
                (Type::Ptr(param), Type::Ptr(arg)) => rec(*param, (**arg).clone(), res),
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
