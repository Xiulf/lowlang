pub use crate::layout::Integer;
use crate::layout::Primitive;
use crate::Flags;
use arena::{Arena, Idx};
use std::collections::HashMap;
use std::lazy::SyncLazy;
use std::sync::Arc;
use std::sync::RwLock;

static TYPE_INTERNER: SyncLazy<RwLock<TypeInterner>> = SyncLazy::new(Default::default);

#[derive(Default)]
struct TypeInterner {
    map: HashMap<Arc<Type>, Ty>,
    vec: Vec<Arc<Type>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ty(u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub repr: Repr,
    pub kind: TypeKind,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct Repr {
    pub scalar: Option<Primitive>,
    pub valid_range_start: Option<u128>,
    pub valid_range_end: Option<u128>,
    pub uninhabited: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Unit,
    Ptr(Ty),
    Box(Ty),
    Tuple(Vec<Ty>),
    Var(GenericVar),
    Func(Signature),
    Generic(Vec<GenericParam>, Ty),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Signature {
    pub params: Vec<SigParam>,
    pub rets: Vec<SigParam>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SigParam {
    pub ty: Ty,
    pub flags: Flags,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GenericVar(pub(crate) u8, pub(crate) u8);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericParam {
    Type,
    Figure,
    Symbol,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Subst {
    Type(Ty),
    Figure(u128),
    Symbol(String),
}

pub struct GenericType {
    params: Vec<GenericParam>,
}

pub mod typ {
    pub use super::TypeKind::*;
}

impl Ty {
    pub fn lookup(self) -> Arc<Type> {
        TYPE_INTERNER.read().unwrap().vec[self.0 as usize].clone()
    }

    fn intern(ty: Arc<Type>) -> Self {
        let mut int = TYPE_INTERNER.write().unwrap();
        let id = Ty(int.vec.len() as u32);

        int.vec.push(ty.clone());
        int.map.insert(ty, id);
        id
    }

    pub fn new(kind: TypeKind) -> Self {
        let ty = Arc::new(Type { repr: Repr::default(), kind });

        Self::intern(ty)
    }

    pub fn ptr(self) -> Self {
        Self::new(typ::Ptr(self))
    }

    pub fn boxed(self) -> Self {
        Self::new(typ::Box(self))
    }

    pub fn int(int: Integer, sign: bool) -> Self {
        Self::intern(Arc::new(Type {
            repr: Repr {
                scalar: Some(Primitive::Int(int, sign)),
                ..Repr::default()
            },
            kind: typ::Unit,
        }))
    }

    pub fn generic() -> GenericType {
        GenericType { params: Vec::new() }
    }

    pub fn subst(self, args: &[Subst], depth: u8) -> Self {
        match self.lookup().kind {
            | typ::Ptr(to) => Ty::new(typ::Ptr(to.subst(args, depth))),
            | typ::Box(to) => Ty::new(typ::Box(to.subst(args, depth))),
            | typ::Var(GenericVar(d2, idx)) if depth == d2 => match args[idx as usize] {
                | Subst::Type(t) => t,
                | _ => panic!("Cannot substitute type"),
            },
            | typ::Func(ref sig) => Ty::new(typ::Func(Signature {
                params: sig
                    .params
                    .iter()
                    .map(|p| SigParam {
                        ty: p.ty.subst(args, depth),
                        flags: p.flags,
                    })
                    .collect(),
                rets: sig
                    .rets
                    .iter()
                    .map(|r| SigParam {
                        ty: r.ty.subst(args, depth),
                        flags: r.flags,
                    })
                    .collect(),
            })),
            | typ::Generic(ref params, ty) => Ty::new(typ::Generic(params.clone(), ty.subst(args, depth + 1))),
            | _ => self,
        }
    }
}

impl Flags {
    pub const IN: Self = Self(1 << 0);
    pub const OUT: Self = Self(1 << 1);
}

impl Signature {
    pub fn new() -> Self {
        Self {
            params: Vec::new(),
            rets: Vec::new(),
        }
    }

    pub fn param(mut self, ty: Ty) -> Self {
        let flags = match ty.lookup().kind {
            | typ::Var(_) => Flags::IN,
            | _ => Flags::EMPTY,
        };

        self.params.push(SigParam { ty, flags });
        self
    }

    pub fn ret(mut self, ty: Ty) -> Self {
        let flags = match ty.lookup().kind {
            | typ::Var(_) => Flags::OUT,
            | _ => Flags::EMPTY,
        };

        self.rets.push(SigParam { ty, flags });
        self
    }
}

#[macro_export]
macro_rules! sig {
    ($($param:ident),* -> $($ret:ident),*) => {
        $crate::ty::Ty::new($crate::ty::typ::Func($crate::ty::Signature {
            params: vec![$($crate::ty::SigParam {
                ty: $param,
                flags: $crate::Flags::EMPTY
            }),*],
            rets: vec![$($crate::ty::SigParam {
                ty: $ret,
                flags: $crate::Flags::EMPTY
            }),*],
        }))
    };
}

impl GenericVar {
    pub fn at(mut self, depth: u8) -> Self {
        self.0 = depth;
        self
    }
}

impl GenericType {
    pub fn add_param(&mut self, param: GenericParam) -> GenericVar {
        let i = self.params.len();

        self.params.push(param);

        GenericVar(0, i as u8)
    }

    pub fn finish(self, ty: Ty) -> Ty {
        Ty::new(typ::Generic(self.params, ty))
    }
}
