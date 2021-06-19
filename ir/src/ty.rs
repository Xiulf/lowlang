pub use crate::layout::Integer;
use crate::layout::Primitive;
use crate::Flags;
use arena::{Arena, Idx};
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ty(Arc<Type>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub repr: Repr,
    pub kind: TypeKind,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Repr {
    pub scalar: Option<Primitive>,
    pub valid_range_start: Option<u128>,
    pub valid_range_end: Option<u128>,
    pub uninhabited: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeKind {
    Unit,
    Ptr(Ty),
    Box(Ty),
    Var(GenericVar),
    Func(Signature),
    Generic(Vec<GenericParam>, Ty),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Signature {
    pub params: Vec<SigParam>,
    pub rets: Vec<SigParam>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SigParam {
    pub ty: Ty,
    pub flags: Flags,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GenericVar(pub(crate) u8, pub(crate) u8);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GenericParam {
    Type,
    Figure,
    Symbol,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    pub fn new(kind: TypeKind) -> Self {
        Self(Arc::new(Type { repr: Repr::default(), kind }))
    }

    pub fn ptr(self) -> Self {
        Self::new(typ::Ptr(self))
    }

    pub fn boxed(self) -> Self {
        Self::new(typ::Box(self))
    }

    pub fn int(int: Integer, sign: bool) -> Self {
        Self(Arc::new(Type {
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

    pub fn cloned(&self) -> Self {
        Self(Arc::new((*self.0).clone()))
    }

    pub fn subst(&self, args: &[Subst], depth: u8) -> Self {
        match self.kind {
            | typ::Ptr(ref to) => Ty::new(typ::Ptr(to.subst(args, depth))),
            | typ::Box(ref to) => Ty::new(typ::Box(to.subst(args, depth))),
            | typ::Var(GenericVar(d2, idx)) if depth == d2 => match args[idx as usize] {
                | Subst::Type(ref t) => t.clone(),
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
            | typ::Generic(ref params, ref ty) => Ty::new(typ::Generic(params.clone(), ty.subst(args, depth + 1))),
            | _ => self.clone(),
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
        let flags = match ty.kind {
            | typ::Var(_) => Flags::IN,
            | _ => Flags::EMPTY,
        };

        self.params.push(SigParam { ty, flags });
        self
    }

    pub fn ret(mut self, ty: Ty) -> Self {
        let flags = match ty.kind {
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
                ty: $param.clone(),
                flags: $crate::Flags::EMPTY
            }),*],
            rets: vec![$($crate::ty::SigParam {
                ty: $ret.clone(),
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

impl std::ops::Deref for Ty {
    type Target = Type;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
