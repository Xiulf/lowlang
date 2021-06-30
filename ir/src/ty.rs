pub use crate::layout::Integer;
use crate::layout::Primitive;
use crate::{Flags, TypeId};
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
    pub flags: Flags,
    pub repr: Repr,
    pub kind: TypeKind,
}

impl Flags {
    pub const OWNED: Self = Self(1 << 0);
    pub const C_REPR: Self = Self(1 << 1);
    pub const PACKED: Self = Self(1 << 2);
    pub const NON_NULL: Self = Self(1 << 3);
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
    Def(TypeId, Option<Vec<Subst>>),
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

        if let Some(id) = int.map.get(&ty) {
            *id
        } else {
            let id = Ty(int.vec.len() as u32);

            int.vec.push(ty.clone());
            int.map.insert(ty, id);
            id
        }
    }

    pub(crate) fn idx(self) -> usize {
        self.0 as usize
    }

    pub fn new(kind: TypeKind) -> Self {
        let ty = Arc::new(Type {
            flags: Flags::EMPTY,
            repr: Repr::default(),
            kind,
        });

        Self::intern(ty)
    }

    pub fn unit() -> Self {
        Self::new(typ::Unit)
    }

    pub fn ptr(self) -> Self {
        Self::new(typ::Ptr(self))
    }

    pub fn boxed(self) -> Self {
        Self::new(typ::Box(self))
    }

    pub fn owned(self) -> Self {
        self.flag(Flags::OWNED)
    }

    pub fn flag(self, flags: Flags) -> Self {
        let int = TYPE_INTERNER.write().unwrap();
        let ptr = Arc::as_ptr(&int.vec[self.0 as usize]) as *mut Type;

        unsafe {
            (*ptr).flags = (*ptr).flags.set(flags);
        }

        self
    }

    pub fn int(int: Integer, sign: bool) -> Self {
        Self::intern(Arc::new(Type {
            repr: Repr {
                scalar: Some(Primitive::Int(int, sign)),
                ..Repr::default()
            },
            flags: Flags::EMPTY,
            kind: typ::Unit,
        }))
    }

    pub fn generic() -> GenericType {
        GenericType { params: Vec::new() }
    }

    pub fn pointee(self) -> Option<Ty> {
        match self.lookup().kind {
            | typ::Ptr(to) => Some(to),
            | _ => None,
        }
    }

    pub fn pass_indirectly(self) -> bool {
        match self.lookup().kind {
            | typ::Var(_) => true,
            | typ::Tuple(ref ts) => ts.iter().any(|t| t.pass_indirectly()),
            | typ::Generic(_, t) => t.pass_indirectly(),
            | typ::Def(_, Some(ref sub)) => sub.iter().any(|s| match s {
                | Subst::Type(t) => t.pass_indirectly(),
                | _ => false,
            }),
            | _ => false,
        }
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
            | typ::Def(id, Some(ref sub)) => Ty::new(typ::Def(
                id,
                Some(
                    sub.iter()
                        .map(|s| match s {
                            | Subst::Type(t) => Subst::Type(t.subst(args, depth)),
                            | Subst::Figure(f) => Subst::Figure(*f),
                            | Subst::Symbol(s) => Subst::Symbol(s.clone()),
                        })
                        .collect(),
                ),
            )),
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
        let flags = match ty.pass_indirectly() {
            | true => Flags::IN,
            | false => Flags::EMPTY,
        };

        self.params.push(SigParam { ty, flags });
        self
    }

    pub fn ret(mut self, ty: Ty) -> Self {
        let flags = match ty.pass_indirectly() {
            | true => Flags::IN,
            | false => Flags::EMPTY,
        };

        self.rets.push(SigParam { ty, flags });
        self
    }
}

impl GenericVar {
    pub fn at(mut self, depth: u8) -> Self {
        self.0 = depth;
        self
    }

    pub fn idx(self) -> usize {
        self.1 as usize
    }

    pub fn depth(self) -> usize {
        self.0 as usize
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

#[macro_export]
macro_rules! sig {
    ($($param:expr),* => $($ret:expr),*) => {
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

#[macro_export]
macro_rules! generic {
    ($($p:ident : $kind:ident),*in $t:expr) => {{
        let mut generic = $crate::ty::Ty::generic();
        $(
            let $p = generic.add_param($crate::ty::GenericParam::$kind);
            let $p = $crate::ty::Ty::new($crate::ty::typ::Var($p));
        )*

        generic.finish($t)
    }};
}
