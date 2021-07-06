use crate::db::IrDatabase;
pub use crate::layout::Integer;
use crate::layout::Primitive;
use crate::{Flags, TypeId};
use std::sync::Arc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ty(salsa::InternId);

impl salsa::InternKey for Ty {
    fn from_intern_id(v: salsa::InternId) -> Self {
        Self(v)
    }

    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
}

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
    pub fn lookup(self, db: &dyn IrDatabase) -> Arc<Type> {
        db.lookup_intern_type(self)
    }

    fn intern(db: &dyn IrDatabase, ty: Arc<Type>) -> Self {
        db.intern_type(ty)
    }

    pub(crate) fn idx(self) -> usize {
        self.0.as_usize()
    }

    pub fn new(db: &dyn IrDatabase, kind: TypeKind) -> Self {
        let ty = Arc::new(Type {
            flags: Flags::EMPTY,
            repr: Repr::default(),
            kind,
        });

        Self::intern(db, ty)
    }

    pub fn unit(db: &dyn IrDatabase) -> Self {
        Self::new(db, typ::Unit)
    }

    pub fn ptr(self, db: &dyn IrDatabase) -> Self {
        Self::new(db, typ::Ptr(self))
    }

    pub fn boxed(self, db: &dyn IrDatabase) -> Self {
        Self::new(db, typ::Box(self))
    }

    pub fn owned(self, db: &dyn IrDatabase) -> Self {
        self.flag(db, Flags::OWNED)
    }

    pub fn flag(self, db: &dyn IrDatabase, flags: Flags) -> Self {
        let ty = self.lookup(db);
        let ptr = Arc::as_ptr(&ty) as *mut Type;

        unsafe {
            (*ptr).flags = (*ptr).flags.set(flags);
        }

        self
    }

    pub fn int(db: &dyn IrDatabase, int: Integer, sign: bool) -> Self {
        Self::intern(
            db,
            Arc::new(Type {
                repr: Repr {
                    scalar: Some(Primitive::Int(int, sign)),
                    ..Repr::default()
                },
                flags: Flags::EMPTY,
                kind: typ::Unit,
            }),
        )
    }

    pub fn generic() -> GenericType {
        GenericType { params: Vec::new() }
    }

    pub fn pointee(self, db: &dyn IrDatabase) -> Option<Ty> {
        match self.lookup(db).kind {
            | typ::Ptr(to) => Some(to),
            | _ => None,
        }
    }

    pub fn pass_indirectly(self, db: &dyn IrDatabase) -> bool {
        match self.lookup(db).kind {
            | typ::Var(_) => true,
            | typ::Tuple(ref ts) => ts.iter().any(|t| t.pass_indirectly(db)),
            | typ::Generic(_, t) => t.pass_indirectly(db),
            | typ::Def(_, Some(ref sub)) => sub.iter().any(|s| match s {
                | Subst::Type(t) => t.pass_indirectly(db),
                | _ => false,
            }),
            | _ => false,
        }
    }

    pub fn subst(self, db: &dyn IrDatabase, args: &[Subst], depth: u8) -> Self {
        match self.lookup(db).kind {
            | typ::Ptr(to) => Ty::new(db, typ::Ptr(to.subst(db, args, depth))),
            | typ::Box(to) => Ty::new(db, typ::Box(to.subst(db, args, depth))),
            | typ::Var(GenericVar(d2, idx)) if depth == d2 => match args[idx as usize] {
                | Subst::Type(t) => t,
                | _ => panic!("Cannot substitute type"),
            },
            | typ::Func(ref sig) => Ty::new(
                db,
                typ::Func(Signature {
                    params: sig
                        .params
                        .iter()
                        .map(|p| SigParam {
                            ty: p.ty.subst(db, args, depth),
                            flags: p.flags,
                        })
                        .collect(),
                    rets: sig
                        .rets
                        .iter()
                        .map(|r| SigParam {
                            ty: r.ty.subst(db, args, depth),
                            flags: r.flags,
                        })
                        .collect(),
                }),
            ),
            | typ::Generic(ref params, ty) => Ty::new(db, typ::Generic(params.clone(), ty.subst(db, args, depth + 1))),
            | typ::Def(id, Some(ref sub)) => Ty::new(
                db,
                typ::Def(
                    id,
                    Some(
                        sub.iter()
                            .map(|s| match s {
                                | Subst::Type(t) => Subst::Type(t.subst(db, args, depth)),
                                | Subst::Figure(f) => Subst::Figure(*f),
                                | Subst::Symbol(s) => Subst::Symbol(s.clone()),
                            })
                            .collect(),
                    ),
                ),
            ),
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

    pub fn param(mut self, db: &dyn IrDatabase, ty: Ty) -> Self {
        let flags = match ty.pass_indirectly(db) {
            | true => Flags::IN,
            | false => Flags::EMPTY,
        };

        self.params.push(SigParam { ty, flags });
        self
    }

    pub fn ret(mut self, db: &dyn IrDatabase, ty: Ty) -> Self {
        let flags = match ty.pass_indirectly(db) {
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

    pub fn finish(self, db: &dyn IrDatabase, ty: Ty) -> Ty {
        Ty::new(db, typ::Generic(self.params, ty))
    }
}

#[macro_export]
macro_rules! sig {
    ($db:ident; $($param:expr),* => $($ret:expr),*) => {
        $crate::ty::Ty::new($db, $crate::ty::typ::Func($crate::ty::Signature {
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
    ($db:ident; $($p:ident : $kind:ident),*in $t:expr) => {{
        let mut generic = $crate::ty::Ty::generic();
        $(
            let $p = generic.add_param($crate::ty::GenericParam::$kind);
            let $p = $crate::ty::Ty::new($db, $crate::ty::typ::Var($p));
        )*

        generic.finish($db, $t)
    }};
}
