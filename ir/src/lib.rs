#![feature(once_cell)]

pub mod builder;
pub mod db;
pub mod display;
mod intrinsics;
pub mod layout;
pub mod parser;
pub mod ty;

use arena::{Arena, Idx};
use std::ops::{Index, IndexMut};
use std::sync::Arc;
use ty::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub name: String,
    pub types: Vec<LocalTypeDef>,
    pub funcs: Arena<Func>,
    bodies: Arena<Body>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeDefId(salsa::InternId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FuncId(pub Idx<Func>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BodyId(pub Idx<Body>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Var(pub Idx<VarInfo>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Block(pub Idx<BlockData>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocalTypeDef {
    pub id: TypeDefId,
    pub linkage: Linkage,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeDef {
    pub name: String,
    pub generic_params: Vec<GenericParam>,
    pub body: Option<TypeDefBody>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeDefBody {
    Struct { fields: Vec<TypeDefField> },
    Union { fields: Vec<TypeDefField> },
    Enum { variants: Vec<TypeDefVariant> },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeDefField {
    pub name: String,
    pub ty: Ty,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeDefVariant {
    pub name: String,
    pub payload: Option<Ty>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Func {
    pub linkage: Linkage,
    pub name: String,
    pub sig: Ty,
    pub body: Option<BodyId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Linkage {
    Import,
    Export,
    Local,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Body {
    pub generic_params: Vec<GenericParam>,
    pub vars: Arena<VarInfo>,
    pub blocks: Arena<BlockData>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarInfo {
    pub ty: Ty,
    pub flags: Flags,
}

impl Flags {
    pub const INDIRECT: Self = Self(1 << 0);
    pub const RETURN: Self = Self(1 << 1);
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockData {
    pub params: Vec<Var>,
    pub instrs: Vec<Instr>,
    pub term: Option<Term>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instr {
    // Stack allocation
    StackAlloc {
        ret: Var,
        ty: Ty,
    },
    StackFree {
        addr: Var,
    },

    // Heap allocation
    BoxAlloc {
        ret: Var,
        ty: Ty,
    },
    BoxFree {
        boxed: Var,
    },
    BoxAddr {
        ret: Var,
        boxed: Var,
    },

    // Memory operations
    Load {
        ret: Var,
        addr: Var,
    },
    Store {
        val: Var,
        addr: Var,
    },
    CopyAddr {
        old: Var,
        new: Var,
        flags: Flags,
    },
    CopyValue {
        ret: Var,
        val: Var,
    },
    DropAddr {
        addr: Var,
    },
    DropValue {
        val: Var,
    },

    // Constants
    ConstInt {
        ret: Var,
        val: u128,
    },
    ConstStr {
        ret: Var,
        val: String,
    },
    FuncRef {
        ret: Var,
        func: FuncId,
    },

    // Tuples
    Tuple {
        ret: Var,
        vals: Vec<Var>,
    },
    TupleExtract {
        ret: Var,
        tuple: Var,
        field: usize,
    },
    TupleInsert {
        tuple: Var,
        field: usize,
        val: Var,
    },
    TupleAddr {
        ret: Var,
        tuple: Var,
        field: usize,
    },

    // Structs
    Struct {
        ret: Var,
        ty: Ty,
        fields: Vec<(String, Var)>,
    },
    StructExtract {
        ret: Var,
        struc: Var,
        field: String,
    },
    StructInsert {
        struc: Var,
        field: String,
        val: Var,
    },
    StructAddr {
        ret: Var,
        struc: Var,
        field: String,
    },

    // Apply
    Apply {
        rets: Vec<Var>,
        func: Var,
        args: Vec<Var>,
        subst: Vec<Subst>,
    },
    Intrinsic {
        rets: Vec<Var>,
        name: String,
        args: Vec<Var>,
        subst: Vec<Subst>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Flags(u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    Unreachable,
    Return { vals: Vec<Var> },
    Br { to: BrTarget },
    Switch { pred: Var, cases: Vec<SwitchCase>, default: BrTarget },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SwitchCase {
    pub val: u128,
    pub to: BrTarget,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BrTarget {
    pub block: Block,
    pub args: Vec<Var>,
}

impl Module {
    pub fn func(&self, name: &str) -> Option<BodyId> {
        self.funcs.iter().find(|(_, f)| f.name == name).and_then(|(_, f)| f.body)
    }
}

impl TypeDef {
    pub fn is_trivial(&self, db: &dyn db::IrDatabase) -> bool {
        match &self.body {
            None => false,
            Some(b) => b.is_trivial(db),
        }
    }
}

impl TypeDefBody {
    pub fn is_trivial(&self, db: &dyn db::IrDatabase) -> bool {
        match self {
            TypeDefBody::Struct { fields } => fields.iter().all(|f| {
                f.ty.lookup(db).flags.is_set(Flags::TRIVIAL)
            }),
            TypeDefBody::Union { fields } => fields.iter().all(|f| {
                f.ty.lookup(db).flags.is_set(Flags::TRIVIAL)
            }),
            TypeDefBody::Enum { variants } => variants.iter().all(|v| {
                match v.payload {
                    None => true,
                    Some(p) => p.lookup(db).flags.is_set(Flags::TRIVIAL),
                }
            }),
        }
    }
}

impl Block {
    pub const ENTRY: Self = Self(Idx::DUMMY);
}

impl Body {
    pub fn var_type(&self, var: Var) -> Ty {
        self[var].ty
    }
}

impl Flags {
    pub const EMPTY: Self = Self(0);

    // CopyAddr
    pub const TAKE: Self = Self(1 << 0);
    pub const INIT: Self = Self(1 << 1);

    pub fn set(self, flag: Self) -> Self {
        Self(self.0 | flag.0)
    }

    pub fn is_set(self, flag: Self) -> bool {
        self.0 & flag.0 != 0
    }
}

impl TypeDef {
    pub fn intern(self, db: &dyn db::IrDatabase) -> TypeDefId {
        db.intern_typedef(Arc::new(self))
    }
}

impl TypeDefId {
    pub fn lookup(self, db: &dyn db::IrDatabase) -> Arc<TypeDef> {
        db.lookup_intern_typedef(self)
    }
}

impl salsa::InternKey for TypeDefId {
    fn from_intern_id(v: salsa::InternId) -> Self {
        Self(v)
    }

    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
}

impl Index<FuncId> for Module {
    type Output = Func;

    fn index(&self, id: FuncId) -> &Self::Output {
        &self.funcs[id.0]
    }
}

impl IndexMut<FuncId> for Module {
    fn index_mut(&mut self, id: FuncId) -> &mut Self::Output {
        &mut self.funcs[id.0]
    }
}

impl Index<BodyId> for Module {
    type Output = Body;

    fn index(&self, id: BodyId) -> &Self::Output {
        &self.bodies[id.0]
    }
}

impl IndexMut<BodyId> for Module {
    fn index_mut(&mut self, id: BodyId) -> &mut Self::Output {
        &mut self.bodies[id.0]
    }
}

impl Index<Var> for Body {
    type Output = VarInfo;

    fn index(&self, id: Var) -> &Self::Output {
        &self.vars[id.0]
    }
}

impl IndexMut<Var> for Body {
    fn index_mut(&mut self, id: Var) -> &mut Self::Output {
        &mut self.vars[id.0]
    }
}

impl Index<Block> for Body {
    type Output = BlockData;

    fn index(&self, id: Block) -> &Self::Output {
        &self.blocks[id.0]
    }
}

impl IndexMut<Block> for Body {
    fn index_mut(&mut self, id: Block) -> &mut Self::Output {
        &mut self.blocks[id.0]
    }
}
