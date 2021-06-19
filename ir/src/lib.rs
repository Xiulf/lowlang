#![feature(once_cell)]

pub mod builder;
pub mod display;
mod intrinsics;
pub mod layout;
mod ty;

use arena::{Arena, Idx};
use std::ops::{Index, IndexMut};
pub use ty::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub name: String,
    funcs: Arena<Func>,
    bodies: Arena<Body>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FuncId(Idx<Func>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BodyId(Idx<Body>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Var(Idx<VarInfo>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Block(Idx<BlockData>);

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

    // Constants
    ConstInt {
        ret: Var,
        val: u128,
    },
    FuncRef {
        ret: Var,
        func: FuncId,
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
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Flags(u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    Unreachable,
    Return { ops: Vec<Var> },
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

impl Block {
    pub const ENTRY: Self = Self(Idx::DUMMY);
}

impl Body {
    pub fn var_type(&self, var: Var) -> Ty {
        self[var].ty.clone()
    }
}

impl Flags {
    pub const EMPTY: Self = Self(0);

    // CopyAddr
    pub const TAKE: Self = Self(1 << 0);
    pub const INIT: Self = Self(1 << 1);

    pub fn set(mut self, flag: Self) -> Self {
        Self(self.0 | flag.0)
    }

    pub fn is_set(self, flag: Self) -> bool {
        self.0 & flag.0 != 0
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
