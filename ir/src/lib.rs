pub mod builder;
pub mod layout;
mod printing;
pub mod visit;

use index_vec::IndexVec;
use std::collections::HashMap;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Module {
    subset: ModuleSubset,
    pub bodies: HashMap<DefId, Body>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct ModuleSubset {
    pub defs: IndexVec<DefId, Decl>,
    pub types: HashMap<DefId, TypeDef>,
}

index_vec::define_index_type! {
    pub struct DefId = u32;
    DEFAULT = DefId::from_raw_unchecked(0);
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Decl {
    pub id: DefId,
    pub name: String,
    pub linkage: Linkage,
    pub kind: DeclKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DeclKind {
    Def(Ty),
    Type,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Linkage {
    Import,
    Export,
    Local,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeDef {
    pub variants: Vec<Variant>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variant {
    pub tys: Vec<Ty>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct Body {
    pub vars: IndexVec<Var, Variable>,
    pub blocks: IndexVec<Block, BasicBlock>,
}

index_vec::define_index_type! {
    pub struct Var = u32;
    DEFAULT = Var::from_raw_unchecked(0);
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variable {
    pub id: Var,
    pub ty: Ty,
}

index_vec::define_index_type! {
    pub struct Block = u32;
    DEFAULT = Block::from_raw_unchecked(0);
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct BasicBlock {
    pub id: Block,
    pub params: Vec<Var>,
    pub instrs: Vec<Instr>,
    pub term: Term,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Instr {
    pub kind: InstrKind,
}

pub mod instr {
    pub use super::InstrKind::*;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InstrKind {
    Const { res: Var, const_: Const },
    Load { res: Var, ptr: Var },
    Store { ptr: Var, val: Operand },
    LoadField { res: Var, val: Var, field: usize },
    StoreField { val: Var, field: usize, new_val: Var },
    Call { rets: Vec<Var>, func: Operand, args: Vec<Operand> },
    Offset { res: Var, ptr: Var, by: Operand },
    Add { res: Var, lhs: Var, rhs: Operand },
    Sub { res: Var, lhs: Var, rhs: Operand },
    Mul { res: Var, lhs: Var, rhs: Operand },
    Div { res: Var, lhs: Var, rhs: Operand },
    Rem { res: Var, lhs: Var, rhs: Operand },
    Shl { res: Var, lhs: Var, rhs: Operand },
    Shr { res: Var, lhs: Var, rhs: Operand },
    And { res: Var, lhs: Var, rhs: Operand },
    Or { res: Var, lhs: Var, rhs: Operand },
    Xor { res: Var, lhs: Var, rhs: Operand },
    Cmp { res: Var, cc: CondCode, lhs: Var, rhs: Operand },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CondCode {
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term {
    Unset,
    Abort,
    Return(Vec<Operand>),
    Br(Block, Vec<Var>),
    BrIf(Operand, Block, Block, Vec<Var>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operand {
    Var(Var),
    Const(Const),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Const {
    Undefined,
    Scalar(u128),
    Addr(DefId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ty {
    pub info: TyInfo,
    pub kind: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyInfo {
    pub valid_range: Option<std::ops::RangeInclusive<u128>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Int(u8, bool),
    Float(u8),
    Ptr(Box<Ty>),
    Box(Box<Ty>),
    Tuple(Vec<Ty>),
    Func(Signature),
    Def(DefId),
    Var(TypeVar),
    Forall(Vec<TypeVar>, Box<Ty>),
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Signature {
    pub params: Vec<Ty>,
    pub rets: Vec<Ty>,
}

impl Block {
    pub const ENTRY: Self = Block::from_raw_unchecked(0);
}

impl Instr {
    pub fn inputs(&self) -> Inputs {
        Inputs { instr: self, idx: 0 }
    }

    pub fn outputs(&self) -> Outputs {
        Outputs { instr: self, idx: 0 }
    }
}

impl Ty {
    pub const fn new(kind: Type) -> Self {
        Ty {
            info: TyInfo { valid_range: None },
            kind,
        }
    }

    pub fn with_valid_range(mut self, valid_range: std::ops::RangeInclusive<u128>) -> Self {
        self.info.valid_range = Some(valid_range);
        self
    }
}

pub struct Inputs<'ir> {
    instr: &'ir Instr,
    idx: usize,
}

pub struct Outputs<'ir> {
    instr: &'ir Instr,
    idx: usize,
}

impl<'ir> Iterator for Inputs<'ir> {
    type Item = Operand;

    fn next(&mut self) -> Option<Self::Item> {
        let op = match self.instr.kind {
            | InstrKind::Const { ref const_, .. } if self.idx == 0 => const_.clone().into(),
            | InstrKind::Load { ptr, .. } if self.idx == 0 => ptr.into(),
            | InstrKind::Store { ptr, .. } if self.idx == 0 => ptr.into(),
            | InstrKind::Store { ref val, .. } if self.idx == 1 => val.clone(),
            | InstrKind::LoadField { val, .. } if self.idx == 0 => val.into(),
            | InstrKind::StoreField { val, .. } if self.idx == 0 => val.into(),
            | InstrKind::StoreField { new_val, .. } if self.idx == 1 => new_val.into(),
            | InstrKind::Call { ref func, .. } if self.idx == 0 => func.clone(),
            | InstrKind::Call { ref args, .. } if self.idx - 1 < args.len() => args[self.idx - 1].clone(),
            | InstrKind::Offset { ptr, .. } if self.idx == 0 => ptr.into(),
            | InstrKind::Offset { ref by, .. } if self.idx == 1 => by.clone(),
            | InstrKind::Add { lhs, .. } if self.idx == 0 => lhs.into(),
            | InstrKind::Add { ref rhs, .. } if self.idx == 1 => rhs.clone(),
            | InstrKind::Sub { lhs, .. } if self.idx == 0 => lhs.into(),
            | InstrKind::Sub { ref rhs, .. } if self.idx == 1 => rhs.clone(),
            | InstrKind::Mul { lhs, .. } if self.idx == 0 => lhs.into(),
            | InstrKind::Mul { ref rhs, .. } if self.idx == 1 => rhs.clone(),
            | InstrKind::Div { lhs, .. } if self.idx == 0 => lhs.into(),
            | InstrKind::Div { ref rhs, .. } if self.idx == 1 => rhs.clone(),
            | InstrKind::Rem { lhs, .. } if self.idx == 0 => lhs.into(),
            | InstrKind::Rem { ref rhs, .. } if self.idx == 1 => rhs.clone(),
            | InstrKind::Shl { lhs, .. } if self.idx == 0 => lhs.into(),
            | InstrKind::Shl { ref rhs, .. } if self.idx == 1 => rhs.clone(),
            | InstrKind::Shr { lhs, .. } if self.idx == 0 => lhs.into(),
            | InstrKind::Shr { ref rhs, .. } if self.idx == 1 => rhs.clone(),
            | InstrKind::And { lhs, .. } if self.idx == 0 => lhs.into(),
            | InstrKind::And { ref rhs, .. } if self.idx == 1 => rhs.clone(),
            | InstrKind::Or { lhs, .. } if self.idx == 0 => lhs.into(),
            | InstrKind::Or { ref rhs, .. } if self.idx == 1 => rhs.clone(),
            | InstrKind::Xor { lhs, .. } if self.idx == 0 => lhs.into(),
            | InstrKind::Xor { ref rhs, .. } if self.idx == 1 => rhs.clone(),
            | InstrKind::Cmp { lhs, .. } if self.idx == 0 => lhs.into(),
            | InstrKind::Cmp { ref rhs, .. } if self.idx == 1 => rhs.clone(),
            | _ => return None,
        };

        self.idx += 1;

        Some(op)
    }
}

impl<'ir> Iterator for Outputs<'ir> {
    type Item = Var;

    fn next(&mut self) -> Option<Self::Item> {
        let var = match self.instr.kind {
            | InstrKind::Const { res, .. } if self.idx == 0 => res,
            | InstrKind::Load { res, .. } if self.idx == 0 => res,
            | InstrKind::LoadField { res, .. } if self.idx == 0 => res,
            | InstrKind::Call { ref rets, .. } if self.idx < rets.len() => rets[self.idx],
            | InstrKind::Offset { res, .. } if self.idx == 0 => res,
            | InstrKind::Add { res, .. } if self.idx == 0 => res,
            | InstrKind::Sub { res, .. } if self.idx == 0 => res,
            | InstrKind::Mul { res, .. } if self.idx == 0 => res,
            | InstrKind::Div { res, .. } if self.idx == 0 => res,
            | InstrKind::Rem { res, .. } if self.idx == 0 => res,
            | InstrKind::Shl { res, .. } if self.idx == 0 => res,
            | InstrKind::Shr { res, .. } if self.idx == 0 => res,
            | InstrKind::And { res, .. } if self.idx == 0 => res,
            | InstrKind::Or { res, .. } if self.idx == 0 => res,
            | InstrKind::Xor { res, .. } if self.idx == 0 => res,
            | InstrKind::Cmp { res, .. } if self.idx == 0 => res,
            | _ => return None,
        };

        self.idx += 1;

        Some(var)
    }
}

impl Default for Term {
    fn default() -> Self {
        Term::Unset
    }
}

impl std::ops::Deref for Module {
    type Target = ModuleSubset;

    fn deref(&self) -> &Self::Target {
        &self.subset
    }
}

impl std::ops::DerefMut for Module {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.subset
    }
}

impl Into<Operand> for Var {
    fn into(self) -> Operand {
        Operand::Var(self)
    }
}

impl Into<Operand> for Const {
    fn into(self) -> Operand {
        Operand::Const(self)
    }
}
