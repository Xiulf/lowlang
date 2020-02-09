pub mod builder;
pub mod ty;
pub mod layout;
mod parsing;
mod printing;
mod util;

pub use parser::parse;
pub use ty::*;
use std::collections::BTreeMap;

#[derive(Default)]
pub struct Package<'t> {
    pub name: String,
    pub externs: BTreeMap<ItemId, Extern<'t>>,
    pub globals: BTreeMap<ItemId, Global<'t>>,
    pub bodies: BTreeMap<ItemId, Body<'t>>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemId(usize);

#[derive(Default, Clone)]
pub struct Signature<'t>(pub CallConv, pub Vec<Ty<'t>>, pub Vec<Ty<'t>>);

pub enum Extern<'t> {
    Proc(String, Signature<'t>),
    Global(String, Ty<'t>),
}

pub struct Global<'t> {
    pub attributes: Attributes,
    pub export: bool,
    pub name: String,
    pub ty: Ty<'t>,
    pub init: Option<Box<[u8]>>,
}

pub struct Body<'t> {
    pub attributes: Attributes,
    pub export: bool,
    pub name: String,
    pub conv: CallConv,
    pub locals: BTreeMap<LocalId, Local<'t>>,
    pub blocks: BTreeMap<BlockId, Block<'t>>,
}

#[derive(Default)]
pub struct Attributes {
    lang: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalId(pub usize);

#[derive(Clone, Copy)]
pub struct Local<'t> {
    pub id: LocalId,
    pub kind: LocalKind,
    pub ty: Ty<'t>,
}

#[derive(Clone, Copy, PartialEq)]
pub enum LocalKind {
    Ret,
    Arg,
    Var,
    Tmp,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(usize);

pub struct Block<'t> {
    pub id: BlockId,
    pub stmts: Vec<Stmt<'t>>,
    pub term: Terminator<'t>,
}

pub enum Stmt<'t> {
    Assign(Place, Value<'t>),
}

pub enum Terminator<'t> {
    Unset,
    Return,
    Jump(BlockId),
    Call(Vec<Place>, Operand<'t>, Vec<Operand<'t>>, BlockId),
    Switch(Operand<'t>, Vec<u128>, Vec<BlockId>),
}

#[derive(Clone)]
pub struct Place {
    pub base: PlaceBase,
    pub elems: Vec<PlaceElem>,
}

#[derive(Clone)]
pub enum PlaceBase {
    Local(LocalId),
    Global(ItemId),
}

#[derive(Clone)]
pub enum PlaceElem {
    Deref,
    Field(usize),
    ConstIndex(usize),
    Index(Place),
}

#[derive(Clone)]
pub enum Operand<'t> {
    Place(Place),
    Constant(Const<'t>),
}

#[derive(Clone)]
pub enum Const<'t> {
    Unit,
    Scalar(u128, Ty<'t>),
    FuncAddr(ItemId),
    Bytes(Box<[u8]>),
}

pub enum Value<'t> {
    Use(Operand<'t>),
    Ref(Place),
    Slice(Place, Operand<'t>, Operand<'t>),
    Cast(Ty<'t>, Operand<'t>),
    BinOp(BinOp, Operand<'t>, Operand<'t>),
    UnOp(UnOp, Operand<'t>),
    NullOp(NullOp, Ty<'t>),
    Init(Ty<'t>, Vec<Operand<'t>>),
}

pub enum BinOp {
    Add, Sub, Mul, Div, Rem,
    Eq, Ne, Lt, Le, Gt, Ge,
    BitAnd, BitOr, BitXOr, Shl, Shr,
}

pub enum UnOp {
    Not,
    Neg,
}

pub enum NullOp {
    SizeOf,
    AlignOf,
}

#[derive(Clone, Copy)]
pub enum CallConv {
    C,
    Fluix,
}

impl Default for CallConv {
    fn default() -> CallConv {
        CallConv::Fluix
    }
}
