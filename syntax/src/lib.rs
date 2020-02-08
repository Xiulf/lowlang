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
pub struct Package {
    pub name: String,
    pub externs: BTreeMap<ItemId, Extern>,
    pub globals: BTreeMap<ItemId, Global>,
    pub bodies: BTreeMap<ItemId, Body>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemId(usize);

#[derive(Default, Clone)]
pub struct Signature(pub CallConv, pub Vec<Ty>, pub Vec<Ty>);

pub enum Extern {
    Proc(String, Signature),
    Global(String, Ty),
}

pub struct Global {
    pub attributes: Attributes,
    pub export: bool,
    pub name: String,
    pub ty: Ty,
    pub init: Option<Box<[u8]>>,
}

pub struct Body {
    pub attributes: Attributes,
    pub export: bool,
    pub name: String,
    pub conv: CallConv,
    pub locals: BTreeMap<LocalId, Local>,
    pub blocks: BTreeMap<BlockId, Block>,
}

#[derive(Default)]
pub struct Attributes {
    lang: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalId(pub usize);

pub struct Local {
    pub id: LocalId,
    pub kind: LocalKind,
    pub ty: Ty,
}

#[derive(PartialEq)]
pub enum LocalKind {
    Ret,
    Arg,
    Var,
    Tmp,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(usize);

pub struct Block {
    pub id: BlockId,
    pub stmts: Vec<Stmt>,
    pub term: Terminator,
}

pub enum Stmt {
    Assign(Place, Value),
}

pub enum Terminator {
    Unset,
    Return,
    Jump(BlockId),
    Call(Vec<Place>, Operand, Vec<Operand>, BlockId),
    Switch(Operand, Vec<u128>, Vec<BlockId>),
}

pub struct Place {
    pub base: PlaceBase,
    pub elems: Vec<PlaceElem>,
}

pub enum PlaceBase {
    Local(LocalId),
    Global(ItemId),
}

pub enum PlaceElem {
    Deref,
    Field(usize),
    ConstIndex(usize),
    Index(Place),
}

pub enum Operand {
    Place(Place),
    Constant(Const),
}

pub enum Const {
    Unit,
    Scalar(u128, Ty),
    FuncAddr(ItemId),
    Bytes(Box<[u8]>),
}

pub enum Value {
    Use(Operand),
    Ref(Place),
    Slice(Place, Operand, Operand),
    Cast(Ty, Operand),
    BinOp(BinOp, Operand, Operand),
    UnOp(UnOp, Operand),
    NullOp(NullOp, Ty),
    Init(Ty, Vec<Operand>),
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
