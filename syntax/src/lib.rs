pub mod builder;
mod parsing;
mod printing;
mod util;

pub use parser::parse;
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

pub struct Signature(CallConv, Vec<Type>, Vec<Type>);

pub enum Extern {
    Proc(String, Signature),
    Global(String, Type),
}

pub struct Global {
    pub export: bool,
    pub name: String,
    pub ty: Type,
    pub init: Option<Box<[u8]>>,
}

pub struct Body {
    pub export: bool,
    pub name: String,
    pub conv: CallConv,
    pub locals: BTreeMap<LocalId, Local>,
    pub blocks: BTreeMap<BlockId, Block>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalId(usize);

pub struct Local {
    pub id: LocalId,
    pub kind: LocalKind,
    pub ty: Type,
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
    Scalar(u128),
    FuncAddr(ItemId),
    Bytes(Box<[u8]>),
}

pub enum Value {
    Use(Operand),
    Ref(Place),
    Slice(Operand, Operand, Operand),
    BinOp(BinOp, Operand, Operand),
    UnOp(UnOp, Operand),
    NullOp(NullOp, Type),
    Init(Type, Vec<Operand>),
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

pub enum Type {
    Unit,
    Bool,
    Char,
    Str,
    Ratio,
    Int(IntSize),
    UInt(IntSize),
    Float(FloatSize),
    Ref(Box<Type>),
    Array(Box<Type>, usize),
    Slice(Box<Type>),
    Vector(Box<Type>, usize),
    Proc(Signature),
}

pub enum IntSize {
    Bits8,
    Bits16,
    Bits32,
    Bits64,
    Bits128,
    Size,
}

pub enum FloatSize {
    Bits32,
    Bits64,
    Size,
}

pub enum CallConv {
    C,
    Fluix,
}
