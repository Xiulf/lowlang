use crate::Signature;

#[derive(Clone, Copy)]
pub struct Ty(usize);

#[derive(Clone)]
pub enum Type {
    Unit,
    Bool,
    Char,
    Str,
    Ratio,
    Int(IntSize),
    UInt(IntSize),
    Float(FloatSize),
    Ref(Ty),
    Array(Ty, usize),
    Slice(Ty),
    Vector(Ty, usize),
    Proc(Signature),
    Tuple(bool, Vec<Ty>),
    Union(bool, Vec<Ty>),
    Tagged(usize, Ty),
}

#[derive(Clone, Copy)]
pub enum IntSize {
    Bits8,
    Bits16,
    Bits32,
    Bits64,
    Bits128,
    Size,
}

#[derive(Clone, Copy)]
pub enum FloatSize {
    Bits32,
    Bits64,
    Size,
}

impl intern::InternKey for Ty {
    fn from_usize(src: usize) -> Ty {
        Ty(src)
    }

    fn into_usize(self) -> usize {
        self.0
    }
}

intern::interner!(TypeInterner, TYPE_INTERNER, Type, Ty);
