use crate::Signature;

#[derive(Clone)]
pub enum Type<'t> {
    Unit,
    Bool,
    Char,
    Str,
    Ratio,
    Int(IntSize),
    UInt(IntSize),
    Float(FloatSize),
    Ref(Ty<'t>),
    Array(Ty<'t>, usize),
    Slice(Ty<'t>),
    Vector(Ty<'t>, usize),
    Proc(Signature<'t>),
    Tuple(bool, Vec<Ty<'t>>),
    Union(bool, Vec<Ty<'t>>),
    Tagged(usize, Ty<'t>),
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

intern::interner!(TypeInterner, Type<'t>, Ty<'t>);
