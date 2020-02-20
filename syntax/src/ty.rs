use crate::Signature;
use intern::Intern;

pub struct TyCtx<'t> {
    pub defaults: DefaultTypes<'t>,
    interner: &'t TypeInterner<'t>,
}

pub struct DefaultTypes<'t> {
    pub unit: Ty<'t>,
    pub bool: Ty<'t>,
    pub char: Ty<'t>,
    pub str: Ty<'t>,
    pub ratio: Ty<'t>,
    pub u8: Ty<'t>,
    pub u16: Ty<'t>,
    pub u32: Ty<'t>,
    pub u64: Ty<'t>,
    pub u128: Ty<'t>,
    pub usize: Ty<'t>,
    pub i8: Ty<'t>,
    pub i16: Ty<'t>,
    pub i32: Ty<'t>,
    pub i64: Ty<'t>,
    pub i128: Ty<'t>,
    pub isize: Ty<'t>,
    pub f32: Ty<'t>,
    pub f64: Ty<'t>,
    pub fsize: Ty<'t>,
    pub ptr_u8: Ty<'t>,
}

impl<'t> TyCtx<'t> {
    pub fn new(interner: &'t TypeInterner<'t>) -> TyCtx<'t> {
        let u8 = Type::UInt(IntSize::Bits8).intern(interner);
        
        TyCtx {
            defaults: DefaultTypes {
                unit: Type::Unit.intern(interner),
                bool: Type::Bool.intern(interner),
                char: Type::Char.intern(interner),
                str: Type::Str.intern(interner),
                ratio: Type::Ratio.intern(interner),
                u8,
                u16: Type::UInt(IntSize::Bits16).intern(interner),
                u32: Type::UInt(IntSize::Bits32).intern(interner),
                u64: Type::UInt(IntSize::Bits64).intern(interner),
                u128: Type::UInt(IntSize::Bits128).intern(interner),
                usize: Type::UInt(IntSize::Size).intern(interner),
                i8: Type::Int(IntSize::Bits8).intern(interner),
                i16: Type::Int(IntSize::Bits16).intern(interner),
                i32: Type::Int(IntSize::Bits32).intern(interner),
                i64: Type::Int(IntSize::Bits64).intern(interner),
                i128: Type::Int(IntSize::Bits128).intern(interner),
                isize: Type::Int(IntSize::Size).intern(interner),
                f32: Type::Float(FloatSize::Bits32).intern(interner),
                f64: Type::Float(FloatSize::Bits64).intern(interner),
                fsize: Type::Float(FloatSize::Size).intern(interner),
                ptr_u8: Type::Ref(u8).intern(interner),
            },
            interner,
        }
    }
}

impl<'t> intern::Interner<'t, Type<'t>> for TyCtx<'t> {
    fn intern(&self, value: Type<'t>) -> Ty<'t> {
        self.interner.intern(value)
    }
}

#[derive(Clone)]
pub enum Type<'t> {
    Param(String),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ty<'a>(*mut Type<'a>, ::std::marker::PhantomData<&'a mut Type<'a>>);

impl<'a> ::std::default::Default for Ty<'a> {
    fn default() -> Ty<'a> {
        Ty(::std::ptr::null_mut(), ::std::marker::PhantomData)
    }
}

impl<'a> ::std::ops::Deref for Ty<'a> {
    type Target = Type<'a>;
    
    fn deref(&self) -> &Type<'a> {
        unsafe { &*self.0 }
    }
}

impl<'a> ::std::ops::DerefMut for Ty<'a> {
    fn deref_mut(&mut self) -> &mut Type<'a> {
        unsafe { &mut *self.0 }
    }
}

pub struct TypeInterner<'a> {
    storage: intern::typed_arena::Arena<Type<'a>>,
}

impl<'a> TypeInterner<'a> {
    pub fn new() -> TypeInterner<'a> {
        TypeInterner {
            storage: intern::typed_arena::Arena::new(),
        }
    }
}

impl<'a> intern::Intern<'a> for Type<'a> {
    type Key = Ty<'a>;
    
    fn intern<I: intern::Interner<'a, Self> + 'a>(self, i: &I) -> Ty<'a> {
        i.intern(self)
    }
}

impl<'a> intern::Interner<'a, Type<'a>> for TypeInterner<'a> {
    fn intern(&self, value: Type<'a>) -> Ty<'a> {
        Ty(self.storage.alloc(value), ::std::marker::PhantomData)
    }
}
// intern::interner!(TypeInterner, Type<'t>, Ty<'t>);
