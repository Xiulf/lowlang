use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Val {
    Int(i128),
    Float(f64),
    Str(String),
    Box(*mut u8, usize),
    StackPtr(usize),
    Func(Func),
    Tuple(Vec<Val>),
}

#[derive(Debug, Clone)]
pub enum Func {
    Local(ir::BodyId),
    Foreign(Box<dyn ForeignFn>),
}

pub trait ForeignFn: private::CopyForeignFn {
    fn call(&self, args: Vec<Val>) -> Val;
}

impl Val {
    pub fn int(&self) -> i128 {
        match *self {
            | Self::Int(v) => v,
            | _ => unreachable!(),
        }
    }

    pub fn float(&self) -> f64 {
        match *self {
            | Self::Float(v) => v,
            | _ => unreachable!(),
        }
    }

    pub fn str(&self) -> &String {
        match self {
            | Self::Str(v) => v,
            | _ => unreachable!(),
        }
    }

    pub fn stack_ptr(&self) -> usize {
        match *self {
            | Self::StackPtr(v) => v,
            | _ => unreachable!(),
        }
    }

    pub fn box_(&self) -> (*mut u8, usize) {
        match *self {
            | Self::Box(p, g) => (p, g),
            | _ => unreachable!(),
        }
    }

    pub fn func(&self) -> &Func {
        match self {
            | Self::Func(v) => v,
            | _ => unreachable!(),
        }
    }

    pub fn tuple(&self) -> &[Val] {
        match self {
            | Self::Tuple(v) => v,
            | _ => unreachable!(),
        }
    }
}

impl fmt::Debug for dyn ForeignFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[foreign]")
    }
}

impl Clone for Box<dyn ForeignFn> {
    fn clone(&self) -> Self {
        self.copy_foreign_fn()
    }
}

impl PartialEq for Func {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            | (&Self::Local(a), &Self::Local(b)) => a == b,
            | (Self::Foreign(ref a), Self::Foreign(ref b)) => {
                let a = std::ptr::metadata(&**a);
                let b = std::ptr::metadata(&**b);

                a == b
            },
            | _ => false,
        }
    }
}

mod private {
    pub trait CopyForeignFn {
        fn copy_foreign_fn(&self) -> Box<dyn super::ForeignFn>;
    }

    impl<T> CopyForeignFn for T
    where
        T: 'static + super::ForeignFn + Copy,
    {
        fn copy_foreign_fn(&self) -> Box<dyn super::ForeignFn> {
            Box::new(*self)
        }
    }
}

impl From<()> for Val {
    fn from(_: ()) -> Self {
        Self::Tuple(Vec::new())
    }
}

impl From<u8> for Val {
    fn from(v: u8) -> Self {
        Self::Int(v as i128)
    }
}

impl From<u16> for Val {
    fn from(v: u16) -> Self {
        Self::Int(v as i128)
    }
}

impl From<u32> for Val {
    fn from(v: u32) -> Self {
        Self::Int(v as i128)
    }
}

impl From<u64> for Val {
    fn from(v: u64) -> Self {
        Self::Int(v as i128)
    }
}

impl From<u128> for Val {
    fn from(v: u128) -> Self {
        Self::Int(v as i128)
    }
}

impl From<i8> for Val {
    fn from(v: i8) -> Self {
        Self::Int(v as i128)
    }
}

impl From<i16> for Val {
    fn from(v: i16) -> Self {
        Self::Int(v as i128)
    }
}

impl From<i32> for Val {
    fn from(v: i32) -> Self {
        Self::Int(v as i128)
    }
}

impl From<i64> for Val {
    fn from(v: i64) -> Self {
        Self::Int(v as i128)
    }
}

impl From<i128> for Val {
    fn from(v: i128) -> Self {
        Self::Int(v)
    }
}

impl From<bool> for Val {
    fn from(v: bool) -> Self {
        Self::Int(v as i128)
    }
}

impl From<f32> for Val {
    fn from(v: f32) -> Self {
        Self::Float(v as f64)
    }
}

impl From<f64> for Val {
    fn from(v: f64) -> Self {
        Self::Float(v)
    }
}

impl From<String> for Val {
    fn from(v: String) -> Self {
        Self::Str(v)
    }
}

impl Into<()> for Val {
    fn into(self) -> () {
        let tuple = self.tuple();
        assert!(tuple.is_empty());
        ()
    }
}

impl Into<u8> for Val {
    fn into(self) -> u8 {
        self.int() as u8
    }
}

impl Into<u16> for Val {
    fn into(self) -> u16 {
        self.int() as u16
    }
}

impl Into<u32> for Val {
    fn into(self) -> u32 {
        self.int() as u32
    }
}

impl Into<u64> for Val {
    fn into(self) -> u64 {
        self.int() as u64
    }
}

impl Into<u128> for Val {
    fn into(self) -> u128 {
        self.int() as u128
    }
}

impl Into<i8> for Val {
    fn into(self) -> i8 {
        self.int() as i8
    }
}

impl Into<i16> for Val {
    fn into(self) -> i16 {
        self.int() as i16
    }
}

impl Into<i32> for Val {
    fn into(self) -> i32 {
        self.int() as i32
    }
}

impl Into<i64> for Val {
    fn into(self) -> i64 {
        self.int() as i64
    }
}

impl Into<i128> for Val {
    fn into(self) -> i128 {
        self.int()
    }
}

impl Into<f32> for Val {
    fn into(self) -> f32 {
        self.float() as f32
    }
}

impl Into<f64> for Val {
    fn into(self) -> f64 {
        self.float()
    }
}

impl Into<bool> for Val {
    fn into(self) -> bool {
        self.int() != 0
    }
}

impl Into<String> for Val {
    fn into(self) -> String {
        self.str().clone()
    }
}

macro_rules! impl_foreign_fn {
    () => {
        impl_foreign_fn!(@instance);
    };

    ($t:ident $(, $rest:ident)*) => {
        impl_foreign_fn!($($rest),*);
        impl_foreign_fn!(@instance $t $(, $rest)*);
    };

    (@instance $($t:ident),*) => {
        impl<RET $(, $t)*> ForeignFn for fn($($t),*) -> RET
        where
            RET: Into<Val> + 'static,
            $($t: From<Val> + 'static,)*
        {
            #[allow(non_snake_case, unused_variables, unused_mut)]
            fn call(&self, mut args: Vec<Val>) -> Val {
                $(let $t = args.remove(0).into();)*

                (self)($($t),*).into()
            }
        }
    };
}

impl_foreign_fn!(A);
