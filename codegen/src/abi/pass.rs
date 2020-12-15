use crate::*;
use ir::layout::{Abi, Size, TyLayout};

#[derive(Clone, Copy)]
pub enum PassMode<'ctx, B: Backend<'ctx>> {
    NoPass,
    ByVal(<B::Type as Type<'ctx>>::Raw),
    ByValPair(<B::Type as Type<'ctx>>::Raw, <B::Type as Type<'ctx>>::Raw),
    ByRef { size: Option<Size> },
}

#[derive(Clone, Copy)]
pub enum EmptySinglePair<T> {
    Empty,
    Single(T),
    Pair(T, T),
}

impl<'ctx, B: Backend<'ctx>> std::fmt::Debug for PassMode<'ctx, B>
where
    <B::Type as Type<'ctx>>::Raw: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            PassMode::NoPass => write!(f, "NoPass"),
            PassMode::ByVal(ty) => write!(f, "ByVal({:?})", ty),
            PassMode::ByValPair(a, b) => write!(f, "ByValPair({:?}, {:?})", a, b),
            PassMode::ByRef { size } => write!(f, "ByRef {{ size: {:?} }}", size),
        }
    }
}

impl<T> EmptySinglePair<T> {
    pub fn map<U>(self, mut f: impl FnMut(T) -> U) -> EmptySinglePair<U> {
        match self {
            EmptySinglePair::Empty => EmptySinglePair::Empty,
            EmptySinglePair::Single(v) => EmptySinglePair::Single(f(v)),
            EmptySinglePair::Pair(a, b) => EmptySinglePair::Pair(f(a), f(b)),
        }
    }
}

impl<T: std::fmt::Debug> EmptySinglePair<T> {
    pub fn assert_single(self) -> T {
        match self {
            EmptySinglePair::Single(v) => v,
            _ => unreachable!(),
        }
    }

    pub fn assert_pair(self) -> (T, T) {
        match self {
            EmptySinglePair::Pair(a, b) => (a, b),
            _ => unreachable!(),
        }
    }
}

impl<T> Iterator for EmptySinglePair<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match std::mem::replace(self, EmptySinglePair::Empty) {
            EmptySinglePair::Empty => None,
            EmptySinglePair::Single(v) => Some(v),
            EmptySinglePair::Pair(a, b) => {
                *self = EmptySinglePair::Single(b);
                Some(a)
            }
        }
    }
}

pub fn get_pass_mode<'ctx, B: Backend<'ctx>>(
    mcx: &ModuleCtx<'_, 'ctx, B>,
    layout: &TyLayout,
) -> PassMode<'ctx, B> {
    if layout.is_zst() {
        PassMode::NoPass
    } else {
        match &layout.abi {
            Abi::Uninhabited => PassMode::NoPass,
            Abi::Scalar(scalar) => PassMode::ByVal(mcx.scalar_ty(scalar)),
            Abi::ScalarPair(a, b) => {
                let a_ty = mcx.scalar_ty(a);
                let b_ty = mcx.scalar_ty(b);

                if a.value.size(&mcx.target).bits() == 128
                    && b.value.size(&mcx.target).bits() == 128
                {
                    PassMode::ByRef {
                        size: Some(layout.size),
                    }
                } else {
                    PassMode::ByValPair(a_ty, b_ty)
                }
            }
            Abi::Aggregate { sized: true } => PassMode::ByRef {
                size: Some(layout.size),
            },
            Abi::Aggregate { sized: false } => PassMode::ByRef { size: None },
        }
    }
}

#[macro_export]
macro_rules! value_for_arg {
    ($fx:ident, $arg:ident, $by_ref:expr) => {
        match $crate::abi::get_pass_mode($fx, $arg.layout()) {
            $crate::abi::PassMode::NoPass => $crate::abi::EmptySinglePair::Empty,
            $crate::abi::PassMode::ByVal(_) => {
                $crate::abi::EmptySinglePair::Single($arg.load_scalar($fx))
            }
            $crate::abi::PassMode::ByValPair(_, _) => {
                let (a, b) = $arg.load_scalar_pair($fx);

                $crate::abi::EmptySinglePair::Pair(a, b)
            }
            $crate::abi::PassMode::ByRef { size: _ } => $by_ref,
        }
    };
}

pub fn value_for_ret<'ctx, B: Backend<'ctx>>(
    fx: &mut FunctionCtx<'_, 'ctx, '_, B>,
    ret: ir::Local,
) -> EmptySinglePair<<B::Value as Value<'ctx>>::Raw> {
    let place = fx.locals[&ret].clone();

    match get_pass_mode(fx.mcx, place.layout()) {
        PassMode::NoPass => EmptySinglePair::Empty,
        PassMode::ByVal(_) => EmptySinglePair::Single(place.to_value(fx).load_scalar(fx)),
        PassMode::ByValPair(_, _) => {
            let (a, b) = place.to_value(fx).load_scalar_pair(fx);

            EmptySinglePair::Pair(a, b)
        }
        PassMode::ByRef { size: _ } => EmptySinglePair::Empty,
    }
}
