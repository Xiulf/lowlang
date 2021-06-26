use std::convert::TryInto;
use std::num::NonZeroUsize;
use std::ops::{self, Range, RangeInclusive};
use target_lexicon::{PointerWidth, Triple};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Layout {
    pub size: Size,
    pub align: Align,
    pub stride: Size,
    pub abi: Abi,
    pub fields: Fields,
    pub variants: Variants,
    pub largest_niche: Option<Niche>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Size {
    bytes: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Align {
    pow2: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Abi {
    Uninhabited,
    Scalar(Scalar),
    ScalarPair(Scalar, Scalar),
    Vector { elem: Scalar, count: u64 },
    Aggregate { sized: bool },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Scalar {
    pub value: Primitive,
    pub valid_range: RangeInclusive<u128>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Primitive {
    Int(Integer, bool),
    F32,
    F64,
    Pointer,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Integer {
    I8,
    I16,
    I32,
    I64,
    I128,
    ISize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Fields {
    Primitive,
    Union(NonZeroUsize),
    Array { stride: Size, count: u64 },
    Arbitrary { offsets: Vec<Size>, memory_index: Vec<usize> },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Variants {
    Single {
        index: usize,
    },
    Multiple {
        tag: Scalar,
        tag_encoding: TagEncoding,
        tag_field: usize,
        variants: Vec<Layout>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Niche {
    pub offset: Size,
    pub scalar: Scalar,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TagEncoding {
    Direct,
    Niche {
        dataful_variant: usize,
        niche_variants: RangeInclusive<usize>,
        niche_start: u128,
    },
}

impl Size {
    pub const ZERO: Self = Self { bytes: 0 };

    pub fn from_bytes(bytes: u64) -> Self {
        Self { bytes }
    }

    pub fn from_bits(bits: impl TryInto<u64>) -> Self {
        let bits = bits.try_into().ok().unwrap();

        Self::from_bytes(bits / 8 + ((bits % 8) + 7) / 8)
    }

    pub fn bytes(self) -> u64 {
        self.bytes
    }

    pub fn bits(self) -> u64 {
        self.bytes() * 8
    }

    pub fn align_to(self, align: Align) -> Self {
        let mask = align.bytes() - 1;

        Self::from_bytes((self.bytes() + mask) & !mask)
    }

    pub fn is_aligned(self, align: Align) -> bool {
        let mask = align.bytes() - 1;

        self.bytes() & mask == 0
    }
}

impl ops::Add for Size {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self { bytes: self.bytes + rhs.bytes }
    }
}

impl ops::Mul<u64> for Size {
    type Output = Self;

    fn mul(self, rhs: u64) -> Self::Output {
        Self { bytes: self.bytes * rhs }
    }
}

impl Align {
    pub const ONE: Self = Self { pow2: 0 };

    pub fn from_bits(bits: impl TryInto<u64>) -> Self {
        Self::from_bytes(Size::from_bits(bits).bytes())
    }

    pub fn from_bytes(mut bytes: u64) -> Self {
        if bytes == 0 {
            return Self::ONE;
        }

        let mut pow2 = 0u8;

        while (bytes & 1) == 0 {
            pow2 += 1;
            bytes >>= 1;
        }

        Self { pow2 }
    }

    pub fn bytes(self) -> u64 {
        1 << self.pow2
    }

    pub fn bits(self) -> u64 {
        self.bytes() * 8
    }

    pub fn max_for_offset(offset: Size) -> Self {
        Self {
            pow2: offset.bytes().trailing_zeros() as u8,
        }
    }

    pub fn restrict_for_offset(self, offset: Size) -> Self {
        self.min(Self::max_for_offset(offset))
    }
}

impl Abi {
    pub fn is_unsized(&self) -> bool {
        match *self {
            | Abi::Uninhabited | Abi::Scalar(_) | Abi::ScalarPair(_, _) | Abi::Vector { .. } => false,
            | Abi::Aggregate { sized } => !sized,
        }
    }

    pub fn is_signed(&self) -> bool {
        match *self {
            | Abi::Scalar(ref s) => match s.value {
                | Primitive::Int(_, signed) => signed,
                | _ => false,
            },
            | _ => panic!("called Abi::is_signed on a non-scalar ABI"),
        }
    }

    pub fn is_uninhabited(&self) -> bool {
        matches!(self, Abi::Uninhabited)
    }

    pub fn is_scalar(&self) -> bool {
        matches!(self, Abi::Scalar(_))
    }
}

impl Scalar {
    pub fn is_bool(&self) -> bool {
        matches!(self.value, Primitive::Int(Integer::I8, false)) && self.valid_range == (0..=1)
    }

    pub fn valid_range_exclusive(&self, triple: &Triple) -> Range<u128> {
        let bits = self.value.size(triple).bits();
        let mask = !0u128 >> (128 - bits);
        let start = *self.valid_range.start();
        let end = *self.valid_range.end();

        start..(end.wrapping_add(1) & mask)
    }
}

impl Primitive {
    pub fn size(self, triple: &Triple) -> Size {
        match self {
            | Primitive::Int(i, _) => i.size(triple),
            | Primitive::F32 => Size::from_bytes(4),
            | Primitive::F64 => Size::from_bytes(8),
            | Primitive::Pointer => match triple.pointer_width() {
                | Ok(PointerWidth::U16) => Size::from_bytes(2),
                | Ok(PointerWidth::U32) => Size::from_bytes(4),
                | Ok(PointerWidth::U64) => Size::from_bytes(8),
                | Err(_) => Size::from_bytes(4),
            },
        }
    }

    pub fn align(self, triple: &Triple) -> Align {
        Align::from_bytes(self.size(triple).bytes())
    }

    pub fn is_int(self) -> bool {
        matches!(self, Primitive::Int(_, _))
    }

    pub fn is_float(self) -> bool {
        matches!(self, Primitive::F32 | Primitive::F64)
    }
}

impl Integer {
    pub fn size(self, triple: &Triple) -> Size {
        match self {
            | Integer::I8 => Size::from_bytes(1),
            | Integer::I16 => Size::from_bytes(2),
            | Integer::I32 => Size::from_bytes(4),
            | Integer::I64 => Size::from_bytes(8),
            | Integer::I128 => Size::from_bytes(16),
            | Integer::ISize => match triple.pointer_width() {
                | Ok(PointerWidth::U16) => Size::from_bytes(2),
                | Ok(PointerWidth::U32) => Size::from_bytes(4),
                | Ok(PointerWidth::U64) => Size::from_bytes(8),
                | Err(_) => Size::from_bytes(4),
            },
        }
    }
}

impl Fields {
    pub fn count(&self) -> usize {
        match *self {
            | Fields::Primitive => 0,
            | Fields::Union(count) => count.get(),
            | Fields::Array { count, .. } => count.try_into().unwrap(),
            | Fields::Arbitrary { ref offsets, .. } => offsets.len(),
        }
    }

    pub fn offset(&self, i: usize) -> Size {
        match *self {
            | Fields::Primitive => unreachable!(),
            | Fields::Union(count) => {
                assert!(i < count.get());
                Size::ZERO
            },
            | Fields::Array { stride, count } => {
                let i: u64 = i.try_into().unwrap();
                assert!(i < count);
                stride * i
            },
            | Fields::Arbitrary { ref offsets, .. } => offsets[i],
        }
    }

    pub fn memory_index(&self, i: usize) -> usize {
        match *self {
            | Fields::Primitive => unreachable!(),
            | Fields::Union(_) | Fields::Array { .. } => i,
            | Fields::Arbitrary { ref memory_index, .. } => memory_index[i].try_into().unwrap(),
        }
    }
}

impl Niche {
    pub fn from_scalar(triple: &Triple, offset: Size, scalar: Scalar) -> Option<Self> {
        let niche = Self { offset, scalar };

        if niche.available(triple) > 0 {
            Some(niche)
        } else {
            None
        }
    }

    pub fn available(&self, triple: &Triple) -> u128 {
        let Scalar { value, valid_range: ref v } = self.scalar;
        let bits = value.size(triple).bits();
        let max_value = !0u128 >> (128 - bits);
        let niche = v.end().wrapping_add(1)..*v.start();

        niche.end.wrapping_sub(niche.start) & max_value
    }

    pub fn reserve(&self, triple: &Triple, count: u128) -> Option<(u128, Scalar)> {
        let Scalar { value, valid_range: ref v } = self.scalar;
        let bits = value.size(triple).bits();
        let max_value = !0u128 >> (128 - bits);

        if count > max_value {
            return None;
        }

        let start = v.end().wrapping_add(1) & max_value;
        let end = v.end().wrapping_add(count) & max_value;

        let valid_range_contains = |x| {
            if v.start() <= v.end() {
                *v.start() <= x && x <= *v.end()
            } else {
                *v.start() <= x || x <= *v.end()
            }
        };

        if valid_range_contains(end) {
            return None;
        }

        Some((start, Scalar {
            value,
            valid_range: *v.start()..=end,
        }))
    }
}
