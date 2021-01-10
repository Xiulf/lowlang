use crate::{Ty, Type};
use std::convert::{TryFrom, TryInto};
use std::ops::{Add, Mul, RangeInclusive};
use target_lexicon::{PointerWidth, Triple};

pub fn layout_of(ty: &Ty, target: &Triple) -> TyLayout {
    let scalar_unit = |value: Primitive| {
        let bits = value.size(target).bits();

        Scalar {
            value,
            valid_range: 0..=(!0 >> (128 - bits)),
        }
    };

    let scalar = |value: Primitive| {
        let mut scalar = scalar_unit(value);

        if let Some(valid_range) = &ty.info.valid_range {
            scalar.valid_range = valid_range.clone();
        }

        Layout::scalar(scalar, target)
    };

    let ptr_size = match target.pointer_width() {
        Ok(PointerWidth::U16) => Size::from_bits(16),
        Ok(PointerWidth::U32) => Size::from_bits(32),
        Ok(PointerWidth::U64) => Size::from_bits(64),
        Err(_) => Size::from_bits(64),
    };

    let mut layout = match &ty.kind {
        Type::Recurse(..) => unreachable!(),
        Type::U8 => scalar(Primitive::Int(Integer::I8, false)),
        Type::U16 => scalar(Primitive::Int(Integer::I16, false)),
        Type::U32 => scalar(Primitive::Int(Integer::I32, false)),
        Type::U64 => scalar(Primitive::Int(Integer::I64, false)),
        Type::U128 => scalar(Primitive::Int(Integer::I128, false)),
        Type::I8 => scalar(Primitive::Int(Integer::I8, true)),
        Type::I16 => scalar(Primitive::Int(Integer::I16, true)),
        Type::I32 => scalar(Primitive::Int(Integer::I32, true)),
        Type::I64 => scalar(Primitive::Int(Integer::I64, true)),
        Type::I128 => scalar(Primitive::Int(Integer::I128, true)),
        Type::F32 => scalar(Primitive::F32),
        Type::F64 => scalar(Primitive::F64),
        Type::Ptr(_) => scalar(Primitive::Pointer),
        Type::Box(_) => {
            let mut scalar = scalar_unit(Primitive::Pointer);

            scalar.valid_range = 1..=*scalar.valid_range.end();
            Layout::scalar(scalar, target)
        }
        Type::Func(_) => {
            let mut ptr = scalar_unit(Primitive::Pointer);

            ptr.valid_range = 1..=*ptr.valid_range.end();
            Layout::scalar(ptr, target)
        }
        Type::Tuple(tys) => {
            struct_layout(tys.iter().map(|t| layout_of(t, target)).collect(), target)
        }
        Type::Opaque(_) => Layout {
            size: Size::ZERO,
            align: Align::from_bytes(1),
            stride: Size::ZERO,
            abi: Abi::Aggregate { sized: false },
            fields: FieldsShape::Arbitrary {
                offsets: Vec::new(),
            },
            variants: Variants::Single { index: 0 },
            largest_niche: None,
        },
        Type::Type(_) => Layout {
            size: ptr_size * 4,
            align: Align::from_bytes(ptr_size.bytes()),
            stride: ptr_size * 4,
            abi: Abi::Aggregate { sized: true },
            fields: FieldsShape::Arbitrary {
                offsets: vec![Size::ZERO, ptr_size, ptr_size * 2, ptr_size * 3],
            },
            variants: Variants::Single { index: 0 },
            largest_niche: None,
        },
        Type::Vwt(_) => Layout {
            size: ptr_size * 3,
            align: Align::from_bytes(ptr_size.bytes()),
            stride: ptr_size * 3,
            abi: Abi::Aggregate { sized: true },
            fields: FieldsShape::Arbitrary {
                offsets: vec![Size::ZERO, ptr_size, ptr_size * 2],
            },
            variants: Variants::Single { index: 0 },
            largest_niche: None,
        },
        Type::Union(tys) => {
            let lyts = tys.iter().map(|t| layout_of(t, target)).collect::<Vec<_>>();
            let max = lyts.iter().max_by_key(|l| l.size).unwrap();

            Layout {
                size: max.size,
                align: max.align,
                stride: max.stride,
                abi: Abi::Aggregate { sized: true },
                fields: FieldsShape::Union(tys.len()),
                variants: Variants::Single { index: 0 },
                largest_niche: None,
            }
        }
        Type::Tagged(tys) => {
            let lyts = tys
                .iter()
                .map(|t| layout_of(t, target).layout)
                .collect::<Vec<_>>();

            enum_layout(lyts, target)
        }
        Type::Discr(ty) => {
            let layout = layout_of(ty, target);

            if let Variants::Multiple { tag, .. } = &layout.variants {
                Layout::scalar(tag.clone(), target)
            } else {
                unreachable!();
            }
        }
    };

    if let Some(ref abi) = ty.info.abi {
        layout.abi = abi.clone();
    }

    TyLayout {
        ty: ty.clone(),
        layout,
    }
}

fn struct_layout(fields: Vec<TyLayout>, target: &Triple) -> Layout {
    let mut align = Align::from_bytes(1);
    let mut offsets = vec![Size::ZERO; fields.len()];
    let mut offset = Size::ZERO;
    let mut niches = Vec::new();

    for (i, field) in fields.into_iter().enumerate() {
        if let Some(niche) = &field.largest_niche {
            niches.push(niche.clone());
        }

        offset = offset.align_to(field.align);
        align = align.max(field.align);
        offsets[i] = offset;
        offset = offset + field.size;
    }

    let size = offset;
    let stride = offset.align_to(align);
    let abi = Abi::Aggregate { sized: true };
    let largest_niche = niches.into_iter().max_by_key(|n| n.available(target));

    Layout {
        size,
        align,
        stride,
        abi,
        fields: FieldsShape::Arbitrary { offsets },
        variants: Variants::Single { index: 0 },
        largest_niche,
    }
}

fn enum_layout(mut variants: Vec<Layout>, target: &Triple) -> Layout {
    if variants.is_empty() {
        Layout {
            fields: FieldsShape::Arbitrary {
                offsets: Vec::new(),
            },
            variants: Variants::Single { index: 0 },
            largest_niche: None,
            abi: Abi::Aggregate { sized: true },
            size: Size::ZERO,
            align: Align::from_bytes(1),
            stride: Size::ZERO,
        }
    } else if variants.len() == 1 {
        variants.remove(0)
    } else {
        let largest_niche = variants
            .iter()
            .filter_map(|v| v.largest_niche.clone())
            .max_by_key(|niche| niche.available(target));

        for (i, variant) in variants.iter_mut().enumerate() {
            variant.variants = Variants::Single { index: i };
        }

        let largest = variants.iter().max_by_key(|v| v.size).unwrap();
        let align = largest.align;
        let mut size = largest.size;
        let mut no_niche = |mut variants: Vec<Layout>| {
            let tag_size = Size::from_bits(variants.len()).align_to(align);
            let offsets = vec![Size::ZERO, tag_size];
            let tag = Scalar {
                value: Primitive::Int(
                    match tag_size.bytes() {
                        1 => Integer::I8,
                        2 => Integer::I16,
                        3 | 4 => Integer::I32,
                        5 | 6 | 7 | 8 => Integer::I64,
                        _ => Integer::I128,
                    },
                    false,
                ),
                valid_range: 0..=u128::max_value(),
            };

            let tag_encoding = TagEncoding::Direct;

            size = size + tag_size;

            for variant in &mut variants {
                if let FieldsShape::Arbitrary { offsets } = &mut variant.fields {
                    for offset in offsets {
                        *offset = *offset + tag_size;
                    }
                }
            }

            (
                FieldsShape::Arbitrary { offsets },
                Variants::Multiple {
                    tag,
                    tag_encoding,
                    variants,
                    tag_field: 0,
                },
            )
        };

        let (fields, variants) = if let Some(niche) = largest_niche {
            if niche.available(target) >= variants.len() as u128 {
                // @TODO: implement niches
                no_niche(variants)
            } else {
                no_niche(variants)
            }
        } else {
            no_niche(variants)
        };

        let stride = size.align_to(align);

        Layout {
            fields,
            variants,
            largest_niche: None,
            abi: Abi::Aggregate { sized: true },
            size,
            align,
            stride,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyLayout {
    pub ty: Ty,
    pub layout: Layout,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Layout {
    pub size: Size,
    pub align: Align,
    pub stride: Size,
    pub abi: Abi,
    pub fields: FieldsShape,
    pub variants: Variants,
    pub largest_niche: Option<Niche>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Abi {
    Uninhabited,
    Scalar(Scalar),
    ScalarPair(Scalar, Scalar),
    Aggregate { sized: bool },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FieldsShape {
    Primitive,
    Union(usize),
    Array { stride: Size, count: u64 },
    Arbitrary { offsets: Vec<Size> },
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
pub enum TagEncoding {
    Direct,
    Niche {
        dataful_variant: usize,
        niche_variants: RangeInclusive<usize>,
        niche_start: u128,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Niche {
    pub offset: Size,
    pub scalar: Scalar,
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Size {
    raw: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Align {
    pow2: u8,
}

impl std::ops::Deref for TyLayout {
    type Target = Layout;

    fn deref(&self) -> &Layout {
        &self.layout
    }
}

pub fn ptr_sized_int(target: &Triple) -> Ty {
    match target.pointer_width() {
        Ok(PointerWidth::U16) => Ty::new(Type::I16),
        Ok(PointerWidth::U32) => Ty::new(Type::I32),
        Ok(PointerWidth::U64) => Ty::new(Type::I64),
        Err(_) => Ty::new(Type::I64),
    }
}

pub fn copy_fn_type(t: &String) -> Ty {
    Ty::new(Type::Func(crate::Signature {
        params: vec![
            Ty::new(Type::Ptr(Box::new(Ty::new(Type::Opaque(t.clone()))))),
            Ty::new(Type::Ptr(Box::new(Ty::new(Type::Opaque(t.clone()))))),
            Ty::new(Type::Ptr(Box::new(Ty::new(Type::Type(t.clone()))))),
        ],
        rets: Vec::new(),
    }))
}

pub fn drop_fn_type(t: &String) -> Ty {
    Ty::new(Type::Func(crate::Signature {
        params: vec![
            Ty::new(Type::Ptr(Box::new(Ty::new(Type::Opaque(t.clone()))))),
            Ty::new(Type::Ptr(Box::new(Ty::new(Type::Type(t.clone()))))),
        ],
        rets: Vec::new(),
    }))
}

impl TyLayout {
    pub fn unit() -> Self {
        TyLayout {
            ty: Ty::new(Type::Tuple(Vec::new())),
            layout: Layout {
                size: Size::ZERO,
                align: Align::from_bytes(1),
                stride: Size::ZERO,
                abi: Abi::Aggregate { sized: true },
                fields: FieldsShape::Arbitrary {
                    offsets: Vec::new(),
                },
                variants: Variants::Single { index: 0 },
                largest_niche: None,
            },
        }
    }

    pub fn pointee(&self, target: &Triple) -> Self {
        if let Type::Ptr(to) = &self.ty.kind {
            layout_of(to, target)
        } else if let Type::Box(to) = &self.ty.kind {
            layout_of(to, target)
        } else {
            unreachable!();
        }
    }

    pub fn element(&self, target: &Triple) -> Self {
        unimplemented!();
    }

    pub fn field(&self, field: usize, target: &Triple) -> Self {
        assert!(field < self.fields.count());

        let ty = match &self.ty.kind {
            Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::U128
            | Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::I128
            | Type::F32
            | Type::F64
            | Type::Ptr(_)
            | Type::Box(_)
            | Type::Func(_)
            | Type::Opaque(_)
            | Type::Discr(_)
            | Type::Recurse(_) => unreachable!(),
            Type::Type(t) => match field {
                0 => ptr_sized_int(target),
                1 => ptr_sized_int(target),
                2 => ptr_sized_int(target),
                3 => Ty::new(Type::Ptr(Box::new(Ty::new(Type::Vwt(t.clone()))))),
                _ => unreachable!(),
            },
            Type::Vwt(t) => match field {
                0 => copy_fn_type(t),
                1 => copy_fn_type(t),
                2 => drop_fn_type(t),
                _ => unreachable!(),
            },
            Type::Tuple(tys) => tys[field].clone(),
            Type::Union(tys) => tys[field].clone(),
            Type::Tagged(tys) => match self.variants {
                Variants::Single { index } => layout_of(&tys[index], target)
                    .field(field, target)
                    .ty
                    .clone(),
                Variants::Multiple { ref tag, .. } => {
                    assert_eq!(field, 0);

                    return TyLayout {
                        layout: Layout::scalar(tag.clone(), target),
                        ty: tag.value.ty(),
                    };
                }
            },
        };

        layout_of(&ty, target)
    }

    pub fn variant(&self, variant: usize) -> Self {
        let layout = match self.variants {
            Variants::Single { index }
                if variant == index && self.fields != FieldsShape::Primitive =>
            {
                self.layout.clone()
            }
            Variants::Single { index } => {
                let fields = match &self.ty {
                    // Type::Data(id) => db.variants(*id).len(),
                    // _ => unreachable!(),
                    _ => 1,
                };

                Layout {
                    variants: Variants::Single { index },
                    fields: if fields == 0 {
                        FieldsShape::Arbitrary {
                            offsets: Vec::new(),
                        }
                    } else {
                        FieldsShape::Union(fields)
                    },
                    abi: Abi::Uninhabited,
                    largest_niche: None,
                    align: Align::from_bytes(1),
                    size: Size::ZERO,
                    stride: Size::ZERO,
                }
            }
            Variants::Multiple { ref variants, .. } => variants[variant].clone(),
        };

        TyLayout {
            layout,
            ty: self.ty.clone(),
        }
    }
}

impl Layout {
    pub fn scalar(scalar: Scalar, triple: &target_lexicon::Triple) -> Self {
        let size = scalar.value.size(triple);
        let align = Align::from_bytes(size.bytes());
        let largest_niche = Niche::from_scalar(triple, Size::ZERO, scalar.clone());

        Layout {
            fields: FieldsShape::Primitive,
            variants: Variants::Single { index: 0 },
            largest_niche,
            abi: Abi::Scalar(scalar),
            size,
            align,
            stride: size,
        }
    }

    pub fn is_unsized(&self) -> bool {
        self.abi.is_unsized()
    }

    pub fn is_zst(&self) -> bool {
        match self.abi {
            Abi::Scalar(_) | Abi::ScalarPair(..) => false,
            Abi::Uninhabited => self.size.bytes() == 0,
            Abi::Aggregate { sized } => sized && self.size.bytes() == 0,
        }
    }
}

impl Abi {
    pub fn is_unsized(&self) -> bool {
        match self {
            Abi::Uninhabited | Abi::Scalar(_) | Abi::ScalarPair(..) => false,
            Abi::Aggregate { sized } => !sized,
        }
    }

    pub fn is_signed(&self) -> bool {
        match self {
            Abi::Scalar(scalar) => match scalar.value {
                Primitive::Int(_, signed) => signed,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Abi::Scalar(scalar) => match scalar.value {
                Primitive::F32 => true,
                Primitive::F64 => true,
                _ => false,
            },
            _ => false,
        }
    }
}

impl FieldsShape {
    pub fn count(&self) -> usize {
        match self {
            FieldsShape::Primitive => 0,
            FieldsShape::Union(count) => *count,
            FieldsShape::Array { count, .. } => *count as usize,
            FieldsShape::Arbitrary { offsets } => offsets.len(),
        }
    }

    pub fn offset(&self, idx: usize) -> Size {
        match self {
            FieldsShape::Primitive => unreachable!(),
            FieldsShape::Union(_) => Size::ZERO,
            FieldsShape::Array { stride, count: _ } => {
                let i = u64::try_from(idx).unwrap();

                *stride * i
            }
            FieldsShape::Arbitrary { offsets } => offsets[idx],
        }
    }
}

impl Niche {
    pub fn from_scalar(
        triple: &target_lexicon::Triple,
        offset: Size,
        scalar: Scalar,
    ) -> Option<Self> {
        let niche = Niche { offset, scalar };

        if niche.available(triple) > 0 {
            Some(niche)
        } else {
            None
        }
    }

    pub fn available(&self, triple: &target_lexicon::Triple) -> u128 {
        let Scalar {
            value,
            valid_range: ref v,
        } = self.scalar;
        let bits = value.size(triple).bits();
        assert!(bits <= 128);
        let max_value = !0u128 >> (128 - bits);
        let niche = v.end().wrapping_add(1)..*v.start();

        niche.end.wrapping_sub(niche.start) & max_value
    }

    pub fn reserve(&self, triple: &target_lexicon::Triple, count: u128) -> Option<(u128, Scalar)> {
        assert!(count > 0);
        let Scalar {
            value,
            valid_range: ref v,
        } = self.scalar;
        let bits = value.size(triple).bits();
        assert!(bits <= 128);
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
            None
        } else {
            Some((
                start,
                Scalar {
                    value,
                    valid_range: *v.start()..=end,
                },
            ))
        }
    }
}

impl Size {
    pub const ZERO: Self = Size { raw: 0 };

    pub fn from_bits(bits: impl TryInto<u64>) -> Self {
        let bits = bits.try_into().ok().unwrap();

        Size::from_bytes(bits / 8 + ((bits % 8) + 7) / 8)
    }

    pub fn from_bytes(bytes: impl TryInto<u64>) -> Self {
        Size {
            raw: bytes.try_into().ok().unwrap(),
        }
    }

    pub fn bytes(self) -> u64 {
        self.raw
    }

    pub fn bits(self) -> u64 {
        self.bytes() * 8
    }

    pub fn align_to(self, align: Align) -> Self {
        let mask = align.bytes() - 1;

        Size::from_bytes((self.bytes() + mask) & !mask)
    }

    pub fn is_aligned(self, align: Align) -> bool {
        let mask = align.bytes() - 1;

        self.bytes() & mask == 0
    }
}

impl Align {
    pub fn from_bits(bits: u64) -> Self {
        Align::from_bytes(Size::from_bits(bits).bytes())
    }

    pub fn from_bytes(mut bytes: u64) -> Self {
        if bytes == 0 {
            return Align { pow2: 0 };
        }

        let mut pow2: u8 = 0;

        while (bytes & 1) == 0 {
            pow2 += 1;
            bytes >>= 1;
        }

        Align { pow2 }
    }

    pub fn bytes(self) -> u64 {
        1 << self.pow2
    }

    pub fn bits(self) -> u64 {
        self.bytes() * 8
    }

    pub fn max_for_offset(offset: Size) -> Self {
        Align {
            pow2: offset.bytes().trailing_zeros() as u8,
        }
    }

    pub fn restrict_for_offset(self, offset: Size) -> Self {
        self.min(Align::max_for_offset(offset))
    }
}

impl Add for Size {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Size {
            raw: self.raw + other.raw,
        }
    }
}

impl Mul<u64> for Size {
    type Output = Self;

    fn mul(self, other: u64) -> Self {
        Size {
            raw: self.raw * other,
        }
    }
}

impl Scalar {
    pub fn is_bool(&self) -> bool {
        self.valid_range == (0..=1) && matches!(self.value, Primitive::Int(Integer::I8, _))
    }

    pub fn unit(value: Primitive, target: &Triple) -> Self {
        let bits = value.size(target).bits();

        Scalar {
            value,
            valid_range: 0..=(!0 >> (128 - bits)),
        }
    }
}

impl Primitive {
    pub fn size(&self, triple: &target_lexicon::Triple) -> Size {
        match self {
            Primitive::Int(i, _) => i.size(),
            Primitive::F32 => Size::from_bits(32),
            Primitive::F64 => Size::from_bits(64),
            Primitive::Pointer => match triple.pointer_width() {
                Ok(pw) => Size::from_bytes(pw.bytes()),
                Err(_) => Size::from_bits(32),
            },
        }
    }

    pub fn align(&self, triple: &target_lexicon::Triple) -> Align {
        Align::from_bytes(self.size(triple).bytes())
    }

    pub fn ty(&self) -> Ty {
        match self {
            Primitive::Int(Integer::I8, _) => Ty::new(Type::I8),
            Primitive::Int(Integer::I16, _) => Ty::new(Type::I16),
            Primitive::Int(Integer::I32, _) => Ty::new(Type::I32),
            Primitive::Int(Integer::I64, _) => Ty::new(Type::I64),
            Primitive::Int(Integer::I128, _) => Ty::new(Type::I128),
            Primitive::F32 => Ty::new(Type::F32),
            Primitive::F64 => Ty::new(Type::F64),
            Primitive::Pointer => unreachable!(),
        }
    }
}

impl Integer {
    pub fn size(&self) -> Size {
        match self {
            Integer::I8 => Size::from_bytes(1),
            Integer::I16 => Size::from_bytes(2),
            Integer::I32 => Size::from_bytes(4),
            Integer::I64 => Size::from_bytes(8),
            Integer::I128 => Size::from_bytes(16),
        }
    }

    pub fn align(&self) -> Align {
        Align::from_bytes(self.size().bytes())
    }

    pub fn ptr_sized(triple: &target_lexicon::Triple) -> Self {
        match triple.pointer_width() {
            Ok(target_lexicon::PointerWidth::U16) => Integer::I16,
            Ok(target_lexicon::PointerWidth::U32) => Integer::I32,
            Ok(target_lexicon::PointerWidth::U64) => Integer::I64,
            Err(_) => Integer::I32,
        }
    }
}
