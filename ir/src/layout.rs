use crate::db::IrDatabase;
use crate::ty::*;
use crate::{Flags, TypeDefBody};
use std::convert::TryInto;
use std::num::NonZeroUsize;
use std::ops::{self, Range, RangeInclusive};
use target_lexicon::PointerWidth;
pub use target_lexicon::Triple;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyAndLayout {
    pub ty: Ty,
    pub layout: Layout,
}

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

impl TyAndLayout {
    pub fn pointee(&self, db: &dyn IrDatabase) -> TyAndLayout {
        match self.ty.lookup(db).kind {
            | typ::Ptr(to) => db.layout_of(to),
            | _ => unreachable!(),
        }
    }

    pub fn field(&self, db: &dyn IrDatabase, idx: usize) -> TyAndLayout {
        match self.ty.lookup(db).kind {
            | typ::Box(of) => match idx {
                | 0 => db.layout_of(of.ptr(db)),
                | 1 => db.layout_of(Ty::int(db, Integer::ISize, false)),
                | _ => unreachable!(),
            },
            | typ::Tuple(ref ts) => db.layout_of(ts[idx]),
            | typ::Def(id, ref subst) => {
                let subst = subst.as_deref().unwrap_or(&[]);
                let info = id.lookup(db);
                let body = info.body.as_ref().unwrap();

                match body {
                    | TypeDefBody::Struct { fields } => db.layout_of(fields[idx].ty.subst(db, subst, 0)),
                    | _ => unimplemented!(),
                }
            },
            | _ => unreachable!(),
        }
    }
}

impl Layout {
    pub const UNIT: Self = Self {
        size: Size::ZERO,
        align: Align::ONE,
        stride: Size::ZERO,
        abi: Abi::Aggregate { sized: true },
        fields: Fields::Arbitrary {
            offsets: Vec::new(),
            memory_index: Vec::new(),
        },
        variants: Variants::Single { index: 0 },
        largest_niche: None,
    };

    pub const UNINHABITED: Self = Self {
        size: Size::ZERO,
        align: Align::ONE,
        stride: Size::ZERO,
        abi: Abi::Uninhabited,
        fields: Fields::Primitive,
        variants: Variants::Single { index: 0 },
        largest_niche: None,
    };

    pub fn scalar(scalar: Scalar, triple: &Triple) -> Self {
        let size = scalar.value.size(triple);
        let align = Align::from_bytes(size.bytes());
        let largest_niche = Niche::from_scalar(triple, Size::ZERO, scalar.clone());

        Self {
            size,
            align,
            stride: size,
            abi: Abi::Scalar(scalar),
            fields: Fields::Primitive,
            variants: Variants::Single { index: 0 },
            largest_niche,
        }
    }

    pub fn is_zst(&self) -> bool {
        match &self.abi {
            | Abi::Uninhabited => self.size == Size::ZERO,
            | Abi::Scalar(_) | Abi::ScalarPair(_, _) | Abi::Vector { .. } => false,
            | Abi::Aggregate { sized } => *sized && self.size == Size::ZERO,
        }
    }
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
    fn new(value: Primitive, triple: &Triple) -> Self {
        let bits = value.size(triple).bits();

        Scalar {
            value,
            valid_range: 0..=(!0 >> (128 - bits)),
        }
    }

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

    pub fn align(self, triple: &Triple) -> Align {
        Align::from_bytes(self.size(triple).bytes())
    }

    pub fn fit_unsigned(x: u128) -> Self {
        match x {
            | 0..=0x0000_0000_0000_00FF => Integer::I8,
            | 0..=0x0000_0000_0000_FFFF => Integer::I16,
            | 0..=0x0000_0000_FFFF_FFFF => Integer::I32,
            | 0..=0xFFFF_FFFF_FFFF_FFFF => Integer::I64,
            | _ => Integer::I128,
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

impl std::ops::Deref for TyAndLayout {
    type Target = Layout;

    fn deref(&self) -> &Self::Target {
        &self.layout
    }
}

pub fn layout_of(db: &dyn IrDatabase, ty: Ty) -> TyAndLayout {
    let triple = db.triple();
    let info = ty.lookup(db);
    let scalar_unit = |value: Primitive| Scalar::new(value, &triple);
    let scalar = |value: Primitive| {
        let mut scalar = scalar_unit(value);

        if let Some(start) = info.repr.valid_range_start {
            scalar.valid_range = start..=*scalar.valid_range.end();
        }

        if let Some(end) = info.repr.valid_range_end {
            scalar.valid_range = *scalar.valid_range.start()..=end;
        }

        Layout::scalar(scalar, &triple)
    };

    let layout = match info.kind {
        | typ::Unit => {
            if let Some(prim) = info.repr.scalar {
                scalar(prim)
            } else {
                Layout::UNIT
            }
        },
        | typ::Ptr(_) => {
            let mut scalar = scalar_unit(Primitive::Pointer);

            if let Some(start) = info.repr.valid_range_start {
                scalar.valid_range = start..=*scalar.valid_range.end();
            } else if info.flags.is_set(Flags::NON_NULL) {
                scalar.valid_range = 1..=*scalar.valid_range.end();
            }

            if let Some(end) = info.repr.valid_range_end {
                scalar.valid_range = *scalar.valid_range.start()..=end;
            }

            Layout::scalar(scalar, &triple)
        },
        | typ::Box(_) => {
            let mut ptr = Scalar::new(Primitive::Pointer, &triple);
            let gen = Scalar::new(Primitive::Int(Integer::ISize, false), &triple);

            ptr.valid_range = 1..=*ptr.valid_range.end();

            scalar_pair(ptr, gen, &triple)
        },
        | typ::Tuple(ref ts) => {
            let fields = ts.iter().map(|&t| db.layout_of(t)).collect::<Vec<_>>();

            struct_layout(&info, &fields, &triple)
        },
        | typ::Func(_) => {
            let mut scalar = scalar_unit(Primitive::Pointer);

            scalar.valid_range = 1..=*scalar.valid_range.end();

            Layout::scalar(scalar, &triple)
        },
        | typ::Generic(_, t) => db.layout_of(t).layout,
        | typ::Var(_) => unreachable!(),
        | typ::Def(id, ref subst) => {
            let def = id.lookup(db);

            match &def.body {
                | None => Layout::UNINHABITED,
                | Some(body) => match body {
                    | TypeDefBody::Struct { fields } => {
                        let fields = fields
                            .iter()
                            .map(|f| {
                                let ty = if let Some(subst) = subst { f.ty.subst(db, subst, 0) } else { f.ty };

                                db.layout_of(ty)
                            })
                            .collect::<Vec<_>>();

                        struct_layout(&info, &fields, &triple)
                    },
                    | TypeDefBody::Union { fields } => {
                        let fields = fields
                            .iter()
                            .map(|f| {
                                let ty = if let Some(subst) = subst { f.ty.subst(db, subst, 0) } else { f.ty };

                                db.layout_of(ty)
                            })
                            .collect::<Vec<_>>();

                        union_layout(&info, &fields, &triple)
                    },
                    | TypeDefBody::Enum { variants } => {
                        let variants = variants
                            .iter()
                            .map(|v| {
                                v.payload.map(|p| {
                                    let ty = if let Some(subst) = subst { p.subst(db, subst, 0) } else { p };

                                    db.layout_of(ty).layout
                                })
                            })
                            .collect::<Vec<_>>();

                        enum_layout(&info, &variants, &triple)
                    },
                },
            }
        },
    };

    TyAndLayout { ty, layout }
}

fn scalar_pair(a: Scalar, b: Scalar, triple: &Triple) -> Layout {
    let b_align = b.value.align(triple);
    let align = a.value.align(triple).max(b_align);
    let b_offset = a.value.size(triple).align_to(b_align);
    let size = b_offset + b.value.size(triple);
    let largest_niche = Niche::from_scalar(triple, b_offset, b.clone())
        .into_iter()
        .chain(Niche::from_scalar(triple, Size::ZERO, a.clone()))
        .max_by_key(|n| n.available(triple));

    Layout {
        size,
        align,
        stride: size.align_to(align),
        abi: Abi::ScalarPair(a, b),
        fields: Fields::Arbitrary {
            offsets: vec![Size::ZERO, b_offset],
            memory_index: vec![0, 1],
        },
        variants: Variants::Single { index: 0 },
        largest_niche,
    }
}

fn struct_layout(info: &Type, fields: &[TyAndLayout], triple: &Triple) -> Layout {
    let optimize = !info.flags.is_set(Flags::C_REPR);
    let packed = info.flags.is_set(Flags::PACKED);
    let mut inverse_memory_index = (0..fields.len()).collect::<Vec<_>>();

    if optimize {
        let optimizing = &mut inverse_memory_index[..fields.len()];

        optimizing.sort_by_key(|&x| {
            let f = &fields[x];

            (!f.is_zst(), std::cmp::Reverse(f.align))
        });
    }

    let mut sized = true;
    let mut offsets = vec![Size::ZERO; fields.len()];
    let mut offset = Size::ZERO;
    let mut align = Align::ONE;
    let mut largest_niche = None;
    let mut largest_niche_available = 0;

    for &i in &inverse_memory_index {
        let field = &fields[i];

        if !sized {
            panic!("struct_layout: field after unsized field");
        }

        if field.abi.is_unsized() {
            sized = false;
        }

        if !packed {
            offset = offset.align_to(field.align);
        }

        align = align.max(field.align);
        offsets[i] = offset;

        if let Some(mut niche) = field.largest_niche.clone() {
            let available = niche.available(triple);

            if available > largest_niche_available {
                largest_niche_available = available;
                niche.offset = niche.offset + offset;
                largest_niche = Some(niche);
            }
        }

        offset = offset + field.size;
    }

    let memory_index = if optimize {
        invert_mapping(&inverse_memory_index)
    } else {
        inverse_memory_index
    };

    let size = offset;
    let stride = size.align_to(align);
    let mut abi = Abi::Aggregate { sized };

    if sized && size.bytes() > 0 {
        let mut non_zst_fields = fields.iter().enumerate().filter(|(_, f)| !f.is_zst());

        match (non_zst_fields.next(), non_zst_fields.next(), non_zst_fields.next()) {
            | (Some((i, field)), None, None) => {
                if offsets[i].bytes() == 0 && align == field.align && size == field.size {
                    match field.abi {
                        | Abi::Scalar(_) | Abi::Vector { .. } if optimize => {
                            abi = field.abi.clone();
                        },
                        | Abi::ScalarPair(_, _) => {
                            abi = field.abi.clone();
                        },
                        | _ => {},
                    }
                }
            },
            | (
                Some((
                    i,
                    TyAndLayout {
                        layout: Layout { abi: Abi::Scalar(ref a), .. },
                        ..
                    },
                )),
                Some((
                    j,
                    TyAndLayout {
                        layout: Layout { abi: Abi::Scalar(ref b), .. },
                        ..
                    },
                )),
                None,
            ) => {
                let ((i, a), (j, b)) = if offsets[i] < offsets[j] { ((i, a), (j, b)) } else { ((j, b), (i, a)) };

                let pair = scalar_pair(a.clone(), b.clone(), triple);
                let pair_offsets = match pair.fields {
                    | Fields::Arbitrary { ref offsets, .. } => offsets,
                    | _ => unreachable!(),
                };

                if offsets[i] == pair_offsets[0] && offsets[j] == pair_offsets[1] && align == pair.align && size == pair.size {
                    abi = pair.abi.clone();
                }
            },
            | _ => {},
        }
    }

    if sized && fields.iter().any(|f| f.abi.is_uninhabited()) {
        abi = Abi::Uninhabited;
    }

    Layout {
        size,
        align,
        stride,
        abi,
        fields: Fields::Arbitrary { offsets, memory_index },
        variants: Variants::Single { index: 0 },
        largest_niche,
    }
}

fn union_layout(info: &Type, fields: &[TyAndLayout], triple: &Triple) -> Layout {
    let optimize = !info.flags.is_set(Flags::C_REPR);
    let mut align = Align::ONE;
    let mut size = Size::ZERO;
    let mut abi = Abi::Aggregate { sized: true };

    for field in fields {
        if optimize && !field.is_zst() {
            let field_abi = match &field.abi {
                | Abi::Scalar(x) => Abi::Scalar(Scalar::new(x.value, triple)),
                | Abi::ScalarPair(a, b) => Abi::ScalarPair(Scalar::new(a.value, triple), Scalar::new(b.value, triple)),
                | Abi::Vector { elem, count } => Abi::Vector {
                    elem: Scalar::new(elem.value, triple),
                    count: *count,
                },
                | Abi::Uninhabited | Abi::Aggregate { .. } => Abi::Aggregate { sized: true },
            };

            if size == Size::ZERO {
                abi = field_abi;
            } else if abi != field_abi {
                abi = Abi::Aggregate { sized: true };
            }
        }

        align = align.max(field.align);
        size = size.max(field.size);
    }

    Layout {
        size,
        align,
        stride: size.align_to(align),
        abi,
        fields: Fields::Union(NonZeroUsize::new(fields.len()).unwrap()),
        variants: Variants::Single { index: 0 },
        largest_niche: None,
    }
}

fn enum_layout(info: &Type, variants: &[Option<Layout>], triple: &Triple) -> Layout {
    let optimize = !info.flags.is_set(Flags::C_REPR);
    let absent = |v: &Option<Layout>| v.as_ref().map(|l| l.is_zst() && l.abi.is_uninhabited()).unwrap_or(false);

    let (present_first, present_second) = {
        let mut present_variants = variants.iter().enumerate().filter_map(|(i, v)| if absent(v) { None } else { Some(i) });

        (present_variants.next(), present_variants.next())
    };

    let present_first = match present_first {
        | Some(present_first) => present_first,
        | None => return Layout::UNINHABITED,
    };

    if present_second.is_none() && optimize {
        // let mut st = variants[present_first].as_ref().unwrap();
        //
        // st.layout.variants = Variants::Single { index: present_first };
        unimplemented!();
    }

    let mut niche_filling_layout = None;

    if optimize {
        let mut dataful_variant = None;
        let mut niche_variants = usize::MAX..=0;

        for (v, payload) in variants.iter().enumerate() {
            if absent(payload) {
                continue;
            }

            if let Some(payload) = payload {
                if !payload.is_zst() {
                    if let None = dataful_variant {
                        dataful_variant = Some(v);
                        continue;
                    } else {
                        dataful_variant = None;
                        break;
                    }
                }
            }

            niche_variants = *niche_variants.start().min(&v)..=v;
        }

        if niche_variants.start() > niche_variants.end() {
            dataful_variant = None;
        }

        if let Some(i) = dataful_variant {
            let count = (*niche_variants.end() - *niche_variants.start() + 1) as u128;
            let niche_candidate = variants[i].as_ref().and_then(|v| v.largest_niche.as_ref());

            if let Some((niche, (niche_start, niche_scalar))) = niche_candidate.and_then(|niche| Some((niche, niche.reserve(triple, count)?))) {
                let mut align = Align::ONE;
                let st = variants
                    .iter()
                    .enumerate()
                    .map(|(i, payload)| {
                        let mut st = payload.clone().unwrap_or(Layout::UNIT);

                        st.variants = Variants::Single { index: i };
                        align = align.max(st.align);
                        st
                    })
                    .collect::<Vec<_>>();

                let offset = niche.offset;
                let size = st[i].size;
                let abi = if st.iter().all(|v| v.abi.is_uninhabited()) {
                    Abi::Uninhabited
                } else {
                    match st[i].abi {
                        | Abi::Scalar(_) => Abi::Scalar(niche_scalar.clone()),
                        | Abi::ScalarPair(ref first, ref second) => {
                            if offset.bytes() > 0 {
                                Abi::ScalarPair(niche_scalar.clone(), Scalar::new(second.value, triple))
                            } else {
                                Abi::ScalarPair(Scalar::new(first.value, triple), niche_scalar.clone())
                            }
                        },
                        | _ => Abi::Aggregate { sized: true },
                    }
                };

                let largest_niche = Niche::from_scalar(triple, offset, niche_scalar.clone());

                niche_filling_layout = Some(Layout {
                    size,
                    align,
                    stride: size.align_to(align),
                    abi,
                    fields: Fields::Arbitrary {
                        offsets: vec![offset],
                        memory_index: vec![0],
                    },
                    variants: Variants::Multiple {
                        tag: niche_scalar,
                        tag_encoding: TagEncoding::Niche {
                            dataful_variant: i,
                            niche_variants,
                            niche_start,
                        },
                        tag_field: 0,
                        variants: st,
                    },
                    largest_niche,
                });
            }
        }
    }

    let discr = Integer::fit_unsigned(variants.len() as u128);
    let mut align = Align::ONE;
    let mut size = Size::ZERO;
    let mut prefix_align = discr.align(triple);

    if info.flags.is_set(Flags::C_REPR) {
        for v in variants {
            if let Some(p) = v {
                prefix_align = prefix_align.max(p.align);
            }
        }
    }

    let prefix_size = discr.size(triple).align_to(prefix_align);
    let layout_variants = variants
        .iter()
        .enumerate()
        .map(|(i, payload)| {
            let mut st = payload.clone().unwrap_or(Layout::UNIT);

            if let Fields::Arbitrary { offsets, .. } = &mut st.fields {
                for offset in offsets {
                    *offset = *offset + prefix_size;
                }
            }

            size = size.max(st.size);
            align = align.max(st.align);
            st.variants = Variants::Single { index: i };
            st
        })
        .collect::<Vec<_>>();

    size = size + prefix_size;

    let stride = size.align_to(align);
    let tag_mask = !0u128 >> (128 - discr.size(triple).bits());
    let tag = Scalar {
        value: Primitive::Int(discr, false),
        valid_range: (0 & tag_mask)..=(variants.len() as u128 & tag_mask),
    };

    let mut abi = Abi::Aggregate { sized: true };

    if tag.value.size(triple) == size {
        abi = Abi::Scalar(tag.clone());
    }

    if layout_variants.iter().all(|v| v.abi.is_uninhabited()) {
        abi = Abi::Uninhabited;
    }

    let largest_niche = Niche::from_scalar(triple, Size::ZERO, tag.clone());
    let tagged_layout = Layout {
        size,
        align,
        stride,
        abi,
        fields: Fields::Arbitrary {
            offsets: vec![Size::ZERO],
            memory_index: vec![0],
        },
        variants: Variants::Multiple {
            tag,
            tag_encoding: TagEncoding::Direct,
            tag_field: 0,
            variants: layout_variants,
        },
        largest_niche,
    };

    match niche_filling_layout {
        | Some(niche_filling_layout) => std::cmp::min_by_key(tagged_layout, niche_filling_layout, |layout| {
            let niche_size = layout.largest_niche.as_ref().map_or(0, |n| n.available(triple));

            (layout.size, std::cmp::Reverse(niche_size))
        }),
        | _ => tagged_layout,
    }
}

fn invert_mapping(map: &[usize]) -> Vec<usize> {
    let mut inverse = vec![0; map.len()];

    for i in 0..map.len() {
        inverse[map[i]] = i;
    }

    inverse
}
