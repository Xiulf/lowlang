pub use crate::Type;
use intern::Intern;

pub const UNIT: Layout = Layout(0);
pub const BOOL: Layout = Layout(1);
pub const CHAR: Layout = Layout(2);
pub const STR: Layout = Layout(3);
pub const RATIO: Layout = Layout(4);
pub const U8: Layout = Layout(5);
pub const U16: Layout = Layout(6);
pub const U32: Layout = Layout(7);
pub const U64: Layout = Layout(8);
pub const U128: Layout = Layout(9);
pub const USIZE: Layout = Layout(10);
pub const I8: Layout = Layout(11);
pub const I16: Layout = Layout(12);
pub const I32: Layout = Layout(13);
pub const I64: Layout = Layout(14);
pub const I128: Layout = Layout(15);
pub const ISIZE: Layout = Layout(16);
pub const F32: Layout = Layout(17);
pub const F64: Layout = Layout(18);
pub const FSIZE: Layout = Layout(19);
pub const FN: Layout = Layout(20);
pub const PTR_U8: Layout = Layout(21);

/// # Important
/// This should be called before any layouts are created!
pub fn init(triple: &target_lexicon::Triple) {
    use crate::{IntSize, FloatSize};
    use intern::InternerExt;

    let ptr_size = triple.pointer_width().map(target_lexicon::PointerWidth::bytes).unwrap_or(4) as usize;

    LayoutInterner::set(UNIT, Details { ty: Type::Unit, .. Default::default() });
    LayoutInterner::set(BOOL, Details { ty: Type::Bool, size: 1, .. Default::default() });
    LayoutInterner::set(CHAR, Details { ty: Type::Char, size: 4, align: 4, ..Default::default() });
    LayoutInterner::set(STR, Details { ty: Type::Str, size: ptr_size * 2, align: ptr_size * 2, idx: Some(U8), fields: Box::new([(0, PTR_U8), (ptr_size, USIZE)]), ..Default::default() });
    LayoutInterner::set(RATIO, Details { ty: Type::Ratio, size: ptr_size * 2, align: ptr_size * 2, fields: Box::new([(0, ISIZE), (ptr_size, ISIZE)]), ..Default::default() });
    LayoutInterner::set(U8, Details { ty: Type::UInt(IntSize::Bits8), size: 1, ..Default::default() });
    LayoutInterner::set(U16, Details { ty: Type::UInt(IntSize::Bits16), size: 2, align: 2, ..Default::default() });
    LayoutInterner::set(U32, Details { ty: Type::UInt(IntSize::Bits32), size: 4, align: 4, ..Default::default() });
    LayoutInterner::set(U64, Details { ty: Type::UInt(IntSize::Bits64), size: 8, align: 8, ..Default::default() });
    LayoutInterner::set(U128, Details { ty: Type::UInt(IntSize::Bits128), size: 16, align: 16, ..Default::default() });
    LayoutInterner::set(USIZE, match triple.pointer_width().unwrap_or(target_lexicon::PointerWidth::U32) {
        target_lexicon::PointerWidth::U16 => Details { ty: Type::UInt(IntSize::Bits16), size: 2, align: 2, ..Default::default() },
        target_lexicon::PointerWidth::U32 => Details { ty: Type::UInt(IntSize::Bits32), size: 4, align: 4, ..Default::default() },
        target_lexicon::PointerWidth::U64 => Details { ty: Type::UInt(IntSize::Bits64), size: 8, align: 8, ..Default::default() },
    });
    LayoutInterner::set(I8, Details { ty: Type::Int(IntSize::Bits8), size: 1, ..Default::default() });
    LayoutInterner::set(I16, Details { ty: Type::Int(IntSize::Bits16), size: 2, align: 2, ..Default::default() });
    LayoutInterner::set(I32, Details { ty: Type::Int(IntSize::Bits32), size: 4, align: 4, ..Default::default() });
    LayoutInterner::set(I64, Details { ty: Type::Int(IntSize::Bits64), size: 8, align: 8, ..Default::default() });
    LayoutInterner::set(I128, Details { ty: Type::Int(IntSize::Bits128), size: 16, align: 16, ..Default::default() });
    LayoutInterner::set(ISIZE, match triple.pointer_width().unwrap_or(target_lexicon::PointerWidth::U32) {
        target_lexicon::PointerWidth::U16 => Details { ty: Type::Int(IntSize::Bits16), size: 2, align: 2, ..Default::default() },
        target_lexicon::PointerWidth::U32 => Details { ty: Type::Int(IntSize::Bits32), size: 4, align: 4, ..Default::default() },
        target_lexicon::PointerWidth::U64 => Details { ty: Type::Int(IntSize::Bits64), size: 8, align: 8, ..Default::default() },
    });
    LayoutInterner::set(F32, Details { ty: Type::Float(FloatSize::Bits32), size: 4, align: 4, ..Default::default() });
    LayoutInterner::set(F64, Details { ty: Type::Float(FloatSize::Bits64), size: 8, align: 8, ..Default::default() });
    LayoutInterner::set(FSIZE, match triple.pointer_width().unwrap_or(target_lexicon::PointerWidth::U32) {
        target_lexicon::PointerWidth::U16 => Details { ty: Type::Float(FloatSize::Bits32), size: 4, align: 4, ..Default::default() },
        target_lexicon::PointerWidth::U32 => Details { ty: Type::Float(FloatSize::Bits32), size: 4, align: 4, ..Default::default() },
        target_lexicon::PointerWidth::U64 => Details { ty: Type::Float(FloatSize::Bits64), size: 8, align: 8, ..Default::default() },
    });
    LayoutInterner::set(FN, Details { ty: Type::Proc(Default::default()), size: ptr_size, align: ptr_size, ..Default::default() });
    LayoutInterner::set(PTR_U8, Details { ty: Type::Ref(Box::new(Type::UInt(IntSize::Bits8))), size: ptr_size, align: ptr_size, pointee: Some(U8), ..Default::default() });
}

#[derive(Clone, Copy)]
pub struct Layout(usize);

#[derive(Clone)]
pub struct Details {
    pub ty: Type,
    pub size: usize,
    pub align: usize,
    pub pointee: Option<Layout>,
    pub idx: Option<Layout>,
    pub fields: Box<[(usize, Layout)]>,
}

impl Default for Details {
    fn default() -> Details {
        Details {
            ty: Type::Unit,
            size: 0,
            align: 1,
            pointee: None,
            idx: None,
            fields: Box::new([]),
        }
    }
}

impl Details {
    pub fn sign(&self) -> bool {
        match &self.ty {
            Type::Int(_) => true,
            _ => false,
        }
    }
}

impl Type {
    pub fn layout(&self) -> Layout {
        use crate::{IntSize, FloatSize};

        match self {
            Type::Unit => UNIT,
            Type::Bool => BOOL,
            Type::Char => CHAR,
            Type::Str => STR,
            Type::Ratio => RATIO,
            Type::Int(IntSize::Bits8) => I8,
            Type::Int(IntSize::Bits16) => I16,
            Type::Int(IntSize::Bits32) => I32,
            Type::Int(IntSize::Bits64) => I64,
            Type::Int(IntSize::Bits128) => I128,
            Type::Int(IntSize::Size) => ISIZE,
            Type::UInt(IntSize::Bits8) => U8,
            Type::UInt(IntSize::Bits16) => U16,
            Type::UInt(IntSize::Bits32) => U32,
            Type::UInt(IntSize::Bits64) => U64,
            Type::UInt(IntSize::Bits128) => U128,
            Type::UInt(IntSize::Size) => USIZE,
            Type::Float(FloatSize::Bits32) => F32,
            Type::Float(FloatSize::Bits64) => F64,
            Type::Float(FloatSize::Size) => FSIZE,
            Type::Ref(to) => Details {
                ty: self.clone(),
                size: PTR_U8.details().size,
                align: PTR_U8.details().align,
                pointee: Some(to.layout()),
                ..Default::default()
            }.intern(),
            Type::Array(of, len) => {
                let of_layout= of.layout();

                Details {
                    ty: self.clone(),
                    size: of_layout.details().size * len,
                    align: alignment(of_layout.details().size * len),
                    idx: Some(of_layout),
                    ..Default::default()
                }.intern()
            },
            Type::Slice(of) => {
                let of_layout = of.layout();

                Details {
                    ty: self.clone(),
                    size: PTR_U8.details().size * 2,
                    align: PTR_U8.details().align * 2,
                    idx: Some(of_layout),
                    fields: Box::new([
                        (0, Details { ty: Type::Ref(of.clone()), size: PTR_U8.details().size, align: PTR_U8.details().align, pointee: Some(of_layout), .. Default::default() }.intern()),
                        (PTR_U8.details().size, USIZE),
                    ]),
                    ..Default::default()
                }.intern()
            },
            Type::Vector(of, len) => {
                let of_layout= of.layout();

                Details {
                    ty: self.clone(),
                    size: of_layout.details().size * len,
                    align: alignment(of_layout.details().size * len),
                    idx: Some(of_layout),
                    ..Default::default()
                }.intern()
            },
            Type::Proc(_) => FN,
            Type::Tuple(packed, types) => {
                let mut size = 0;
                let mut fields = Vec::new();

                for ty in types {
                    let layout = ty.layout();

                    if !*packed {
                        let align = layout.details().align;

                        while size % align != 0 {
                            size += 1;
                        }
                    }

                    fields.push((size, layout));
                    size += layout.details().size;
                }

                while !pow2(size) {
                    size += 1;
                }

                Details {
                    ty: self.clone(),
                    size,
                    align: alignment(size),
                    fields: fields.into_boxed_slice(),
                    ..Default::default()
                }.intern()
            },
            Type::Union(tagged, types) => {
                let mut fields = Vec::new();
                let mut size = 0;

                for ty in types {
                    let s = ty.layout().details().size;

                    size = usize::max(size, s);
                }

                if *tagged {
                    let layout = Details {
                        ty: Type::Union(false, types.clone()),
                        size,
                        align: alignment(size),
                        ..Default::default()
                    }.intern();

                    fields.push((0, USIZE));
                    fields.push((USIZE.details().size, layout));
                    size += USIZE.details().size;

                    Details {
                        ty: self.clone(),
                        size,
                        align: alignment(size),
                        fields: fields.into_boxed_slice(),
                        ..Default::default()
                    }.intern()
                } else {
                    Details {
                        ty: self.clone(),
                        size,
                        align: alignment(size),
                        ..Default::default()
                    }.intern()
                }
            },
            Type::Tagged(_tag, _ty) => {
                unimplemented!();
            },
        }
    }
}

fn pow2(x: usize) -> bool {
    (x & (x - 1)) == 0
}

fn alignment(size: usize) -> usize {
    if size % 2 == 1 || size == 0 {
        return 1;
    }

    let mut bytes = 1;

    loop {
        if size % bytes != 0 || bytes > size {
            bytes /= 2;
            break;
        }

        bytes *= 2;
    };

    bytes
}

impl intern::InternKey for Layout {
    fn from_usize(src: usize) -> Layout {
        Layout(src)
    }

    fn into_usize(self) -> usize {
        self.0
    }
}

impl Layout {
    #[deprecated]
    pub fn _details(self) -> intern::Interned<'static, Details> {
        Details::untern(self)
    }

    pub fn details(self) -> Details {
        (&*Details::untern(self)).clone()
    }
}

intern::interner!(LayoutInterner, LAYOUT_INTERNER, Details, Layout);
