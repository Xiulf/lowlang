pub use crate::{Type, Ty};
use intern::Intern;

pub struct LayoutCtx<'l> {
    pub default_layouts: DefaultLayouts<'l>,
    interner: LayoutInterner,
    types: &'l crate::ty::TypeInterner,
}

pub struct DefaultLayouts<'l> {
    pub unit: TyLayout<'l>,
    pub bool: TyLayout<'l>,
    pub char: TyLayout<'l>,
    pub str: TyLayout<'l>,
    pub ratio: TyLayout<'l>,
    pub u8: TyLayout<'l>,
    pub u16: TyLayout<'l>,
    pub u32: TyLayout<'l>,
    pub u64: TyLayout<'l>,
    pub u128: TyLayout<'l>,
    pub usize: TyLayout<'l>,
    pub i8: TyLayout<'l>,
    pub i16: TyLayout<'l>,
    pub i32: TyLayout<'l>,
    pub i64: TyLayout<'l>,
    pub i128: TyLayout<'l>,
    pub isize: TyLayout<'l>,
    pub f32: TyLayout<'l>,
    pub f64: TyLayout<'l>,
    pub fsize: TyLayout<'l>,
    pub proc: TyLayout<'l>,
    pub ptr_u8: TyLayout<'l>,
}

/// # Important
/// This should be called before any layouts are created!
pub fn init(triple: &target_lexicon::Triple) {
    use crate::{IntSize, FloatSize};

    let ptr_size = triple.pointer_width().map(target_lexicon::PointerWidth::bytes).unwrap_or(4) as usize;

    LayoutInterner::set(UNIT, Details { ty: Type::Unit.intern(), size: 0, align: 1, pointee: None, idx: None, fields: Box::new([]) });
    LayoutInterner::set(BOOL, Details { ty: Type::Bool.intern(), size: 1, .. Default::default() });
    LayoutInterner::set(CHAR, Details { ty: Type::Char.intern(), size: 4, align: 4, ..Default::default() });
    LayoutInterner::set(STR, Details { ty: Type::Str.intern(), size: ptr_size * 2, align: ptr_size * 2, idx: Some(U8), fields: Box::new([(0, PTR_U8), (ptr_size, USIZE)]), ..Default::default() });
    LayoutInterner::set(RATIO, Details { ty: Type::Ratio.intern(), size: ptr_size * 2, align: ptr_size * 2, fields: Box::new([(0, ISIZE), (ptr_size, ISIZE)]), ..Default::default() });
    LayoutInterner::set(U8, Details { ty: Type::UInt(IntSize::Bits8).intern(), size: 1, ..Default::default() });
    LayoutInterner::set(U16, Details { ty: Type::UInt(IntSize::Bits16).intern(), size: 2, align: 2, ..Default::default() });
    LayoutInterner::set(U32, Details { ty: Type::UInt(IntSize::Bits32).intern(), size: 4, align: 4, ..Default::default() });
    LayoutInterner::set(U64, Details { ty: Type::UInt(IntSize::Bits64).intern(), size: 8, align: 8, ..Default::default() });
    LayoutInterner::set(U128, Details { ty: Type::UInt(IntSize::Bits128).intern(), size: 16, align: 16, ..Default::default() });
    LayoutInterner::set(USIZE, match triple.pointer_width().unwrap_or(target_lexicon::PointerWidth::U32) {
        target_lexicon::PointerWidth::U16 => Details { ty: Type::UInt(IntSize::Bits16).intern(), size: 2, align: 2, ..Default::default() },
        target_lexicon::PointerWidth::U32 => Details { ty: Type::UInt(IntSize::Bits32).intern(), size: 4, align: 4, ..Default::default() },
        target_lexicon::PointerWidth::U64 => Details { ty: Type::UInt(IntSize::Bits64).intern(), size: 8, align: 8, ..Default::default() },
    });
    LayoutInterner::set(I8, Details { ty: Type::Int(IntSize::Bits8).intern(), size: 1, ..Default::default() });
    LayoutInterner::set(I16, Details { ty: Type::Int(IntSize::Bits16).intern(), size: 2, align: 2, ..Default::default() });
    LayoutInterner::set(I32, Details { ty: Type::Int(IntSize::Bits32).intern(), size: 4, align: 4, ..Default::default() });
    LayoutInterner::set(I64, Details { ty: Type::Int(IntSize::Bits64).intern(), size: 8, align: 8, ..Default::default() });
    LayoutInterner::set(I128, Details { ty: Type::Int(IntSize::Bits128).intern(), size: 16, align: 16, ..Default::default() });
    LayoutInterner::set(ISIZE, match triple.pointer_width().unwrap_or(target_lexicon::PointerWidth::U32) {
        target_lexicon::PointerWidth::U16 => Details { ty: Type::Int(IntSize::Bits16).intern(), size: 2, align: 2, ..Default::default() },
        target_lexicon::PointerWidth::U32 => Details { ty: Type::Int(IntSize::Bits32).intern(), size: 4, align: 4, ..Default::default() },
        target_lexicon::PointerWidth::U64 => Details { ty: Type::Int(IntSize::Bits64).intern(), size: 8, align: 8, ..Default::default() },
    });
    LayoutInterner::set(F32, Details { ty: Type::Float(FloatSize::Bits32).intern(), size: 4, align: 4, ..Default::default() });
    LayoutInterner::set(F64, Details { ty: Type::Float(FloatSize::Bits64).intern(), size: 8, align: 8, ..Default::default() });
    LayoutInterner::set(FSIZE, match triple.pointer_width().unwrap_or(target_lexicon::PointerWidth::U32) {
        target_lexicon::PointerWidth::U16 => Details { ty: Type::Float(FloatSize::Bits32).intern(), size: 4, align: 4, ..Default::default() },
        target_lexicon::PointerWidth::U32 => Details { ty: Type::Float(FloatSize::Bits32).intern(), size: 4, align: 4, ..Default::default() },
        target_lexicon::PointerWidth::U64 => Details { ty: Type::Float(FloatSize::Bits64).intern(), size: 8, align: 8, ..Default::default() },
    });
    LayoutInterner::set(FN, Details { ty: Type::Proc(crate::Signature::default()).intern(), size: ptr_size, align: ptr_size, ..Default::default() });
    LayoutInterner::set(PTR_U8, Details { ty: Type::Ref(U8.details().ty).intern(), size: ptr_size, align: ptr_size, pointee: Some(U8), ..Default::default() });
}

pub struct TyLayout<'t> {
    pub ty: Ty<'t>,
    pub layout: Layout<'t>,
}

pub struct Details<'l> {
    pub size: usize,
    pub align: usize,
    pub pointee: Option<TyLayout<'l>>,
    pub idx: Option<TyLayout<'l>>,
    pub fields: Box<[(usize, TyLayout<'l>)]>,
}

impl<'l> Default for Details<'l> {
    fn default() -> Details<'l> {
        Details {
            size: 0,
            align: 1,
            pointee: None,
            idx: None,
            fields: Box::new([]),
        }
    }
}

impl<'l> TyLayout<'l> {
    pub fn sign(&self) -> bool {
        match &*self.ty {
            Type::Int(_) => true,
            _ => false,
        }
    }
}

impl<'t> Ty<'t> {
    pub fn layout(self, ctx: &'t LayoutCtx<'t>) -> TyLayout<'t> {
        use crate::{IntSize, FloatSize};

        TyLayout {
            ty: self,
            layout: match &*self {
                Type::Unit => ctx.default_layouts.unit.layout,
                Type::Bool => ctx.default_layouts.bool.layout,
                Type::Char => ctx.default_layouts.char.layout,
                Type::Str => ctx.default_layouts.str.layout,
                Type::Ratio => ctx.default_layouts.ratio.layout,
                Type::Int(IntSize::Bits8) => ctx.default_layouts.i8.layout,
                Type::Int(IntSize::Bits16) => ctx.default_layouts.i16.layout,
                Type::Int(IntSize::Bits32) => ctx.default_layouts.i32.layout,
                Type::Int(IntSize::Bits64) => ctx.default_layouts.i64.layout,
                Type::Int(IntSize::Bits128) => ctx.default_layouts.i128.layout,
                Type::Int(IntSize::Size) => ctx.default_layouts.isize.layout,
                Type::UInt(IntSize::Bits8) => ctx.default_layouts.u8.layout,
                Type::UInt(IntSize::Bits16) => ctx.default_layouts.u16.layout,
                Type::UInt(IntSize::Bits32) => ctx.default_layouts.u32.layout,
                Type::UInt(IntSize::Bits64) => ctx.default_layouts.u64.layout,
                Type::UInt(IntSize::Bits128) => ctx.default_layouts.u128.layout,
                Type::UInt(IntSize::Size) => ctx.default_layouts.usize.layout,
                Type::Float(FloatSize::Bits32) => ctx.default_layouts.f32.layout,
                Type::Float(FloatSize::Bits64) => ctx.default_layouts.f64.layout,
                Type::Float(FloatSize::Size) => ctx.default_layouts.fsize.layout,
                Type::Ref(to) => Details {
                    size: ctx.default_layouts.ptr_u8.layout.size,
                    align: ctx.default_layouts.ptr_u8.layout.align,
                    pointee: Some(to.layout(ctx)),
                    ..Default::default()
                }.intern(&ctx.interner),
                Type::Array(of, len) => {
                    let of_layout= of.layout(ctx);

                    Details {
                        size: of_layout.layout.size * len,
                        align: alignment(of_layout.layout.size * len),
                        idx: Some(of_layout),
                        ..Default::default()
                    }.intern(&ctx.interner)
                },
                Type::Slice(of) => {
                    let of_layout = of.layout(ctx);

                    Details {
                        size: ctx.default_layouts.ptr_u8.layout.size * 2,
                        align: ctx.default_layouts.ptr_u8.layout.align * 2,
                        idx: Some(of_layout),
                        fields: Box::new([
                            (0, TyLayout {
                                ty: Type::Ref(*of).intern(ctx.types),
                                layout: Details {
                                    size: ctx.default_layouts.ptr_u8.layout.size,
                                    align: ctx.default_layouts.ptr_u8.layout.align,
                                    pointee: Some(of_layout),
                                    .. Default::default()
                                }.intern(&ctx.interner)
                            }),
                            (ctx.default_layouts.ptr_u8.layout.size, ctx.default_layouts.usize),
                        ]),
                        ..Default::default()
                    }.intern(&ctx.interner)
                },
                Type::Vector(of, len) => {
                    let of_layout= of.layout(ctx);

                    Details {
                        size: of_layout.layout.size * len,
                        align: alignment(of_layout.layout.size * len),
                        idx: Some(of_layout),
                        ..Default::default()
                    }.intern(&ctx.interner)
                },
                Type::Proc(_) => ctx.default_layouts.proc.layout,
                Type::Tuple(packed, types) => {
                    let mut size = 0;
                    let mut fields = Vec::new();

                    for ty in types {
                        let layout = ty.layout(ctx);

                        if !*packed {
                            let align = layout.layout.align;

                            while size % align != 0 {
                                size += 1;
                            }
                        }

                        fields.push((size, layout));
                        size += layout.layout.size;
                    }

                    while !pow2(size) {
                        size += 1;
                    }

                    Details {
                        size,
                        align: alignment(size),
                        fields: fields.into_boxed_slice(),
                        ..Default::default()
                    }.intern(&ctx.interner)
                },
                Type::Union(tagged, types) => {
                    let mut fields = Vec::new();
                    let mut size = 0;

                    for ty in types {
                        let s = ty.layout(ctx).layout.size;

                        size = usize::max(size, s);
                    }

                    if *tagged {
                        let layout = TyLayout {
                            ty: Type::Union(false, types.clone()).intern(ctx.types),
                            layout: Details {
                                size,
                                align: alignment(size),
                                ..Default::default()
                            }.intern(&ctx.interner)
                        };

                        fields.push((0, ctx.default_layouts.usize));
                        fields.push((ctx.default_layouts.usize.layout.size, layout));
                        size += ctx.default_layouts.usize.layout.size;

                        Details {
                            size,
                            align: alignment(size),
                            fields: fields.into_boxed_slice(),
                            ..Default::default()
                        }.intern(&ctx.interner)
                    } else {
                        Details {
                            size,
                            align: alignment(size),
                            ..Default::default()
                        }.intern(&ctx.interner)
                    }
                },
                Type::Tagged(_tag, _ty) => {
                    unimplemented!();
                },
            }
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

intern::interner!(LayoutInterner, Details<'l>, Layout<'l>);
