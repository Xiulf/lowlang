pub use crate::{Type, Ty};
use intern::Intern;

pub struct LayoutCtx<'t, 'l> {
    pub defaults: DefaultLayouts<'t, 'l>,
    interner: &'l LayoutInterner<'t, 'l>,
    types: &'t crate::ty::TyCtx<'t>,
}

pub struct DefaultLayouts<'t, 'l> {
    pub unit: TyLayout<'t, 'l>,
    pub bool: TyLayout<'t, 'l>,
    pub char: TyLayout<'t, 'l>,
    pub str: TyLayout<'t, 'l>,
    pub ratio: TyLayout<'t, 'l>,
    pub u8: TyLayout<'t, 'l>,
    pub u16: TyLayout<'t, 'l>,
    pub u32: TyLayout<'t, 'l>,
    pub u64: TyLayout<'t, 'l>,
    pub u128: TyLayout<'t, 'l>,
    pub usize: TyLayout<'t, 'l>,
    pub i8: TyLayout<'t, 'l>,
    pub i16: TyLayout<'t, 'l>,
    pub i32: TyLayout<'t, 'l>,
    pub i64: TyLayout<'t, 'l>,
    pub i128: TyLayout<'t, 'l>,
    pub isize: TyLayout<'t, 'l>,
    pub f32: TyLayout<'t, 'l>,
    pub f64: TyLayout<'t, 'l>,
    pub fsize: TyLayout<'t, 'l>,
    pub proc: TyLayout<'t, 'l>,
    pub ptr_u8: TyLayout<'t, 'l>,
}

impl<'t, 'l> LayoutCtx<'t, 'l> {
    pub fn new(types: &'t crate::ty::TyCtx<'t>, interner: &'l LayoutInterner<'t, 'l>, triple: &target_lexicon::Triple) -> LayoutCtx<'t, 'l> {
        use target_lexicon::PointerWidth;
        let ptr_width = triple.pointer_width().unwrap_or(PointerWidth::U32);
        let ptr_size = ptr_width.bytes() as usize;
        
        let u8 = TyLayout { ty: types.defaults.u8, details: Details { size: 1, align: 1, ..Details::default() }.intern(interner) };
        let u16 = TyLayout { ty: types.defaults.u16, details: Details { size: 2, align: 2, ..Details::default() }.intern(interner) };
        let u32 = TyLayout { ty: types.defaults.u32, details: Details { size: 4, align: 4, ..Details::default() }.intern(interner) };
        let u64 = TyLayout { ty: types.defaults.u64, details: Details { size: 8, align: 8, ..Details::default() }.intern(interner) };
        let usize = match ptr_width {
            PointerWidth::U16 => u16,
            PointerWidth::U32 => u32,
            PointerWidth::U64 => u64,
        };
        
        let i16 = TyLayout { ty: types.defaults.i16, details: Details { size: 2, align: 2, ..Details::default() }.intern(interner) };
        let i32 = TyLayout { ty: types.defaults.i32, details: Details { size: 4, align: 4, ..Details::default() }.intern(interner) };
        let i64 = TyLayout { ty: types.defaults.i64, details: Details { size: 8, align: 8, ..Details::default() }.intern(interner) };
        let isize = match ptr_width {
            PointerWidth::U16 => i16,
            PointerWidth::U32 => i32,
            PointerWidth::U64 => i64,
        };
        
        let f32 = TyLayout { ty: types.defaults.f32, details: Details { size: 4, align: 4, ..Details::default() }.intern(interner) };
        let f64 = TyLayout { ty: types.defaults.f64, details: Details { size: 8, align: 8, ..Details::default() }.intern(interner) };
        
        let ptr_u8 = TyLayout { ty: types.defaults.ptr_u8, details: Details {
            size: ptr_size,
            align: alignment(ptr_size),
            pointee: Some(u8),
            ..Details::default()
        }.intern(interner) };
        
        LayoutCtx {
            defaults: DefaultLayouts {
                unit: TyLayout { ty: types.defaults.unit, details: Details::default().intern(interner) },
                bool: TyLayout { ty: types.defaults.bool, details: Details { size: 1, align: 1, ..Details::default() }.intern(interner) },
                char: TyLayout { ty: types.defaults.char, details: Details { size: 4, align: 4, ..Details::default() }.intern(interner) },
                str: TyLayout { ty: types.defaults.str, details: Details {
                    size: 2 * ptr_size,
                    align: alignment(2 * ptr_size),
                    idx: Some(u8),
                    fields: Box::new([
                        (0, ptr_u8),
                        (ptr_size, usize),
                    ]),
                    ..Details::default()
                }.intern(interner) },
                ratio: TyLayout { ty: types.defaults.ratio, details: Details {
                    size: 2 * isize.details.size,
                    align: 2 * isize.details.align,
                    fields: Box::new([
                        (0, isize),
                        (isize.details.size, isize),
                    ]),
                    ..Details::default()
                }.intern(interner) },
                u8, u16, u32, u64,
                u128: TyLayout { ty: types.defaults.u128, details: Details::default().intern(interner) },
                usize,
                i8: TyLayout { ty: types.defaults.i8, details: Details::default().intern(interner) },
                i16, i32, i64,
                i128: TyLayout { ty: types.defaults.i128, details: Details::default().intern(interner) },
                isize, f32, f64,
                fsize: match ptr_width {
                    PointerWidth::U16 => f32,
                    PointerWidth::U32 => f32,
                    PointerWidth::U64 => f64,
                },
                proc: TyLayout { ty: Type::Proc(Default::default()).intern(types), details: Details {
                    size: ptr_size,
                    align: alignment(ptr_size),
                    ..Details::default()
                }.intern(interner) },
                ptr_u8,
            },
            interner,
            types,
        }
    }
}

#[derive(Clone, Copy)]
pub struct TyLayout<'t, 'l> {
    pub ty: Ty<'t>,
    pub details: Layout<'t, 'l>,
}

pub struct Details<'t, 'l> {
    pub size: usize,
    pub align: usize,
    pub pointee: Option<TyLayout<'t, 'l>>,
    pub idx: Option<TyLayout<'t, 'l>>,
    pub fields: Box<[(usize, TyLayout<'t, 'l>)]>,
}

impl<'t, 'l> Default for Details<'t, 'l> {
    fn default() -> Details<'t, 'l> {
        Details {
            size: 0,
            align: 1,
            pointee: None,
            idx: None,
            fields: Box::new([]),
        }
    }
}

impl<'t, 'l> TyLayout<'t, 'l> {
    pub fn sign(&self) -> bool {
        match &*self.ty {
            Type::Int(_) => true,
            _ => false,
        }
    }
}

impl<'t, 'l> Ty<'t> {
    pub fn layout(self, ctx: &LayoutCtx<'t, 'l>) -> TyLayout<'t, 'l> {
        use crate::{IntSize, FloatSize};

        TyLayout {
            ty: self,
            details: match &*self {
                Type::Param(_) => unreachable!(),
                Type::Unit => ctx.defaults.unit.details,
                Type::Bool => ctx.defaults.bool.details,
                Type::Char => ctx.defaults.char.details,
                Type::Str => ctx.defaults.str.details,
                Type::Ratio => ctx.defaults.ratio.details,
                Type::Int(IntSize::Bits8) => ctx.defaults.i8.details,
                Type::Int(IntSize::Bits16) => ctx.defaults.i16.details,
                Type::Int(IntSize::Bits32) => ctx.defaults.i32.details,
                Type::Int(IntSize::Bits64) => ctx.defaults.i64.details,
                Type::Int(IntSize::Bits128) => ctx.defaults.i128.details,
                Type::Int(IntSize::Size) => ctx.defaults.isize.details,
                Type::UInt(IntSize::Bits8) => ctx.defaults.u8.details,
                Type::UInt(IntSize::Bits16) => ctx.defaults.u16.details,
                Type::UInt(IntSize::Bits32) => ctx.defaults.u32.details,
                Type::UInt(IntSize::Bits64) => ctx.defaults.u64.details,
                Type::UInt(IntSize::Bits128) => ctx.defaults.u128.details,
                Type::UInt(IntSize::Size) => ctx.defaults.usize.details,
                Type::Float(FloatSize::Bits32) => ctx.defaults.f32.details,
                Type::Float(FloatSize::Bits64) => ctx.defaults.f64.details,
                Type::Float(FloatSize::Size) => ctx.defaults.fsize.details,
                Type::Ref(to) => Details {
                    size: ctx.defaults.ptr_u8.details.size,
                    align: ctx.defaults.ptr_u8.details.align,
                    pointee: Some(to.layout(ctx)),
                    ..Default::default()
                }.intern(ctx.interner),
                Type::Array(of, len) => {
                    let of_layout= of.layout(ctx);

                    Details {
                        size: of_layout.details.size * len,
                        align: alignment(of_layout.details.size * len),
                        idx: Some(of_layout),
                        ..Default::default()
                    }.intern(ctx.interner)
                },
                Type::Slice(of) => {
                    let of_layout = of.layout(ctx);

                    Details {
                        size: ctx.defaults.ptr_u8.details.size * 2,
                        align: ctx.defaults.ptr_u8.details.align * 2,
                        idx: Some(of_layout),
                        fields: Box::new([
                            (0, TyLayout {
                                ty: Type::Ref(*of).intern(ctx.types),
                                details: Details {
                                    size: ctx.defaults.ptr_u8.details.size,
                                    align: ctx.defaults.ptr_u8.details.align,
                                    pointee: Some(of_layout),
                                    .. Default::default()
                                }.intern(ctx.interner)
                            }),
                            (ctx.defaults.ptr_u8.details.size, ctx.defaults.usize),
                        ]),
                        ..Default::default()
                    }.intern(ctx.interner)
                },
                Type::Vector(of, len) => {
                    let of_layout= of.layout(ctx);

                    Details {
                        size: of_layout.details.size * len,
                        align: alignment(of_layout.details.size * len),
                        idx: Some(of_layout),
                        ..Default::default()
                    }.intern(ctx.interner)
                },
                Type::Proc(_) => ctx.defaults.proc.details,
                Type::Tuple(packed, types) => {
                    let mut size = 0;
                    let mut fields = Vec::new();

                    for ty in types {
                        let layout = ty.layout(ctx);

                        if !*packed {
                            let align = layout.details.align;

                            while size % align != 0 {
                                size += 1;
                            }
                        }

                        fields.push((size, layout));
                        size += layout.details.size;
                    }

                    while !pow2(size) {
                        size += 1;
                    }

                    Details {
                        size,
                        align: alignment(size),
                        fields: fields.into_boxed_slice(),
                        ..Default::default()
                    }.intern(ctx.interner)
                },
                Type::Union(tagged, types) => {
                    let mut fields = Vec::new();
                    let mut size = 0;

                    for ty in types {
                        let s = ty.layout(ctx).details.size;

                        size = usize::max(size, s);
                    }

                    if *tagged {
                        let layout = TyLayout {
                            ty: Type::Union(false, types.clone()).intern(ctx.types),
                            details: Details {
                                size,
                                align: alignment(size),
                                ..Default::default()
                            }.intern(ctx.interner)
                        };

                        fields.push((0, ctx.defaults.usize));
                        fields.push((ctx.defaults.usize.details.size, layout));
                        size += ctx.defaults.usize.details.size;

                        Details {
                            size,
                            align: alignment(size),
                            fields: fields.into_boxed_slice(),
                            ..Default::default()
                        }.intern(ctx.interner)
                    } else {
                        Details {
                            size,
                            align: alignment(size),
                            ..Default::default()
                        }.intern(ctx.interner)
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

pub struct LayoutInterner<'t, 'l> {
    storage: intern::typed_arena::Arena<Details<'t, 'l>>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Layout<'t, 'l>(*mut Details<'t, 'l>, ::std::marker::PhantomData<&'l mut Details<'t, 'l>>);

impl<'t, 'l> LayoutInterner<'t, 'l> {
    pub fn new() -> LayoutInterner<'t, 'l> {
        LayoutInterner {
            storage: intern::typed_arena::Arena::new(),
        }
    }
}

impl<'t, 'l> intern::Intern<'l> for Details<'t, 'l> {
    type Key = Layout<'t, 'l>;
    
    fn intern<I: intern::Interner<'l, Self> + 'l>(self, i: &I) -> Layout<'t, 'l> {
        i.intern(self)
    }
}

impl<'t, 'l> intern::Interner<'l, Details<'t, 'l>> for LayoutInterner<'t, 'l> {
    fn intern(&self, value: Details<'t, 'l>) -> Layout<'t, 'l> {
        Layout(self.storage.alloc(value), ::std::marker::PhantomData)
    }
}

impl<'t, 'l> ::std::default::Default for Layout<'t, 'l> {
    fn default() -> Layout<'t, 'l> {
        Layout(::std::ptr::null_mut(), ::std::marker::PhantomData)
    }
}

impl<'t, 'l> ::std::ops::Deref for Layout<'t, 'l> {
    type Target = Details<'t, 'l>;
    
    fn deref(&self) -> &Details<'t, 'l> {
        unsafe { &*self.0 }
    }
}

impl<'t, 'l> ::std::ops::DerefMut for Layout<'t, 'l> {
    fn deref_mut(&mut self) -> &mut Details<'t, 'l> {
        unsafe { &mut *self.0 }
    }
}
