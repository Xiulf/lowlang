use super::{Backend, State};
use ir::{
    db::IrDatabase,
    layout::{Fields, TyAndLayout},
    ty::{GenericVar, Ty},
    Flags, Linkage, TypeDef, TypeDefBody,
};
use mangling::mangle;

pub trait FnBuilder<B: Backend> {
    fn ptr_size(&self) -> i32;

    fn param(&self, n: usize) -> B::Value;
    fn type_info(&mut self, ty: Ty, info: B::Value) -> B::Value;

    fn stack_alloc(&mut self, size: u64) -> B::Value;
    fn const_int(&mut self, int: u64) -> B::Value;
    fn fn_addr(&mut self, id: B::FuncId) -> B::Value;

    fn load(&mut self, ptr: B::Value, offset: i32) -> B::Value;
    fn store(&mut self, ptr: B::Value, offset: i32, value: B::Value);

    fn add(&mut self, a: B::Value, b: B::Value) -> B::Value;
    fn mul(&mut self, a: B::Value, b: B::Value) -> B::Value;

    fn offset(&mut self, ptr: B::Value, offset: B::Value) -> B::Value;
    fn offset_u64(&mut self, ptr: B::Value, offset: u64) -> B::Value;

    fn memcopy(&mut self, dst: B::Value, src: B::Value, bytes: u64);
    fn memmove(&mut self, dst: B::Value, src: B::Value, bytes: u64);

    fn gt(&mut self, a: B::Value, b: B::Value) -> B::Value;
    fn conditional(&mut self, condition: B::Value, a: B::Value, b: B::Value) -> B::Value;

    fn call(&mut self, fn_ptr: B::Value, args: &[B::Value]);
    fn ret(&mut self);
}

impl<B: Backend> State<B> {
    pub(super) fn generate_def_copy(
        &mut self,
        backend: &mut B,
        db: &dyn IrDatabase,
        module: &str,
        linkage: Linkage,
        def: &TypeDef,
        _layout: &TyAndLayout,
    ) -> B::FuncId {
        let name = mangle(format!("{}#{}.copy", module, def.name).as_bytes());
        let export = linkage == Linkage::Export;
        let body = def.body.as_ref().expect("Opaque types cannot be copied");

        backend.mk_fn(&name, export, 3, |fx| {
            let ptr_size = fx.ptr_size();
            let dst = fx.param(0);
            let src = fx.param(1);
            let info = fx.param(2);
            let offsets = (2 + def.generic_params.len()) as i32 * ptr_size;

            match body {
                | TypeDefBody::Struct { fields } => {
                    for (i, field) in fields.iter().enumerate() {
                        let offset = fx.load(info.clone(), offsets + i as i32 * ptr_size);
                        let dst = fx.offset(dst.clone(), offset.clone());
                        let src = fx.offset(src.clone(), offset);

                        if field.ty.lookup(db).flags.is_set(Flags::TRIVIAL) {
                            let layout = db.layout_of(field.ty);

                            fx.memcopy(dst, src, layout.size.bytes());
                        } else {
                            let info = fx.type_info(field.ty, info.clone());
                            let vwt = fx.load(info.clone(), 0);
                            let copy_fn = fx.load(vwt, 3 * ptr_size);

                            fx.call(copy_fn, &[dst, src, info]);
                        }
                    }

                    fx.ret();
                },
                | TypeDefBody::Enum { variants: _ } => todo!(),
                | TypeDefBody::Union { .. } => unreachable!("Unions must be trivial types"),
            }
        })
    }

    pub(super) fn generate_def_move(
        &mut self,
        backend: &mut B,
        db: &dyn IrDatabase,
        module: &str,
        linkage: Linkage,
        def: &TypeDef,
        _layout: &TyAndLayout,
    ) -> B::FuncId {
        let name = mangle(format!("{}#{}.move", module, def.name).as_bytes());
        let export = linkage == Linkage::Export;
        let body = def.body.as_ref().expect("Opaque types cannot be moved");

        backend.mk_fn(&name, export, 3, |fx| {
            let ptr_size = fx.ptr_size();
            let dst = fx.param(0);
            let src = fx.param(1);
            let info = fx.param(2);
            let offsets = (2 + def.generic_params.len()) as i32 * ptr_size;

            match body {
                | TypeDefBody::Struct { fields } => {
                    for (i, field) in fields.iter().enumerate() {
                        let offset = fx.load(info.clone(), offsets + i as i32 * ptr_size);
                        let dst = fx.offset(dst.clone(), offset.clone());
                        let src = fx.offset(src.clone(), offset);

                        if field.ty.lookup(db).flags.is_set(Flags::TRIVIAL) {
                            let layout = db.layout_of(field.ty);

                            fx.memcopy(dst, src, layout.size.bytes());
                        } else {
                            let info = fx.type_info(field.ty, info.clone());
                            let vwt = fx.load(info.clone(), 0);
                            let move_fn = fx.load(vwt, 4 * ptr_size);

                            fx.call(move_fn, &[dst, src, info]);
                        }
                    }

                    fx.ret();
                },
                | TypeDefBody::Enum { variants: _ } => todo!(),
                | TypeDefBody::Union { .. } => unreachable!("Unions must be trivial types"),
            }
        })
    }

    pub(super) fn generate_def_drop(
        &mut self,
        backend: &mut B,
        db: &dyn IrDatabase,
        module: &str,
        linkage: Linkage,
        def: &TypeDef,
        _layout: &TyAndLayout,
    ) -> B::FuncId {
        let name = mangle(format!("{}#{}.drop", module, def.name).as_bytes());
        let export = linkage == Linkage::Export;
        let body = def.body.as_ref().expect("Opaque types cannot be moved");

        backend.mk_fn(&name, export, 2, |fx| {
            let ptr_size = fx.ptr_size();
            let val = fx.param(0);
            let info = fx.param(1);
            let offsets = (2 + def.generic_params.len()) as i32 * ptr_size;

            match body {
                | TypeDefBody::Struct { fields } => {
                    for (i, field) in fields.iter().enumerate() {
                        if !field.ty.lookup(db).flags.is_set(Flags::TRIVIAL) {
                            let offset = fx.load(info.clone(), offsets + i as i32 * ptr_size);
                            let val = fx.offset(val.clone(), offset);
                            let info = fx.type_info(field.ty, info.clone());
                            let vwt = fx.load(info.clone(), 0);
                            let drop_fn = fx.load(vwt, 4 * ptr_size);

                            fx.call(drop_fn, &[val, info]);
                        }
                    }

                    fx.ret();
                },
                | TypeDefBody::Enum { variants: _ } => todo!(),
                | TypeDefBody::Union { .. } => unreachable!("Unions must be trivial types"),
            }
        })
    }

    pub(super) fn generate_mk_generics(
        &mut self,
        backend: &mut B,
        _db: &dyn IrDatabase,
        module: &str,
        linkage: Linkage,
        def: &TypeDef,
        _layout: &TyAndLayout,
    ) -> B::FuncId {
        let name = mangle(format!("{}#{}.generics", module, def.name).as_bytes());
        let export = linkage == Linkage::Export;

        backend.mk_fn(&name, export, def.generic_params.len() + 1, |fx| {
            let generics = fx.param(0);
            let ptr_size = fx.ptr_size();

            for i in 0..def.generic_params.len() {
                let param = fx.param(i + 1);

                fx.store(generics.clone(), i as i32 * ptr_size, param);
            }

            fx.ret();
        })
    }

    pub(super) fn generate_mk_vwt(
        &mut self,
        backend: &mut B,
        db: &dyn IrDatabase,
        module: &str,
        linkage: Linkage,
        def: &TypeDef,
        layout: &TyAndLayout,
        copy_fn: B::FuncId,
        move_fn: B::FuncId,
        drop_fn: B::FuncId,
    ) -> B::FuncId {
        let name = mangle(format!("{}#{}.vwt", module, def.name).as_bytes());
        let export = linkage == Linkage::Export;
        let body = def.body.as_ref().expect("Opaque types cannot be dropped");

        backend.mk_fn(&name, export, 2, |fx| {
            let ptr_size = fx.ptr_size();
            let vwt = fx.param(0);
            let generics = fx.param(1);
            let mut size = fx.const_int(layout.size.bytes());
            let mut align = fx.const_int(layout.align.bytes());
            let mut stride = fx.const_int(layout.stride.bytes());
            let copy_fn = fx.fn_addr(copy_fn);
            let move_fn = fx.fn_addr(move_fn);
            let drop_fn = fx.fn_addr(drop_fn);

            match body {
                | TypeDefBody::Struct { fields } => {
                    for i in 0..def.generic_params.len() {
                        let param = fx.load(generics.clone(), i as i32 * ptr_size);
                        let param_vwt = fx.load(param, 0);
                        let param_align = fx.load(param_vwt.clone(), 1 * ptr_size);
                        let param_stride = fx.load(param_vwt.clone(), 2 * ptr_size);
                        let count = fields.iter().map(|f| f.ty.var_count(db, GenericVar(0, i as u8))).sum::<u64>();

                        if count > 1 {
                            let count = fx.const_int(count);
                            let added = fx.mul(param_stride, count);

                            size = fx.add(size, added.clone());
                            stride = fx.add(stride, added);
                        } else if count == 1 {
                            size = fx.add(size, param_stride.clone());
                            stride = fx.add(stride, param_stride);
                        }

                        let cond = fx.gt(param_align.clone(), align.clone());

                        align = fx.conditional(cond, param_align, align);
                    }
                },
                | TypeDefBody::Union { fields: _ } => {},
                | TypeDefBody::Enum { variants: _ } => {},
            }

            fx.store(vwt.clone(), 0 * ptr_size, size);
            fx.store(vwt.clone(), 1 * ptr_size, align);
            fx.store(vwt.clone(), 2 * ptr_size, stride);
            fx.store(vwt.clone(), 3 * ptr_size, copy_fn);
            fx.store(vwt.clone(), 4 * ptr_size, move_fn);
            fx.store(vwt.clone(), 5 * ptr_size, drop_fn);

            fx.ret();
        })
    }

    pub(super) fn generate_mk_info(
        &mut self,
        backend: &mut B,
        db: &dyn IrDatabase,
        module: &str,
        linkage: Linkage,
        def: &TypeDef,
        layout: &TyAndLayout,
    ) -> B::FuncId {
        let name = mangle(format!("{}#{}.info", module, def.name).as_bytes());
        let export = linkage == Linkage::Export;
        let body = def.body.as_ref().expect("Opaque types don't have type info");

        backend.mk_fn(&name, export, 3, |fx| {
            let ptr_size = fx.ptr_size();
            let info = fx.param(0);
            let vwt = fx.param(1);
            let generics = fx.param(2);
            let flags = fx.const_int(0);

            fx.store(info.clone(), 0, vwt);
            fx.store(info.clone(), ptr_size, flags);

            let generics_start = fx.offset_u64(info.clone(), ptr_size as u64 * 2);

            fx.memcopy(generics_start, generics, ptr_size as u64 * def.generic_params.len() as u64);

            let fields_start = (2 + def.generic_params.len() as i32) * ptr_size;

            match body {
                | TypeDefBody::Struct { fields } => match &layout.fields {
                    | Fields::Arbitrary { offsets, memory_index } => {
                        let inverse_index = ir::layout::invert_mapping(memory_index);
                        let mut offset = fx.const_int(0);
                        let mut dyn_offset = fx.const_int(0);

                        for (i, j) in inverse_index.into_iter().enumerate() {
                            let o = fx.add(offset, dyn_offset.clone());

                            fx.store(info.clone(), fields_start + j as i32 * ptr_size, o);
                            offset = fx.const_int(offsets[i].bytes());

                            for g in 0..def.generic_params.len() {
                                let count = fields[j].ty.var_count(db, GenericVar(0, g as u8));

                                if count > 0 {
                                    let param = fx.load(info.clone(), (2 + i as i32) * ptr_size);
                                    let vwt = fx.load(param, 0);
                                    let stride = fx.load(vwt, 2 * ptr_size);

                                    if count > 1 {
                                        let n = fx.const_int(count);
                                        let stride = fx.mul(n, stride);

                                        dyn_offset = fx.add(dyn_offset, stride);
                                    } else {
                                        dyn_offset = fx.add(dyn_offset, stride);
                                    }
                                }
                            }
                        }
                    },
                    | _ => unreachable!(),
                },
                | TypeDefBody::Enum { variants: _ } => todo!(),
                | TypeDefBody::Union { .. } => {},
            }

            fx.ret();
        })
    }
}
