use crate::CodegenCtx;
use inkwell::types::*;
use inkwell::AddressSpace;
use ir::layout::Primitive;
use ir::ty::*;

pub trait AsBasicType {
    fn as_basic_type<'ctx>(&self, ctx: &CodegenCtx<'ctx>) -> BasicTypeEnum<'ctx>;
}

pub trait AsFuncType {
    fn as_func_type<'ctx>(&self, ctx: &CodegenCtx<'ctx>) -> FunctionType<'ctx>;
}

impl AsBasicType for Ty {
    fn as_basic_type<'ctx>(&self, ctx: &CodegenCtx<'ctx>) -> BasicTypeEnum<'ctx> {
        match &self.kind {
            | typ::Unit => match self.repr.scalar {
                | Some(prim) => match prim {
                    | Primitive::Int(Integer::I8, _) => ctx.context.i8_type().into(),
                    | Primitive::Int(Integer::I16, _) => ctx.context.i16_type().into(),
                    | Primitive::Int(Integer::I32, _) => ctx.context.i32_type().into(),
                    | Primitive::Int(Integer::I64, _) => ctx.context.i64_type().into(),
                    | Primitive::Int(Integer::I128, _) => ctx.context.i128_type().into(),
                    | Primitive::F32 => ctx.context.f32_type().into(),
                    | Primitive::F64 => ctx.context.f64_type().into(),
                    | Primitive::Pointer => unimplemented!(),
                },
                | None => ctx.context.struct_type(&[], false).into(),
            },
            | typ::Ptr(to) => to.as_basic_type(ctx).ptr_type(AddressSpace::Generic).into(),
            | typ::Box(to) => {
                let fst = to.as_basic_type(ctx).ptr_type(AddressSpace::Generic).into();
                let snd = ctx.context.ptr_sized_int_type(&ctx.target_data, None).into();

                ctx.context.struct_type(&[fst, snd], false).into()
            },
            | typ::Var(_) => ctx.context.struct_type(&[], false).into(),
            | typ::Func(sig) => {
                let fn_type = self.as_func_type(ctx);

                fn_type.ptr_type(AddressSpace::Const).into()
            },
            | typ::Generic(_, t) => t.as_basic_type(ctx),
        }
    }
}

impl AsFuncType for Ty {
    fn as_func_type<'ctx>(&self, ctx: &CodegenCtx<'ctx>) -> FunctionType<'ctx> {
        match &self.kind {
            | typ::Func(sig) => {
                let mut params = sig
                    .params
                    .iter()
                    .map(|p| {
                        let ty = p.ty.as_basic_type(ctx);

                        if p.flags.is_set(ir::Flags::IN) {
                            ty.ptr_type(AddressSpace::Generic).into()
                        } else {
                            ty
                        }
                    })
                    .collect::<Vec<_>>();

                let rets = sig
                    .rets
                    .iter()
                    .rev()
                    .filter_map(|r| {
                        let ty = r.ty.as_basic_type(ctx);

                        if r.flags.is_set(ir::Flags::OUT) {
                            params.insert(0, ty.ptr_type(AddressSpace::Generic).into());
                            None
                        } else {
                            Some(ty)
                        }
                    })
                    .rev()
                    .collect::<Vec<_>>();

                match rets.len() {
                    | 0 => ctx.context.void_type().fn_type(&params, false),
                    | 1 => rets[0].fn_type(&params, false),
                    | _ => ctx.context.struct_type(&rets, false).fn_type(&params, false),
                }
            },
            | typ::Generic(_, t) => t.as_func_type(ctx),
            | _ => unreachable!(),
        }
    }
}
