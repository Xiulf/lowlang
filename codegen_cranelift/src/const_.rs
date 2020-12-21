use super::*;
use clif::Module;

impl<'ctx> ConstMethods<'ctx> for ClifBackend<'ctx> {
    type Backend = Self;

    fn alloc_const(
        mcx: &mut ModuleCtx<'_, 'ctx, ClifBackend<'ctx>>,
        c: &ir::Const,
        layout: ir::layout::TyLayout,
        data_id: Option<clif::DataId>,
    ) -> clif::DataId {
        let data_id = data_id.unwrap_or_else(|| {
            let id = mcx
                .module
                .declare_data(
                    &format!("__const_{}", mcx.backend.anon_count),
                    clif::Linkage::Local,
                    false,
                    false,
                )
                .unwrap();

            mcx.backend.anon_count += 1;
            id
        });

        let mut dcx = clif::DataContext::new();
        let mut bytes = Vec::with_capacity(layout.size.bytes() as usize);

        fn rec<'ctx>(
            mcx: &mut ModuleCtx<'_, 'ctx, ClifBackend<'ctx>>,
            dcx: &mut clif::DataContext,
            c: &ir::Const,
            layout: ir::layout::TyLayout,
            bytes: &mut Vec<u8>,
        ) {
            match c {
                ir::Const::Undefined(_) => {
                    bytes.resize(bytes.len() + layout.size.bytes() as usize, 0)
                }
                ir::Const::Scalar(s, _) => {
                    bytes.extend(&s.to_ne_bytes()[..layout.size.bytes() as usize])
                }
                ir::Const::Addr(id) => {
                    if let Some((id, _)) = mcx.func_ids.get(id) {
                        let func = mcx.module.declare_func_in_data(*id, dcx);

                        dcx.write_function_addr(bytes.len() as u32, func);
                        bytes.extend(vec![0; layout.size.bytes() as usize]);
                    } else {
                        unimplemented!();
                    }
                }
                ir::Const::Ptr(to) => {
                    let id = ClifBackend::alloc_const(mcx, to, layout.pointee(&mcx.target), None);
                    let global = mcx.module.declare_data_in_data(id, dcx);

                    dcx.write_data_addr(bytes.len() as u32, global, 0);
                    bytes.extend(vec![0; layout.size.bytes() as usize]);
                }
                ir::Const::Tuple(cs) => match &layout.fields {
                    ir::layout::FieldsShape::Arbitrary { offsets } => {
                        let mut i = 0;

                        for (j, (c, offset)) in cs.iter().zip(offsets).enumerate() {
                            bytes.extend(vec![0; offset.bytes() as usize - i]);
                            i = offset.bytes() as usize;

                            let field = layout.field(j, &mcx.target);

                            i += field.size.bytes() as usize;
                            rec(mcx, dcx, c, field, bytes);
                        }
                    }
                    _ => unimplemented!(),
                },
            }
        }

        rec(mcx, &mut dcx, c, layout, &mut bytes);

        bytes.resize(bytes.capacity(), 0);
        dcx.define(bytes.into());
        mcx.module.define_data(data_id, &dcx).unwrap();
        data_id
    }
}
