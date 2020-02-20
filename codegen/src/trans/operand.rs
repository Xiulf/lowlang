use crate::{FunctionCtx, Backend};
use crate::value::Value;
use crate::place::Place;
use cranelift_codegen::ir::InstBuilder;

impl<'a, 't, 'l, B: Backend> FunctionCtx<'a, 't, 'l, B> {
    pub fn trans_op(&mut self, op: &syntax::Operand<'t>) -> Value<'t, 'l> {
        match op {
            syntax::Operand::Place(place) => {
                self.trans_place(place).to_value(self)
            },
            syntax::Operand::Constant(c) => match c {
                syntax::Const::Unit => Value::new_unit(self.layouts),
                syntax::Const::Scalar(s, ty) => {
                    let ty_layout = ty.layout(self.layouts);
                    let clif_type = self.clif_type(ty_layout).unwrap();
                    let value = match &*ty_layout.ty {
                        syntax::Type::Float(syntax::FloatSize::Bits32) => self.builder.ins().f32const(f32::from_bits(*s as u32)),
                        syntax::Type::Float(syntax::FloatSize::Bits64) => self.builder.ins().f64const(f64::from_bits(*s as u64)),
                        _ => self.builder.ins().iconst(clif_type, *s as i64),
                    };

                    Value::new_val(value, ty_layout)
                },
                syntax::Const::FuncAddr(id, _) => {
                    let (func_id, _, _) = self.func_ids[id];
                    let func = self.module.declare_func_in_func(func_id, self.builder.func);
                    let value = self.builder.ins().func_addr(self.pointer_type, func);

                    Value::new_val(value, self.layouts.defaults.proc)
                },
                syntax::Const::Bytes(bytes) => {
                    let place = Place::new_stack(self, self.layouts.defaults.str);

                    self.trans_bytes(place, bytes)
                },
                syntax::Const::Param(_) => unreachable!(),
            },
        }
    }

    pub fn trans_bytes(&mut self, place: Place<'t, 'l>, bytes: &Box<[u8]>) -> Value<'t, 'l> {
        use cranelift_module::Linkage;
        let data_id = self.module.declare_data(&format!("__bytes_{}", self.bytes_count), Linkage::Local, false, Some(1)).unwrap();
        let mut data_ctx = cranelift_module::DataContext::new();

        *self.bytes_count += 1;
        data_ctx.define(bytes.clone());
        self.module.define_data(data_id, &data_ctx).unwrap();

        let global = self.module.declare_data_in_func(data_id, self.builder.func);
        let value = self.builder.ins().global_value(self.pointer_type, global);
        let len = self.builder.ins().iconst(self.pointer_type, bytes.len() as i64);

        place.field(self, 0).store(self, Value::new_val(value, self.layouts.defaults.ptr_u8));
        place.field(self, 1).store(self, Value::new_val(len, self.layouts.defaults.usize));

        place.to_value(self)
    }
}
