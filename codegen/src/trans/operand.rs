use crate::{FunctionCtx, Backend};
use crate::value::Value;
use syntax::layout::Layout;
use cranelift_codegen::ir::InstBuilder;

impl<'a, B: Backend> FunctionCtx<'a, B> {
    pub fn trans_op(&mut self, op: &syntax::Operand) -> Value {
        match op {
            syntax::Operand::Place(place) => {
                self.trans_place(place).to_value(self)
            },
            syntax::Operand::Constant(c) => match c {
                syntax::Const::Unit => Value::new_unit(),
                syntax::Const::Scalar(s, ty) => {
                    let clif_type = self.clif_type(ty.layout()).unwrap();
                    let value = match &ty.layout().details().ty {
                        syntax::Type::Float(syntax::FloatSize::Bits32) => self.builder.ins().f32const(f32::from_bits(*s as u32)),
                        syntax::Type::Float(syntax::FloatSize::Bits64) => self.builder.ins().f64const(f64::from_bits(*s as u64)),
                        _ => self.builder.ins().iconst(clif_type, *s as i64),
                    };

                    Value::new_val(value, ty.layout())
                },
                syntax::Const::FuncAddr(id) => {
                    let (func_id, _, _) = self.func_ids[id];
                    let func = self.module.declare_func_in_func(func_id, self.builder.func);
                    let value = self.builder.ins().func_addr(self.pointer_type, func);

                    Value::new_val(value, syntax::Type::Proc(Default::default()).layout())
                },
                _ => unimplemented!(),
            },
        }
    }
}
