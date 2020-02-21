use crate::{FunctionCtx, Backend};
use crate::place::Place;
use crate::value::Value;
use lowlang_syntax as syntax;
use cranelift_codegen::ir::{self, InstBuilder};
use cranelift_codegen::ir::condcodes::{IntCC, FloatCC};

impl<'a, 't, 'l, B: Backend> FunctionCtx<'a, 't, 'l, B> {
    pub fn trans_value(&mut self, place: Place<'t, 'l>, value: &syntax::Value<'t>) {
        match value {
            syntax::Value::Use(op) => match op {
                syntax::Operand::Constant(syntax::Const::Unit) => {},
                syntax::Operand::Constant(syntax::Const::Bytes(bytes)) => {
                    self.trans_bytes(place, bytes);
                },
                _ => {
                    let value = self.trans_op(op);

                    place.store(self, value);
                },
            },
            syntax::Value::Ref(to) => {
                let to = self.trans_place(to);
                let ptr = Value::new_val(to.as_ptr(self).get_addr(self), place.layout);

                place.store(self, ptr);
            },
            syntax::Value::Slice(arr, lo, hi) => {
                let arr_place = self.trans_place(arr);
                let idx_ty = arr_place.layout.details.idx.unwrap();
                let arr = if let syntax::Type::Array(_, _) = &*arr_place.layout.ty {
                    arr_place.as_ptr(self)
                } else {
                    arr_place.field(self, 0).deref(self).as_ptr(self)
                };

                let lo = self.trans_op(lo).load_scalar(self);
                let lo2 = self.builder.ins().imul_imm(lo, idx_ty.details.size as i64);
                let arr = arr.offset_value(self, lo2);
                let arr = Value::new_val(arr.get_addr(self), place.layout.details.fields[0].1);
                let hi = self.trans_op(hi).load_scalar(self);
                let len = self.builder.ins().isub(hi, lo);
                let len_field = place.layout.details.fields[1];

                place.field(self, 0).store(self, arr);
                place.field(self, 1).store(self, Value::new_val(len, len_field.1));
            },
            syntax::Value::Cast(ty, op) => {
                let op = self.trans_op(op);

                match (self.clif_type(op.layout), self.clif_type(ty.layout(self.layouts))) {
                    (Some(_), Some(to_ty)) => {
                        let from = op.load_scalar(self);
                        let value = self.trans_cast(from, op.layout.sign(), to_ty, ty.layout(self.layouts).sign());

                        place.store(self, Value::new_val(value, place.layout));

                        return;
                    },
                    _ => {},
                }

                place.store(self, op.cast(ty.layout(self.layouts)));
            },
            syntax::Value::BinOp(op, lhs, rhs) => {
                let lhs = self.trans_op(lhs);
                let ty = lhs.layout.ty;

                if let syntax::Type::Ratio = &*ty {
                    let rhs = self.trans_op(rhs);
                    
                    return self.trans_binop_ratio_ratio(place, op, lhs, rhs);
                }

                let lhs = lhs.load_scalar(self);
                let rhs = self.trans_op(rhs).load_scalar(self);
                let mut value = match op {
                    syntax::BinOp::Add => match &*ty {
                        syntax::Type::Float(_) => self.builder.ins().fadd(lhs, rhs),
                        _ => self.builder.ins().iadd(lhs, rhs),
                    },
                    syntax::BinOp::Sub => match &*ty {
                        syntax::Type::Float(_) => self.builder.ins().fsub(lhs, rhs),
                        _ => self.builder.ins().isub(lhs, rhs),
                    },
                    syntax::BinOp::Mul => match &*ty {
                        syntax::Type::Float(_) => self.builder.ins().fmul(lhs, rhs),
                        _ => self.builder.ins().imul(lhs, rhs),
                    },
                    syntax::BinOp::Div => match &*ty {
                        syntax::Type::Int(_) => self.builder.ins().sdiv(lhs, rhs),
                        syntax::Type::UInt(_) => self.builder.ins().udiv(lhs, rhs),
                        syntax::Type::Float(_) => self.builder.ins().fdiv(lhs, rhs),
                        _ => unreachable!(),
                    },
                    syntax::BinOp::Rem => match &*ty {
                        syntax::Type::Int(_) => self.builder.ins().srem(lhs, rhs),
                        syntax::Type::UInt(_) => self.builder.ins().urem(lhs, rhs),
                        _ => unreachable!(),
                    },
                    syntax::BinOp::Eq => match &*ty {
                        syntax::Type::Char => self.builder.ins().icmp(IntCC::Equal, lhs, rhs),
                        syntax::Type::Int(_) => self.builder.ins().icmp(IntCC::Equal, lhs, rhs),
                        syntax::Type::UInt(_) => self.builder.ins().icmp(IntCC::Equal, lhs, rhs),
                        syntax::Type::Float(_) => self.builder.ins().fcmp(FloatCC::Equal, lhs, rhs),
                        _ => unreachable!(),
                    },
                    syntax::BinOp::Ne => match &*ty {
                        syntax::Type::Int(_) => self.builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
                        syntax::Type::UInt(_) => self.builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
                        syntax::Type::Float(_) => self.builder.ins().fcmp(FloatCC::NotEqual, lhs, rhs),
                        _ => unreachable!(),
                    },
                    syntax::BinOp::Lt => match &*ty {
                        syntax::Type::Int(_) => self.builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs),
                        syntax::Type::UInt(_) => self.builder.ins().icmp(IntCC::UnsignedLessThan, lhs, rhs),
                        syntax::Type::Float(_) => self.builder.ins().fcmp(FloatCC::LessThan, lhs, rhs),
                        _ => unreachable!(),
                    },
                    syntax::BinOp::Le => match &*ty {
                        syntax::Type::Int(_) => self.builder.ins().icmp(IntCC::SignedLessThanOrEqual, lhs, rhs),
                        syntax::Type::UInt(_) => self.builder.ins().icmp(IntCC::UnsignedLessThanOrEqual, lhs, rhs),
                        syntax::Type::Float(_) => self.builder.ins().fcmp(FloatCC::LessThanOrEqual, lhs, rhs),
                        _ => unreachable!(),
                    },
                    syntax::BinOp::Gt => match &*ty {
                        syntax::Type::Int(_) => self.builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs),
                        syntax::Type::UInt(_) => self.builder.ins().icmp(IntCC::UnsignedGreaterThan, lhs, rhs),
                        syntax::Type::Float(_) => self.builder.ins().fcmp(FloatCC::GreaterThan, lhs, rhs),
                        _ => unreachable!(),
                    },
                    syntax::BinOp::Ge => match &*ty {
                        syntax::Type::Int(_) => self.builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs),
                        syntax::Type::UInt(_) => self.builder.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, lhs, rhs),
                        syntax::Type::Float(_) => self.builder.ins().fcmp(FloatCC::GreaterThanOrEqual, lhs, rhs),
                        _ => unreachable!(),
                    },
                    syntax::BinOp::BitAnd => self.builder.ins().band(lhs, rhs),
                    syntax::BinOp::BitOr => self.builder.ins().bor(lhs, rhs),
                    syntax::BinOp::BitXOr => self.builder.ins().bxor(lhs, rhs),
                    syntax::BinOp::Shl => self.builder.ins().ishl(lhs, rhs),
                    syntax::BinOp::Shr => match &*ty {
                        syntax::Type::Int(_) => self.builder.ins().sshr(lhs, rhs),
                        syntax::Type::UInt(_) => self.builder.ins().ushr(lhs, rhs),
                        _ => unreachable!(),
                    },
                };

                match op {
                    syntax::BinOp::Eq | syntax::BinOp::Ne |
                    syntax::BinOp::Lt | syntax::BinOp::Le |
                    syntax::BinOp::Gt | syntax::BinOp::Ge => {
                        value = self.builder.ins().bint(ir::types::I8, value);
                    },
                    _ => {},
                }

                place.store(self, Value::new_val(value, place.layout));
            },
            syntax::Value::UnOp(op, operand) => {
                match op {
                    syntax::UnOp::Not => {
                        let operand = self.trans_op(operand);
                        let val = operand.load_scalar(self);
                        let res = self.builder.ins().bnot(val);

                        place.store(self, Value::new_val(res, place.layout));
                    },
                    syntax::UnOp::Neg => {
                        let operand = self.trans_op(operand);
                        let val = operand.load_scalar(self);
                        let res = match &*operand.layout.ty {
                            syntax::Type::Int(_) => {
                                self.builder.ins().irsub_imm(val, 0)
                            },
                            syntax::Type::Float(_) => {
                                self.builder.ins().fneg(val)
                            },
                            _ => unreachable!(),
                        };

                        place.store(self, Value::new_val(res, place.layout));
                    },
                }
            },
            syntax::Value::NullOp(op, ty) => {
                match op {
                    syntax::NullOp::SizeOf => {
                        let size = ty.layout(self.layouts).details.size;
                        let ty = self.clif_type(self.layouts.defaults.usize).unwrap();
                        let value = self.builder.ins().iconst(ty, size as i64);
                        
                        place.store(self, Value::new_val(value, place.layout))
                    },
                    syntax::NullOp::AlignOf => {
                        let align = ty.layout(self.layouts).details.align;
                        let ty = self.clif_type(self.layouts.defaults.usize).unwrap();
                        let value = self.builder.ins().iconst(ty, align as i64);
                        
                        place.store(self, Value::new_val(value, place.layout))
                    },
                }
            },
            syntax::Value::Init(ty, ops) => {
                match &**ty {
                    syntax::Type::Str |
                    syntax::Type::Ratio |
                    syntax::Type::Slice(_) => {
                        assert!(ops.len() == 2);

                        let a = self.trans_op(&ops[0]);
                        let b = self.trans_op(&ops[1]);

                        place.field(self, 0).store(self, a);
                        place.field(self, 1).store(self, b);
                    },
                    syntax::Type::Array(_, len) => {
                        assert!(ops.len() == *len);

                        for (i, op) in ops.iter().enumerate() {
                            let op = self.trans_op(op);

                            place.const_index(self, i).store(self, op);
                        }
                    },
                    syntax::Type::Vector(..) => {
                        unimplemented!();
                    },
                    syntax::Type::Tuple(_, tys) => {
                        assert!(ops.len() == tys.len());

                        for (i, op) in ops.iter().enumerate() {
                            let op = self.trans_op(op);

                            place.field(self, i).store(self, op);
                        }
                    },
                    syntax::Type::Tagged(tag, _) => {
                        assert!(ops.len() == 1);

                        let tag = Value::new_const(self, *tag as u128, self.layouts.defaults.usize);
                        let op = self.trans_op(&ops[0]);

                        place.field(self, 0).store(self, tag);
                        place.field(self, 1).store(self, op);
                    },
                    _ => {
                        assert!(ops.len() == 1);

                        let val = self.trans_op(&ops[0]);

                        place.store(self, val);
                    },
                }
            },
        }
    }
}
