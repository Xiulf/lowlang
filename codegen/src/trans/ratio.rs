use crate::{FunctionCtx, Backend};
use crate::place::Place;
use cranelift_codegen::ir::{self, InstBuilder};
use std::ops::{Add, Sub, Mul, Div, Rem};

#[derive(Clone, Copy)]
struct Ratio<'a, B: Backend> {
    fx: *mut FunctionCtx<'a, B>,
    num: Value<'a, B>,
    den: Value<'a, B>,
}

struct Value<'a, B: Backend>(*mut FunctionCtx<'a, B>, ir::Value);

impl<'a, B: Backend> FunctionCtx<'a, B> {
     fn trans_gcd(&mut self, lhs: ir::Value, rhs: ir::Value) -> ir::Value {
        let mut sig = self.module.make_signature();

        sig.params.push(ir::AbiParam::new(self.pointer_type));
        sig.params.push(ir::AbiParam::new(self.pointer_type));
        sig.returns.push(ir::AbiParam::new(self.pointer_type));

        let gcd = unsafe { self.package().insintric_gcd() };
        let func = self.func_ids[&gcd].0;
        let func = self.module.declare_func_in_func(func, self.builder.func);
        let call = self.builder.ins().call(func, &[lhs, rhs]);

        self.builder.inst_results(call)[0]
    }

    fn trans_lcm(&mut self, lhs: ir::Value, rhs: ir::Value) -> ir::Value {
        let mut sig = self.module.make_signature();

        sig.params.push(ir::AbiParam::new(self.pointer_type));
        sig.params.push(ir::AbiParam::new(self.pointer_type));
        sig.returns.push(ir::AbiParam::new(self.pointer_type));

        let lcm = unsafe { self.package().insintric_lcm() };
        let func = self.func_ids[&lcm].0;
        let func = self.module.declare_func_in_func(func, self.builder.func);
        let call = self.builder.ins().call(func, &[lhs, rhs]);

        self.builder.inst_results(call)[0]
    }

    pub fn trans_binop_ratio_ratio(&mut self, place: Place, op: &syntax::BinOp, lhs: crate::value::Value, rhs: crate::value::Value) {
        let lhs_num = lhs.field(self, 0).load_scalar(self);
        let lhs_den = lhs.field(self, 1).load_scalar(self);
        let lhs = Ratio { fx: self, num: Value(self, lhs_num), den: Value(self, lhs_den) };
        let rhs_num = rhs.field(self, 0).load_scalar(self);
        let rhs_den = rhs.field(self, 1).load_scalar(self);
        let rhs = Ratio { fx: self, num: Value(self, rhs_num), den: Value(self, rhs_den) };

        match op {
            syntax::BinOp::Add => lhs.add(&rhs, place),
            syntax::BinOp::Sub => lhs.sub(&rhs, place),
            syntax::BinOp::Mul => lhs.mul(&rhs, place),
            syntax::BinOp::Div => lhs.div(&rhs, place),
            syntax::BinOp::Rem => lhs.rem(&rhs, place),
            _ => unimplemented!(),
        }
    }
}

impl<'a, B: Backend> Clone for Value<'a, B> {
    fn clone(&self) -> Value<'a, B> {
        Value(self.0, self.1)
    }
}

impl<'a, B: Backend> Copy for Value<'a, B> {}

impl<'a, B: Backend> Ratio<'a, B> {
    fn fx(&self) -> &mut FunctionCtx<'a, B> {
        unsafe { &mut *(self.fx) }
    }

    fn lcm(&self, a: Value<'a, B>, b: Value<'a, B>) -> Value<'a, B> {
        Value(self.fx(), self.fx().trans_lcm(a.1, b.1))
    }

    fn gcd(&self, a: Value<'a, B>, b: Value<'a, B>) -> Value<'a, B> {
        Value(self.fx(), self.fx().trans_gcd(a.1, b.1))
    }
    
    fn add(&self, other: &Self, place: Place) {
        self.den.branch(
            other.den,
            |fx| {
                let num = self.num + other.num;

                place.field(fx, 0).store(fx, num.into());
                place.field(fx, 1).store(fx, self.den.into());
            },
            |fx| {
                let lcm = self.lcm(self.den, other.den);
                let lhs_num = self.num * (lcm / self.den);
                let rhs_num = other.num * (lcm / other.den);
                let num = lhs_num + rhs_num;

                place.field(fx, 0).store(fx, num.into());
                place.field(fx, 1).store(fx, lcm.into());
            }
        );
    }
    
    fn sub(&self, other: &Self, place: Place) {
        self.den.branch(
            other.den,
            |fx| {
                let num = self.num - other.num;

                place.field(fx, 0).store(fx, num.into());
                place.field(fx, 1).store(fx, self.den.into());
            },
            |fx| {
                let lcm = self.lcm(self.den, other.den);
                let lhs_num = self.num * (lcm / self.den);
                let rhs_num = other.num * (lcm / other.den);
                let num = lhs_num - rhs_num;

                place.field(fx, 0).store(fx, num.into());
                place.field(fx, 1).store(fx, lcm.into());
            }
        );
    }

    fn mul(&self, other: &Self, place: Place) {
        let gcd_ad = self.gcd(self.num, other.den);
        let gcd_bc = self.gcd(self.den, other.num);
        let num = self.num / gcd_ad * (other.num / gcd_bc);
        let den = self.den / gcd_bc * (other.den / gcd_ad);

        place.field(self.fx(), 0).store(self.fx(), num.into());
        place.field(self.fx(), 1).store(self.fx(), den.into());
    }

    fn div(&self, other: &Self, place: Place) {
        let gcd_ac = self.gcd(self.num, other.num);
        let gcd_bd = self.gcd(self.den, other.den);
        let num = self.num / gcd_ac * (other.den / gcd_bd);
        let den = self.den / gcd_bd * (other.num / gcd_ac);

        place.field(self.fx(), 0).store(self.fx(), num.into());
        place.field(self.fx(), 1).store(self.fx(), den.into());
    }
    
    fn rem(&self, other: &Self, place: Place) {
        self.den.branch(
            other.den,
            |fx| {
                let num = self.num % other.num;

                place.field(fx, 0).store(fx, num.into());
                place.field(fx, 1).store(fx, self.den.into());
            },
            |fx| {
                let lcm = self.lcm(self.den, other.den);
                let lhs_num = self.num * (lcm / self.den);
                let rhs_num = other.num * (lcm / other.den);
                let num = lhs_num % rhs_num;

                place.field(fx, 0).store(fx, num.into());
                place.field(fx, 1).store(fx, lcm.into());
            }
        );
    }
}

impl<'a, B: Backend> Value<'a, B> {
    fn fx(&self) -> &mut FunctionCtx<'a, B> {
        unsafe { &mut *(self.0) }
    }

    fn branch<T: FnOnce(&mut FunctionCtx<'a, B>), E: FnOnce(&mut FunctionCtx<'a, B>)>
        (self, other: Self, then: T, else_: E)
    {
        let eq = self.fx().builder.ins().icmp(ir::condcodes::IntCC::Equal, self.1, other.1);
        let then_block = self.fx().builder.create_ebb();
        let else_block = self.fx().builder.create_ebb();
        let exit_block = self.fx().builder.create_ebb();

        self.fx().builder.ins().brz(eq, else_block, &[]);
        self.fx().builder.ins().jump(then_block, &[]);
        self.fx().builder.switch_to_block(then_block);

        then(self.fx());

        self.fx().builder.ins().jump(exit_block, &[]);
        self.fx().builder.switch_to_block(else_block);

        else_(self.fx());

        self.fx().builder.ins().jump(exit_block, &[]);
        self.fx().builder.switch_to_block(exit_block);
    }
}

impl<'a, B: Backend> Into<crate::value::Value> for Value<'a, B> {
    fn into(self) -> crate::value::Value {
        crate::value::Value::new_val(self.1, syntax::layout::ISIZE)
    }
}

impl<'a, B: Backend> Add for Value<'a, B> {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        let val = self.fx().builder.ins().iadd(self.1, other.1);

        Value(self.0, val)
    }
}

impl<'a, B: Backend> Sub for Value<'a, B> {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        let val = self.fx().builder.ins().isub(self.1, other.1);

        Value(self.0, val)
    }
}

impl<'a, B: Backend> Mul for Value<'a, B> {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        let val = self.fx().builder.ins().imul(self.1, other.1);

        Value(self.0, val)
    }
}

impl<'a, B: Backend> Div for Value<'a, B> {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        let val = self.fx().builder.ins().sdiv(self.1, other.1);

        Value(self.0, val)
    }
}

impl<'a, B: Backend> Rem for Value<'a, B> {
    type Output = Self;

    fn rem(self, other: Self) -> Self {
        let val = self.fx().builder.ins().srem(self.1, other.1);

        Value(self.0, val)
    }
}
