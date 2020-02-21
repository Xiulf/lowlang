use crate::{FunctionCtx, Backend};
use crate::place::Place;
use lowlang_syntax as syntax;
use cranelift_codegen::ir::{self, InstBuilder};
use std::ops::{Add, Sub, Mul, Div, Rem};

#[derive(Clone, Copy)]
struct Ratio<'a, 't, 'l, B: Backend> {
    fx: *mut FunctionCtx<'a, 't, 'l, B>,
    num: Value<'a, 't, 'l, B>,
    den: Value<'a, 't, 'l, B>,
}

struct Value<'a, 't, 'l, B: Backend>(*mut FunctionCtx<'a, 't, 'l, B>, ir::Value);

impl<'a, 't, 'l, B: Backend> FunctionCtx<'a, 't, 'l, B> {
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

    pub fn trans_binop_ratio_ratio(&mut self, place: Place<'t, 'l>, op: &syntax::BinOp, lhs: crate::value::Value<'t, 'l>, rhs: crate::value::Value<'t, 'l>) {
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

impl<'a, 't, 'l, B: Backend> Clone for Value<'a, 't, 'l, B> {
    fn clone(&self) -> Value<'a, 't, 'l, B> {
        Value(self.0, self.1)
    }
}

impl<'a, 't, 'l, B: Backend> Copy for Value<'a, 't, 'l, B> {}

impl<'a, 't, 'l, B: Backend> Ratio<'a, 't, 'l, B> {
    fn fx(&self) -> &mut FunctionCtx<'a, 't, 'l, B> {
        unsafe { &mut *(self.fx) }
    }

    fn lcm(&self, a: Value<'a, 't, 'l, B>, b: Value<'a, 't, 'l, B>) -> Value<'a, 't, 'l, B> {
        Value(self.fx(), self.fx().trans_lcm(a.1, b.1))
    }

    fn gcd(&self, a: Value<'a, 't, 'l, B>, b: Value<'a, 't, 'l, B>) -> Value<'a, 't, 'l, B> {
        Value(self.fx(), self.fx().trans_gcd(a.1, b.1))
    }
    
    fn add(&self, other: &Self, place: Place<'t, 'l>) {
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
    
    fn sub(&self, other: &Self, place: Place<'t, 'l>) {
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

    fn mul(&self, other: &Self, place: Place<'t, 'l>) {
        let gcd_ad = self.gcd(self.num, other.den);
        let gcd_bc = self.gcd(self.den, other.num);
        let num = self.num / gcd_ad * (other.num / gcd_bc);
        let den = self.den / gcd_bc * (other.den / gcd_ad);

        place.field(self.fx(), 0).store(self.fx(), num.into());
        place.field(self.fx(), 1).store(self.fx(), den.into());
    }

    fn div(&self, other: &Self, place: Place<'t, 'l>) {
        let gcd_ac = self.gcd(self.num, other.num);
        let gcd_bd = self.gcd(self.den, other.den);
        let num = self.num / gcd_ac * (other.den / gcd_bd);
        let den = self.den / gcd_bd * (other.num / gcd_ac);

        place.field(self.fx(), 0).store(self.fx(), num.into());
        place.field(self.fx(), 1).store(self.fx(), den.into());
    }
    
    fn rem(&self, other: &Self, place: Place<'t, 'l>) {
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

impl<'a, 't, 'l, B: Backend> Value<'a, 't, 'l, B> {
    fn fx(&self) -> &mut FunctionCtx<'a, 't, 'l, B> {
        unsafe { &mut *(self.0) }
    }

    fn branch<T: FnOnce(&mut FunctionCtx<'a, 't, 'l, B>), E: FnOnce(&mut FunctionCtx<'a, 't, 'l, B>)>
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

impl<'a, 't, 'l, B: Backend> Into<crate::value::Value<'t, 'l>> for Value<'a, 't, 'l, B> {
    fn into(self) -> crate::value::Value<'t, 'l> {
        crate::value::Value::new_val(self.1, self.fx().layouts.defaults.isize)
    }
}

impl<'a, 't, 'l, B: Backend> Add for Value<'a, 't, 'l, B> {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        let val = self.fx().builder.ins().iadd(self.1, other.1);

        Value(self.0, val)
    }
}

impl<'a, 't, 'l, B: Backend> Sub for Value<'a, 't, 'l, B> {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        let val = self.fx().builder.ins().isub(self.1, other.1);

        Value(self.0, val)
    }
}

impl<'a, 't, 'l, B: Backend> Mul for Value<'a, 't, 'l, B> {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        let val = self.fx().builder.ins().imul(self.1, other.1);

        Value(self.0, val)
    }
}

impl<'a, 't, 'l, B: Backend> Div for Value<'a, 't, 'l, B> {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        let val = self.fx().builder.ins().sdiv(self.1, other.1);

        Value(self.0, val)
    }
}

impl<'a, 't, 'l, B: Backend> Rem for Value<'a, 't, 'l, B> {
    type Output = Self;

    fn rem(self, other: Self) -> Self {
        let val = self.fx().builder.ins().srem(self.1, other.1);

        Value(self.0, val)
    }
}
