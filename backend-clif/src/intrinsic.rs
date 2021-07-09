use super::*;

macro_rules! intrinsics {
    ($self:ident, $name:ident, $rets:ident, $args:ident, { $($(| $i:ident)+ => ($($arg:ident),*) $body:block)* }) => {
        match $name {
            $($(stringify!($i))|* => {
                $(let $arg = $args.next().unwrap();)*
                let res = $body;

                for (i, r) in <_>::into_iter(res).enumerate() {
                    $self.vars.insert($rets[i].0, r);
                }
            },)*
            _ => unreachable!(),
        }
    };
}

impl<'a, 'ctx> BodyCtx<'a, 'ctx> {
    pub fn lower_intrinsic(&mut self, name: &str, rets: &[ir::Var], args: Vec<Val>) {
        let mut args = args.into_iter();

        intrinsics!(self, name, rets, args, {
            | ptr_offset => (ptr, offset) {
                let pointee = ptr.layout().pointee(self.db);
                let ptr = ptr.load(self);
                let offset = offset.load(self);
                let offset = self.bcx.ins().imul_imm(offset, pointee.size.bytes() as i64);
                let res = self.bcx.ins().iadd(ptr, offset);
                let layout = self.db.layout_of(self.body[rets[0]].ty);

                [Val::new_val(res, layout)]
            }

            | sub_i8 | sub_i16 | sub_i32 | sub_i64 | sub_i128
            | sub_u8 | sub_u16 | sub_u32 | sub_u64 | sub_u128
            => (lhs, rhs) {
                let lhs = lhs.load(self);
                let rhs = rhs.load(self);
                let res = self.bcx.ins().isub(lhs, rhs);
                let layout = self.db.layout_of(self.body[rets[0]].ty);

                [Val::new_val(res, layout)]
            }

            | lt_i8 | lt_i16| lt_i32 | lt_i64 | lt_i128 => (lhs, rhs) {
                let lhs = lhs.load(self);
                let rhs = rhs.load(self);
                let cmp = self.bcx.ins().icmp(clif::IntCC::SignedLessThan, lhs, rhs);
                let cmp = self.bcx.ins().bint(clif::types::I8, cmp);
                let layout = self.db.layout_of(self.body[rets[0]].ty);

                [Val::new_val(cmp, layout)]
            }

            | lt_u8 | lt_u16| lt_u32 | lt_u64 | lt_u128 => (lhs, rhs) {
                let lhs = lhs.load(self);
                let rhs = rhs.load(self);
                let cmp = self.bcx.ins().icmp(clif::IntCC::UnsignedLessThan, lhs, rhs);
                let cmp = self.bcx.ins().bint(clif::types::I8, cmp);
                let layout = self.db.layout_of(self.body[rets[0]].ty);

                [Val::new_val(cmp, layout)]
            }

            | le_i8 | le_i16| le_i32 | le_i64 | le_i128 => (lhs, rhs) {
                let lhs = lhs.load(self);
                let rhs = rhs.load(self);
                let cmp = self.bcx.ins().icmp(clif::IntCC::SignedLessThanOrEqual, lhs, rhs);
                let cmp = self.bcx.ins().bint(clif::types::I8, cmp);
                let layout = self.db.layout_of(self.body[rets[0]].ty);

                [Val::new_val(cmp, layout)]
            }

            | le_u8 | le_u16| le_u32 | le_u64 | le_u128 => (lhs, rhs) {
                let lhs = lhs.load(self);
                let rhs = rhs.load(self);
                let cmp = self.bcx.ins().icmp(clif::IntCC::UnsignedLessThanOrEqual, lhs, rhs);
                let cmp = self.bcx.ins().bint(clif::types::I8, cmp);
                let layout = self.db.layout_of(self.body[rets[0]].ty);

                [Val::new_val(cmp, layout)]
            }

            | gt_i8 | gt_i16| gt_i32 | gt_i64 | gt_i128 => (lhs, rhs) {
                let lhs = lhs.load(self);
                let rhs = rhs.load(self);
                let cmp = self.bcx.ins().icmp(clif::IntCC::SignedGreaterThan, lhs, rhs);
                let cmp = self.bcx.ins().bint(clif::types::I8, cmp);
                let layout = self.db.layout_of(self.body[rets[0]].ty);

                [Val::new_val(cmp, layout)]
            }

            | gt_u8 | gt_u16| gt_u32 | gt_u64 | gt_u128 => (lhs, rhs) {
                let lhs = lhs.load(self);
                let rhs = rhs.load(self);
                let cmp = self.bcx.ins().icmp(clif::IntCC::UnsignedGreaterThan, lhs, rhs);
                let cmp = self.bcx.ins().bint(clif::types::I8, cmp);
                let layout = self.db.layout_of(self.body[rets[0]].ty);

                [Val::new_val(cmp, layout)]
            }
        });
    }
}
