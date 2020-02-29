use crate::Transformer;
use lowlang_syntax::*;
use lowlang_syntax::visit::VisitorMut;
use lowlang_syntax::layout::LayoutCtx;
use std::collections::BTreeMap;

pub struct ConstVar<'a, 't, 'l> {
    lcx: &'a LayoutCtx<'t, 'l>,
    current: BTreeMap<LocalId, Const<'t>>,
    remove: bool,
    changed: bool,
}

impl<'a, 't, 'l> ConstVar<'a, 't, 'l> {
    pub fn new(lcx: &'a LayoutCtx<'t, 'l>) -> ConstVar<'a, 't, 'l> {
        ConstVar {
            lcx,
            current: BTreeMap::new(),
            remove: false,
            changed: false,
        }
    }
}

impl<'a, 't, 'l> Transformer<'t> for ConstVar<'a, 't, 'l> {
    fn transform(&mut self, package: &mut Package<'t>) -> bool {
        self.visit_package(package);
        self.changed
    }

    fn reset(&mut self) {
        self.changed = false;
        self.remove = false;
        self.current.clear();
    }
}

impl<'a, 't, 'l> VisitorMut<'t> for ConstVar<'a, 't, 'l> {
    fn visit_body(&mut self, body: &mut Body<'t>) {
        self.current.clear();
        self.super_body(body);
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt<'t>) {
        self.super_stmt(stmt);

        if self.remove {
            *stmt = Stmt::Nop;
            self.remove = false;
            self.changed = true;
        }
    }

    fn visit_assign(&mut self, place: &mut Place, value: &mut Value<'t>) {
        self.super_assign(place, value);

        if let PlaceBase::Local(local) = &place.base {
            if self.current.contains_key(local) {
                self.current.remove(local);
            }

            if place.elems.is_empty() {
                match value {
                    Value::Use(Operand::Constant(c)) => {
                        self.current.insert(*local, c.clone());
                        self.remove = true;
                    },
                    Value::NullOp(op, ty) => {
                        let val = Const::Scalar(match op {
                            NullOp::SizeOf => ty.layout(self.lcx).details.size,
                            NullOp::AlignOf => ty.layout(self.lcx).details.align,
                        } as u128, self.lcx.defaults.usize.ty);

                        self.current.insert(*local, val);
                        self.remove = true;
                    },
                    _ => {},
                }
            }
        }
    }

    fn visit_op(&mut self, op: &mut Operand<'t>) {
        match op {
            Operand::Place(place) => {
                if place.elems.is_empty() {
                    if let PlaceBase::Local(id) = &place.base {
                        if let Some(new) = self.current.get(id) {
                            *op = Operand::Constant(new.clone());
                            return;
                        }
                    }
                }

                *op = Operand::Place(place.clone());
            },
            Operand::Constant(c) => *op = Operand::Constant(c.clone()),
        }
    }
}
