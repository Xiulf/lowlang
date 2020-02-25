use crate::Transformer;
use lowlang_syntax::*;
use lowlang_syntax::visit::VisitorMut;
use lowlang_syntax::layout::LayoutCtx;

pub struct ConstVar<'a, 't, 'l> {
    lcx: &'a LayoutCtx<'t, 'l>,
    current: Option<(LocalId, Const<'t>)>,
    remove: bool,
}

impl<'a, 't, 'l> ConstVar<'a, 't, 'l> {
    pub fn new(lcx: &'a LayoutCtx<'t, 'l>) -> ConstVar<'a, 't, 'l> {
        ConstVar {
            lcx,
            current: None,
            remove: false,
        }
    }
}

impl<'a, 't, 'l> Transformer<'t> for ConstVar<'a, 't, 'l> {
    fn transform(&mut self, package: &mut Package<'t>) {
        self.visit_package(package);
    }
}

impl<'a, 't, 'l> VisitorMut<'t> for ConstVar<'a, 't, 'l> {
    fn visit_body(&mut self, body: &mut Body<'t>) {
        self.current = None;
        self.super_body(body);
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt<'t>) {
        self.super_stmt(stmt);

        if self.remove {
            *stmt = Stmt::Nop;
            self.remove = false;
        }
    }

    fn visit_assign(&mut self, place: &mut Place, value: &mut Value<'t>) {
        self.super_assign(place, value);

        if let PlaceBase::Local(local) = place.base {
            if let &Some((current, _)) = &self.current {
                if local == current {
                    self.current = None;
                }
            }

            if place.elems.is_empty() {
                match value {
                    Value::Use(Operand::Constant(c)) => {
                        self.current = Some((local, c.clone()));
                        self.remove = true;
                    },
                    Value::NullOp(op, ty) => {
                        let val = Const::Scalar(match op {
                            NullOp::SizeOf => ty.layout(self.lcx).details.size,
                            NullOp::AlignOf => ty.layout(self.lcx).details.align,
                        } as u128, self.lcx.defaults.usize.ty);

                        self.current = Some((local, val));
                        self.remove = true;
                    },
                    _ => {},
                }
            }
        }
    }

    fn visit_op(&mut self, op: &mut Operand<'t>) {
        if let Some((local, val)) = &self.current {
            *op = replace_op(op, local, Operand::Constant(val.clone()));
        }

        self.super_op(op);
    }
}

fn replace_op<'t>(orig: &Operand<'t>, id: &LocalId, new: Operand<'t>) -> Operand<'t> {
    match orig {
        Operand::Place(place) => {
            if place.elems.is_empty() {
                if let PlaceBase::Local(id2) = &place.base {
                    if id2 == id {
                        return new;
                    }
                }
            }

            Operand::Place(place.clone())
        },
        Operand::Constant(c) => Operand::Constant(c.clone())
    }
}
