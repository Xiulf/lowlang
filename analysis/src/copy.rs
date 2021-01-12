use crate::Analyzer;
use ir::visitor::VisitorMut;
use transform::Transform;

pub struct CopyAnalyzer;

pub struct CopyTransform {
    copies: Vec<(ir::Location, ir::Stmt)>,
    body: *const ir::Body,
}

impl Analyzer for CopyAnalyzer {
    type Output = CopyTransform;

    fn analyze(&mut self, _: &ir::Module) -> Self::Output {
        CopyTransform {
            copies: Vec::new(),
            body: std::ptr::null(),
        }
    }
}

impl CopyTransform {
    #[inline(always)]
    fn body(&self) -> &ir::Body {
        unsafe { &*self.body }
    }

    #[inline(always)]
    fn body_mut(&mut self) -> &mut ir::Body {
        unsafe { &mut *(self.body as *mut _) }
    }
}

impl Transform for CopyTransform {
    fn apply(&mut self, module: &mut ir::Module) {
        self.visit_module(module);
    }
}

impl VisitorMut for CopyTransform {
    fn visit_body(&mut self, body: &mut ir::Body) {
        self.body = body;
        self.copies.clear();
        self.super_body(body);

        for (loc, stmt) in self.copies.drain(..).rev() {
            body.blocks[loc.block].stmts.insert(loc.stmt, stmt);
        }
    }

    fn visit_stmt(&mut self, stmt: &mut ir::Stmt, loc: ir::Location) {
        if let ir::Stmt::Assign(lhs, rhs) = stmt {
            if let ir::RValue::Use(rhs_op) = rhs {
                let lhs_ty = ir::place_type(self.body(), lhs);

                if let ir::Type::Opaque(g) = lhs_ty.kind {
                    lhs.elems.pop().unwrap();

                    if let ir::Operand::Place(rhs_place) = rhs_op {
                        rhs_place.elems.pop().unwrap();
                    }

                    let lhs_op = ir::Operand::Place(lhs.clone());
                    let mut builder = ir::Builder::new(self.body_mut());
                    let local = builder.create_tmp(ir::Ty::new(ir::Type::Tuple(Vec::new())));
                    let typeinfo = self.body().gen_local(&g).unwrap();
                    let typeinfo = ir::Place::new(typeinfo.id).deref().field(0);
                    let typeinfo = ir::Operand::Place(typeinfo);

                    lhs.local = local;
                    lhs.elems.clear();

                    *rhs = ir::RValue::Intrinsic(
                        "memcpy".into(),
                        vec![lhs_op, rhs_op.clone(), typeinfo],
                    )
                } else if let (ir::Type::Box(_), ir::Operand::Place(_)) = (lhs_ty.kind, &rhs_op) {
                    let mut builder = ir::Builder::new(self.body_mut());
                    let local = builder.create_tmp(ir::Ty::new(ir::Type::Tuple(Vec::new())));
                    let local = ir::Place::new(local);

                    self.copies.push((
                        loc,
                        ir::Stmt::Assign(
                            local,
                            ir::RValue::Intrinsic("box_copy".into(), vec![rhs_op.clone()]),
                        ),
                    ));
                }
            }
        }
    }
}
