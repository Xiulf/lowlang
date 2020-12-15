use crate::Analyzer;
use ir::visitor::VisitorMut;
use transform::Transform;

pub struct GenericAnalyzer;

pub struct GenericFixer {
    module: *const ir::Module,
    body: *const ir::Body,
    insert: Vec<(ir::Location, ir::Stmt)>,
}

impl Analyzer for GenericAnalyzer {
    type Output = GenericFixer;

    fn analyze(&mut self, _module: &ir::Module) -> Self::Output {
        GenericFixer {
            module: std::ptr::null(),
            body: std::ptr::null(),
            insert: Vec::new(),
        }
    }
}

impl GenericFixer {
    #[inline(always)]
    fn module(&self) -> &ir::Module {
        unsafe { &*self.module }
    }

    #[inline(always)]
    fn body(&self) -> &ir::Body {
        unsafe { &*self.body }
    }

    #[inline(always)]
    fn body_mut(&mut self) -> &mut ir::Body {
        unsafe { &mut *(self.body as *mut _) }
    }

    fn placed(&mut self, op: ir::Operand, loc: ir::Location) -> ir::Place {
        match op {
            ir::Operand::Place(place) => place,
            ir::Operand::Const(ref c) => {
                let ty = ir::const_type(self.module(), &c);
                let mut builder = ir::Builder::new(self.body_mut());
                let local = builder.create_tmp(ty);
                let place = ir::Place::new(local);

                self.insert
                    .push((loc, ir::Stmt::Assign(place.clone(), ir::RValue::Use(op))));

                place
            }
        }
    }

    fn fix_operand(&mut self, op: &mut ir::Operand, loc: ir::Location) {
        let op_ty = ir::operand_type(self.module(), self.body(), op);

        if !matches!(op_ty, ir::Type::Opaque(_)) {
            let val = self.placed(op.clone(), loc);
            let mut builder = ir::Builder::new(self.body_mut());
            let local = builder.create_tmp(ir::Type::Ptr(Box::new(op_ty)));
            let place = ir::Place::new(local);

            self.insert.push((
                loc,
                ir::Stmt::Assign(place.clone(), ir::RValue::AddrOf(val)),
            ));

            *op = ir::Operand::Place(place);
        }
    }

    fn fix_operand2(&mut self, op: &mut ir::Operand, loc: ir::Location, ty: ir::Type) {
        let val = self.placed(op.clone(), loc);
        let mut builder = ir::Builder::new(self.body_mut());
        let ty = ir::Type::Ptr(Box::new(ty));
        let local = builder.create_tmp(ty.clone());
        let place = ir::Place::new(local);

        self.insert.push((
            loc,
            ir::Stmt::Assign(place.clone(), ir::RValue::Cast(val, ty)),
        ));

        *op = ir::Operand::Place(place.deref());
    }
}

impl Transform for GenericFixer {
    fn apply(&mut self, module: &mut ir::Module) {
        self.visit_module(module);
    }
}

impl VisitorMut for GenericFixer {
    fn visit_module(&mut self, module: &mut ir::Module) {
        self.module = module;

        let ir::Module {
            decls,
            bodies,
            impls: _,
        } = module;

        for body in bodies {
            self.visit_body(body);
        }

        for decl in decls {
            self.visit_decl(decl);
        }
    }

    fn visit_body(&mut self, body: &mut ir::Body) {
        self.body = body;

        let ir::Body { locals, blocks, .. } = body;

        for block in blocks.iter_mut() {
            self.visit_block(block, body.id);
        }

        for local in locals {
            self.visit_local(local);
        }

        let insert = std::mem::replace(&mut self.insert, Vec::new());

        for (loc, stmt) in insert.into_iter().rev() {
            blocks[loc.block].stmts.insert(loc.stmt, stmt);
        }
    }

    fn visit_local(&mut self, local: &mut ir::LocalData) {
        match (local.kind, &local.ty) {
            (ir::LocalKind::Ret, ir::Type::Opaque(_)) => {
                local.ty = ir::Type::Ptr(Box::new(local.ty.clone()));
                local.kind = ir::LocalKind::Arg;
            }
            _ => self.super_local(local),
        }
    }

    fn visit_stmt(&mut self, stmt: &mut ir::Stmt, loc: ir::Location) {
        if let ir::Stmt::Assign(lhs, ir::RValue::Use(ir::Operand::Place(rhs))) = stmt {
            let lhs_ty = ir::place_type(self.body(), lhs);

            if let ir::Type::Opaque(_) = lhs_ty {
                lhs.elems.push(ir::PlaceElem::Deref);
                rhs.elems.push(ir::PlaceElem::Deref);
            }
        } else if let ir::Stmt::Call(rets, func, args) = stmt {
            let func_ty = ir::operand_type(self.module(), self.body(), func);

            if let ir::Type::Func(ir::Signature {
                params: param_tys,
                rets: ret_tys,
            }) = func_ty
            {
                for (i, param_ty) in param_tys.iter().enumerate() {
                    if let ir::Type::Opaque(_) = param_ty {
                        self.fix_operand(&mut args[i], loc);
                    } else if let ir::Type::Opaque(_) =
                        ir::operand_type(self.module(), self.body(), &args[i])
                    {
                        self.fix_operand2(&mut args[i], loc, param_ty.clone());
                    }
                }

                for (i, ret_ty) in ret_tys.iter().enumerate().rev() {
                    if let ir::Type::Opaque(_) = ret_ty {
                        let place = rets.remove(i);

                        args.insert(0, ir::Operand::Place(place));
                        self.fix_operand(&mut args[0], loc);
                    } else if let ir::Type::Opaque(_) = ir::place_type(self.body(), &rets[i]) {
                        let mut builder = ir::Builder::new(self.body_mut());
                        let ty = ir::Type::Ptr(Box::new(ret_ty.clone()));
                        let local = builder.create_tmp(ty.clone());
                        let place = ir::Place::new(local);

                        self.insert.push((
                            loc,
                            ir::Stmt::Assign(place.clone(), ir::RValue::Cast(rets[i].clone(), ty)),
                        ));

                        rets[i] = place.deref();
                    }
                }
            }
        }
    }

    fn visit_type(&mut self, ty: &mut ir::Type) {
        if let ir::Type::Opaque(name) = ty {
            *ty = ir::Type::Ptr(Box::new(ir::Type::Opaque(name.clone())));
        } else if let ir::Type::Func(ir::Signature { params, rets }) = ty {
            for ty in params.iter_mut() {
                self.visit_type(ty);
            }

            let generic = rets
                .drain_filter(|r| {
                    if let ir::Type::Opaque(_) = r {
                        true
                    } else {
                        false
                    }
                })
                .collect::<Vec<_>>();

            for ty in generic.into_iter().rev() {
                params.insert(0, ir::Type::Ptr(Box::new(ty)));
            }
        } else {
            self.super_type(ty);
        }
    }
}
