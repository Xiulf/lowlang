use crate::Analyzer;
use indexmap::IndexSet;
use ir::visitor::VisitorMut;
use transform::Transform;

pub struct WitnessAnalyzer;

pub struct WitnessTransform {
    module: *const ir::Module,
    body: *const ir::Body,
}

impl Analyzer for WitnessAnalyzer {
    type Output = WitnessTransform;

    fn analyze(&mut self, _: &ir::Module) -> Self::Output {
        WitnessTransform {
            module: std::ptr::null(),
            body: std::ptr::null(),
        }
    }
}

impl WitnessTransform {
    #[inline(always)]
    fn module(&self) -> &ir::Module {
        unsafe { &*self.module }
    }

    #[inline(always)]
    fn body(&self) -> &ir::Body {
        unsafe { &*self.body }
    }
}

impl Transform for WitnessTransform {
    fn apply(&mut self, module: &mut ir::Module) {
        self.visit_module(module);
    }
}

impl VisitorMut for WitnessTransform {
    fn visit_module(&mut self, module: &mut ir::Module) {
        self.module = module;
        self.super_module(module);
    }

    fn visit_decl(&mut self, decl: &mut ir::Decl) {
        if !decl.attrs.c_abi {
            self.super_decl(decl);
        }
    }

    fn visit_body(&mut self, body: &mut ir::Body) {
        if !self.module().decls[body.decl].attrs.c_abi {
            self.body = body;
            let mut gen = IndexSet::new();

            for local in body.locals.iter().rev() {
                if let ir::LocalKind::Ret | ir::LocalKind::Arg = local.kind {
                    collect_generic(&local.ty, &mut gen);
                }
            }

            for ty in gen {
                let ty = ir::Type::Ptr(Box::new(ir::Type::Type(ty)));
                let local = body.locals.next_idx();

                body.locals.insert(
                    local,
                    ir::LocalData {
                        id: local,
                        kind: ir::LocalKind::Arg,
                        ty,
                    },
                );
            }

            self.super_body(body);
        }
    }

    fn visit_stmt(&mut self, stmt: &mut ir::Stmt, _loc: ir::Location) {
        if let ir::Stmt::Call(_, func, args) = stmt {
            let func_ty = ir::operand_type(self.module(), self.body(), func);

            if let ir::Type::Func(ir::Signature { params, .. }) = func_ty {
                for param in params {
                    if let ir::Type::Ptr(ref to) = param {
                        if let ir::Type::Type(gen) = &**to {
                            let arg = if let Some(local) = self.body().gen_local(gen) {
                                ir::Operand::Place(ir::Place {
                                    local: local.id,
                                    elems: Vec::new(),
                                })
                            } else {
                                ir::Operand::Const(ir::Const::Undefined(param))
                            };

                            args.push(arg);
                        }
                    }
                }
            }
        }
    }

    fn visit_type(&mut self, ty: &mut ir::Type) {
        if let ir::Type::Func(ir::Signature { params, rets }) = ty {
            let mut gen = IndexSet::new();

            for ty in params.iter_mut() {
                collect_generic(ty, &mut gen);
                self.visit_type(ty);
            }

            for ty in rets.iter_mut() {
                collect_generic(ty, &mut gen);
                self.visit_type(ty);
            }

            for ty in gen {
                params.push(ir::Type::Ptr(Box::new(ir::Type::Type(ty))));
            }
        }
    }
}

fn collect_generic(ty: &ir::Type, gen: &mut IndexSet<String>) {
    match ty {
        ir::Type::Opaque(name) => {
            gen.insert(name.clone());
        }
        ir::Type::Tuple(tys) => {
            for ty in tys {
                collect_generic(ty, gen);
            }
        }
        ir::Type::Union(tys) => {
            for ty in tys {
                collect_generic(ty, gen);
            }
        }
        ir::Type::Ptr(to) => collect_generic(to, gen),
        ir::Type::Func(ir::Signature { params, rets }) => {
            for ty in params {
                collect_generic(ty, gen);
            }

            for ty in rets {
                collect_generic(ty, gen);
            }
        }
        _ => {}
    }
}
