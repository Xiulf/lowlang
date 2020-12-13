use crate::Analyzer;
use ir::visitor::Visitor;
use transform::Transform;

pub struct ThunkAnalyzer {
    module: *const ir::Module,
    body: *const ir::Body,
    thunks: Vec<Thunk>,
}

pub struct ThunkTransform {
    thunks: Vec<Thunk>,
}

pub struct Thunk {
    pub for_: ir::DeclId,
    pub from: ir::Signature,
    pub into: ir::Signature,
    pub ops: Vec<*mut ir::DeclId>,
}

impl ThunkAnalyzer {
    pub fn new() -> Self {
        ThunkAnalyzer {
            module: std::ptr::null(),
            body: std::ptr::null(),
            thunks: Vec::new(),
        }
    }

    #[inline(always)]
    fn module(&self) -> &ir::Module {
        unsafe { &*self.module }
    }

    #[inline(always)]
    fn body(&self) -> &ir::Body {
        unsafe { &*self.body }
    }

    fn find_thunk(&mut self, for_: ir::DeclId, into: &ir::Type) -> Option<&mut Thunk> {
        self.thunks
            .iter_mut()
            .find(|t| t.for_ == for_ && type_matches(&ir::Type::Func(t.into.clone()), into))
    }
}

impl Analyzer for ThunkAnalyzer {
    type Output = ThunkTransform;

    fn analyze(&mut self, module: &ir::Module) -> Self::Output {
        self.visit_module(module);

        ThunkTransform {
            thunks: std::mem::replace(&mut self.thunks, Vec::new()),
        }
    }
}

impl Visitor for ThunkAnalyzer {
    fn visit_module(&mut self, module: &ir::Module) {
        self.module = module;
        self.super_module(module)
    }

    fn visit_body(&mut self, body: &ir::Body) {
        self.body = body;
        self.super_body(body);
    }

    fn visit_stmt(&mut self, stmt: &ir::Stmt, _: ir::Location) {
        if let ir::Stmt::Call(_, func, args) = stmt {
            let func_ty = ir::operand_type(self.module(), self.body(), func);

            if let ir::Type::Func(ir::Signature { params, .. }) = func_ty {
                for (param, arg) in params.iter().zip(args) {
                    if let ir::Operand::Const(ir::Const::Addr(decl)) = arg {
                        let arg_ty = ir::operand_type(self.module(), self.body(), arg);

                        if !type_matches(&arg_ty, &param) {
                            if let Some(thunk) = self.find_thunk(*decl, &param) {
                                thunk.ops.push(decl as *const _ as *mut _);
                            } else {
                                self.thunks.push(Thunk {
                                    for_: *decl,
                                    from: arg_ty.signature(),
                                    into: param.signature(),
                                    ops: vec![decl as *const _ as *mut _],
                                });
                            }
                        }
                    }
                }
            }
        }
    }
}

impl Transform for ThunkTransform {
    fn apply(&mut self, module: &mut ir::Module) {
        for thunk in self.thunks.drain(..) {
            let id = module.decls.next_idx();
            let name = format!("{}_thunk_{}", module.decls[thunk.for_].name, id.index());

            module.decls.insert(
                id,
                ir::Decl {
                    id,
                    name,
                    linkage: ir::Linkage::Local,
                    ty: ir::Type::Func(thunk.into.clone()),
                },
            );

            let bodyid = module.bodies.next_idx();
            let mut body = ir::Body {
                id: bodyid,
                decl: id,
                locals: Default::default(),
                blocks: Default::default(),
            };

            let mut builder = ir::Builder::new(&mut body);
            let rets = thunk
                .into
                .rets
                .into_iter()
                .map(|p| builder.create_ret(p))
                .collect::<Vec<_>>();
            let params = thunk
                .into
                .params
                .into_iter()
                .map(|p| builder.create_arg(p))
                .collect::<Vec<_>>();
            let entry = builder.create_block();

            builder.set_block(entry);

            let args = (0..thunk.from.params.len())
                .map(|i| {
                    let param = params[i];
                    let param = ir::Place::new(param);

                    ir::Operand::Place(param)
                })
                .collect();

            let rets = (0..thunk.from.rets.len())
                .map(|i| {
                    let ret = rets[i];

                    ir::Place::new(ret)
                })
                .collect();

            let func = ir::Operand::Const(ir::Const::Addr(thunk.for_));

            builder.call(rets, func, args);
            builder.return_();

            module.bodies.insert(bodyid, body);

            for op in thunk.ops {
                unsafe {
                    *op = id;
                }
            }
        }
    }
}

use ir::Type;

fn type_matches(a: &Type, b: &Type) -> bool {
    use std::collections::HashMap;

    fn rec(a: &Type, b: &Type, env: &mut HashMap<String, String>) -> bool {
        match (a, b) {
            (Type::I8, Type::I8)
            | (Type::I16, Type::I16)
            | (Type::I32, Type::I32)
            | (Type::I64, Type::I64)
            | (Type::I128, Type::I128)
            | (Type::F32, Type::F32)
            | (Type::F64, Type::F64) => true,
            (Type::Ptr(a), Type::Ptr(b)) => rec(a, b, env),
            (Type::Tuple(a), Type::Tuple(b)) if a.len() == b.len() => {
                a.iter().zip(b).all(|(a, b)| rec(a, b, env))
            }
            (Type::Union(a), Type::Union(b)) if a.len() == b.len() => {
                a.iter().zip(b).all(|(a, b)| rec(a, b, env))
            }
            (Type::Func(a), Type::Func(b))
                if a.params.len() == b.params.len() && a.rets.len() == b.rets.len() =>
            {
                a.params.iter().zip(&b.params).all(|(a, b)| rec(a, b, env))
                    && a.rets.iter().zip(&b.rets).all(|(a, b)| rec(a, b, env))
            }
            (Type::Opaque(a), Type::Opaque(b))
            | (Type::Type(a), Type::Type(b))
            | (Type::Vwt(a), Type::Vwt(b)) => {
                if let Some(b) = env.get(b) {
                    a == b
                } else if env.values().any(|v| v == a) {
                    false
                } else {
                    env.insert(b.clone(), a.clone());
                    true
                }
            }
            _ => false,
        }
    }

    let mut env = HashMap::new();

    rec(a, b, &mut env)
}
