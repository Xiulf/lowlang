use crate::Analyzer;
use ir::visitor::Visitor;
use transform::Transform;

pub struct TypeInfoAnalyzer<'a> {
    target: &'a target_lexicon::Triple,
    module: *const ir::Module,
    body: *const ir::Body,
    infos: Vec<TypeInfo>,
}

pub struct TypeInfoTransform<'a> {
    target: &'a target_lexicon::Triple,
    infos: Vec<TypeInfo>,
}

pub struct TypeInfo {
    ty: ir::Ty,
    ops: Vec<*mut ir::Const>,
}

impl<'a> TypeInfoAnalyzer<'a> {
    pub fn new(target: &'a target_lexicon::Triple) -> Self {
        TypeInfoAnalyzer {
            target,
            module: std::ptr::null(),
            body: std::ptr::null(),
            infos: Vec::new(),
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
}

impl<'a> Analyzer for TypeInfoAnalyzer<'a> {
    type Output = TypeInfoTransform<'a>;

    fn analyze(&mut self, module: &ir::Module) -> Self::Output {
        self.visit_module(module);

        TypeInfoTransform {
            target: self.target,
            infos: std::mem::replace(&mut self.infos, Vec::new()),
        }
    }
}

impl<'a> Visitor for TypeInfoAnalyzer<'a> {
    fn visit_module(&mut self, module: &ir::Module) {
        self.module = module;
        self.super_module(module);
    }

    fn visit_body(&mut self, body: &ir::Body) {
        self.body = body;
        self.super_body(body);
    }

    fn visit_stmt(&mut self, stmt: &ir::Stmt, _: ir::Location) {
        if let ir::Stmt::Call(rets, func, args) = stmt {
            let sig = ir::operand_type(self.module(), self.body(), func).signature();
            let tys = sig.find_type_instances(args, rets, self.module(), self.body());

            for arg in args {
                if let ir::Operand::Const(
                    ref
                    c
                    @
                    ir::Const::Undefined(ir::Ty {
                        kind:
                            ir::Type::Ptr(box ir::Ty {
                                kind: ir::Type::Type(ref t),
                                ..
                            }),
                        ..
                    }),
                ) = arg
                {
                    let ty = tys[t].clone();

                    if let Some(info) = self.infos.iter_mut().find(|i| i.ty == ty) {
                        info.ops.push(c as *const _ as *mut _);
                    } else {
                        self.infos.push(TypeInfo {
                            ty,
                            ops: vec![c as *const _ as *mut _],
                        });
                    }
                }
            }
        }
    }
}

impl<'a> TypeInfoTransform<'a> {
    fn gen_type_info(
        target: &'a target_lexicon::Triple,
        module: &mut ir::Module,
        decl: ir::DeclId,
        ty: ir::Ty,
    ) {
        let bodyid = module.bodies.next_idx();
        let mut body = ir::Body::new(bodyid, decl);
        let mut builder = ir::Builder::new(&mut body);
        let layout = ir::layout::layout_of(&ty, target);
        let ret = builder.create_ret(ir::Ty::new(ir::Type::Type(String::new())));
        let place = ir::Place::new(ret);
        let entry = builder.create_block();

        builder.set_block(entry);

        builder.use_op(
            place.clone().field(0),
            ir::Operand::Const(ir::Const::Scalar(
                layout.size.bytes() as u128,
                ir::layout::ptr_sized_int(target),
            )),
        );

        builder.use_op(
            place.clone().field(1),
            ir::Operand::Const(ir::Const::Scalar(
                layout.align.bytes() as u128,
                ir::layout::ptr_sized_int(target),
            )),
        );

        builder.use_op(
            place.clone().field(2),
            ir::Operand::Const(ir::Const::Scalar(
                layout.stride.bytes() as u128,
                ir::layout::ptr_sized_int(target),
            )),
        );

        builder.return_();

        module.bodies.insert(bodyid, body);
    }
}

impl<'a> Transform for TypeInfoTransform<'a> {
    fn apply(&mut self, module: &mut ir::Module) {
        for info in self.infos.drain(..) {
            let decl = module.decls.next_idx();

            module.decls.insert(
                decl,
                ir::Decl {
                    id: decl,
                    name: format!("__type_info_{}", decl.index()),
                    linkage: ir::Linkage::Hidden,
                    ty: ir::Ty::new(ir::Type::Type(String::new())),
                    attrs: ir::Attrs::default(),
                },
            );

            Self::gen_type_info(self.target, module, decl, info.ty);

            for op in info.ops {
                // SAFETY: we have exclusive access to module and all `op` pointers point into
                // module
                unsafe {
                    *op = ir::Const::Addr(decl);
                }
            }
        }
    }
}
