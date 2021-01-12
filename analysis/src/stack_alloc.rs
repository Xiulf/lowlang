use crate::Analyzer;
use ir::visitor::Visitor;
use transform::Transform;

pub struct AllocAnalyzer<'a> {
    target: &'a target_lexicon::Triple,
    body: *const ir::Body,
    allocs: Vec<Alloc>,
}

pub struct Alloc {
    local: ir::Local,
    loc: ir::Location,
    is_free: bool,
    kind: AllocKind,
}

pub enum AllocKind {
    Box { ty: ir::Ty },
    Stack { ty: String },
}

pub struct AllocTransform<'a> {
    target: &'a target_lexicon::Triple,
    allocs: Vec<Alloc>,
}

impl<'a> AllocAnalyzer<'a> {
    pub fn new(target: &'a target_lexicon::Triple) -> Self {
        AllocAnalyzer {
            target,
            body: std::ptr::null(),
            allocs: Vec::new(),
        }
    }
}

impl<'a> Analyzer for AllocAnalyzer<'a> {
    type Output = AllocTransform<'a>;

    fn analyze(&mut self, module: &ir::Module) -> Self::Output {
        self.visit_module(module);

        AllocTransform {
            target: self.target,
            allocs: std::mem::replace(&mut self.allocs, Vec::new()),
        }
    }
}

impl<'a> Transform for AllocTransform<'a> {
    fn apply(&mut self, module: &mut ir::Module) {
        for alloc in self.allocs.drain(..).rev() {
            let body = &mut module.bodies[alloc.loc.body];

            match alloc.kind {
                AllocKind::Stack { ty } => {
                    if alloc.is_free {
                        let arg = ir::Operand::Place(ir::Place::new(alloc.local));
                        let rvalue = ir::RValue::Intrinsic(String::from("stack_free"), vec![arg]);
                        let mut builder = ir::Builder::new(body);
                        let tmp = builder.create_tmp(ir::Ty::new(ir::Type::Tuple(Vec::new())));
                        let block = &mut body.blocks[alloc.loc.block];

                        block.stmts.insert(
                            alloc.loc.stmt + 1,
                            ir::Stmt::Assign(ir::Place::new(tmp), rvalue),
                        );
                    } else {
                        let type_info = body.gen_local(&ty).unwrap().id;
                        let type_info = ir::Place::new(type_info).deref().field(0);
                        let type_info = ir::Operand::Place(type_info);
                        let rvalue =
                            ir::RValue::Intrinsic(String::from("stack_alloc"), vec![type_info]);
                        let block = &mut body.blocks[alloc.loc.block];

                        block.stmts.insert(
                            alloc.loc.stmt,
                            ir::Stmt::Assign(ir::Place::new(alloc.local), rvalue),
                        );
                    }
                }
                AllocKind::Box { ty } => {
                    if alloc.is_free {
                        let arg = ir::Operand::Place(ir::Place::new(alloc.local));
                        let rvalue = ir::RValue::Intrinsic(String::from("box_free"), vec![arg]);
                        let mut builder = ir::Builder::new(body);
                        let tmp = builder.create_tmp(ir::Ty::new(ir::Type::Tuple(Vec::new())));
                        let block = &mut body.blocks[alloc.loc.block];

                        block.stmts.insert(
                            alloc.loc.stmt + 1,
                            ir::Stmt::Assign(ir::Place::new(tmp), rvalue),
                        );
                    } else {
                        let size_ty = ir::layout::ptr_sized_int(self.target);
                        let size = ir::layout::layout_of(&ty, self.target).size.bytes() as u128;
                        let size = ir::Operand::Const(ir::Const::Scalar(size, size_ty));
                        let rvalue = ir::RValue::Intrinsic(String::from("box_alloc"), vec![size]);
                        let block = &mut body.blocks[alloc.loc.block];

                        block.stmts.insert(
                            alloc.loc.stmt,
                            ir::Stmt::Assign(ir::Place::new(alloc.local), rvalue),
                        );
                    }
                }
            }
        }
    }
}

impl Visitor for AllocAnalyzer<'_> {
    fn visit_body(&mut self, body: &ir::Body) {
        self.body = body;
        self.super_body(body);
    }

    fn visit_stmt(&mut self, stmt: &ir::Stmt, loc: ir::Location) {
        match stmt {
            ir::Stmt::Init(local) => self.visit_init(*local, loc),
            ir::Stmt::Drop(local) => self.visit_drop(*local, loc),
            _ => {}
        }
    }

    fn visit_init(&mut self, local: ir::Local, loc: ir::Location) {
        let data = &unsafe { &*self.body }.locals[local];
        let ty = data.ty.access();

        if let ir::Type::Ptr(ty) = ty.kind {
            if let ir::Type::Opaque(ty) = ty.kind {
                self.allocs.push(Alloc {
                    local,
                    loc,
                    is_free: false,
                    kind: AllocKind::Stack { ty },
                });
            }
        } else if let ir::Type::Box(of) = ty.kind {
            self.allocs.push(Alloc {
                local,
                loc,
                is_free: false,
                kind: AllocKind::Box { ty: *of },
            });
        }
    }

    fn visit_drop(&mut self, local: ir::Local, loc: ir::Location) {
        let data = &unsafe { &*self.body }.locals[local];
        let ty = data.ty.access();

        if let ir::Type::Ptr(ty) = ty.kind {
            if let ir::Type::Opaque(ty) = ty.kind {
                self.allocs.push(Alloc {
                    local,
                    loc,
                    is_free: true,
                    kind: AllocKind::Stack { ty },
                });
            }
        } else if let ir::Type::Box(of) = ty.kind {
            self.allocs.push(Alloc {
                local,
                loc,
                is_free: true,
                kind: AllocKind::Box { ty: *of },
            });
        }
    }
}
