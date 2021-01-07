use crate::Analyzer;
use ir::visitor::Visitor;
use transform::Transform;

pub struct StackAllocAnalyzer {
    body: *const ir::Body,
    allocs: Vec<Alloc>,
}

pub struct Alloc {
    local: ir::Local,
    loc: ir::Location,
    is_free: bool,
    ty: String,
}

pub struct StackAllocTransform {
    allocs: Vec<Alloc>,
}

impl StackAllocAnalyzer {
    pub fn new() -> Self {
        StackAllocAnalyzer {
            body: std::ptr::null(),
            allocs: Vec::new(),
        }
    }
}

impl Analyzer for StackAllocAnalyzer {
    type Output = StackAllocTransform;

    fn analyze(&mut self, module: &ir::Module) -> Self::Output {
        self.visit_module(module);

        StackAllocTransform {
            allocs: std::mem::replace(&mut self.allocs, Vec::new()),
        }
    }
}

impl Transform for StackAllocTransform {
    fn apply(&mut self, module: &mut ir::Module) {
        for alloc in self.allocs.drain(..).rev() {
            let body = &mut module.bodies[alloc.loc.body];

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
                let type_info = body.gen_local(&alloc.ty).unwrap().id;
                let type_info = ir::Place::new(type_info).deref().field(0);
                let type_info = ir::Operand::Place(type_info);
                let rvalue = ir::RValue::Intrinsic(String::from("stack_alloc"), vec![type_info]);
                let block = &mut body.blocks[alloc.loc.block];

                block.stmts.insert(
                    alloc.loc.stmt,
                    ir::Stmt::Assign(ir::Place::new(alloc.local), rvalue),
                );
            }
        }
    }
}

impl Visitor for StackAllocAnalyzer {
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

        if let ir::Type::Ptr(ty) = &data.ty.kind {
            if let ir::Type::Opaque(ty) = &ty.kind {
                self.allocs.push(Alloc {
                    local,
                    loc,
                    is_free: false,
                    ty: ty.clone(),
                });
            }
        }
    }

    fn visit_drop(&mut self, local: ir::Local, loc: ir::Location) {
        let data = &unsafe { &*self.body }.locals[local];

        if let ir::Type::Ptr(ty) = &data.ty.kind {
            if let ir::Type::Opaque(ty) = &ty.kind {
                self.allocs.push(Alloc {
                    local,
                    loc,
                    is_free: true,
                    ty: ty.clone(),
                });
            }
        }
    }
}
