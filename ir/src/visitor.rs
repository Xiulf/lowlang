use crate::*;

macro_rules! define_visitor {
    ($name:ident $($mut:ident)?) => {
        pub trait $name {
            fn visit_module(&mut self, module: &$($mut)? Module) {
                self.super_module(module);
            }

            fn visit_decl(&mut self, decl: &$($mut)? Decl) {
                self.super_decl(decl);
            }

            fn visit_impl(&mut self, impl_: &$($mut)? Impl) {
                self.super_impl(impl_);
            }

            fn visit_impl_entry(&mut self, _entry: &$($mut)? ImplEntry) {
            }

            fn visit_body(&mut self, body: &$($mut)? Body) {
                self.super_body(body);
            }

            fn visit_local(&mut self, local: &$($mut)? LocalData) {
                self.super_local(local);
            }

            fn visit_block(&mut self, block: &$($mut)? BlockData, body: BodyId) {
                self.super_block(block, body);
            }

            fn visit_stmt(&mut self, stmt: &$($mut)? Stmt, _loc: Location) {
                self.super_stmt(stmt);
            }

            fn visit_term(&mut self, term: &$($mut)? Term, _loc: Location) {
                self.super_term(term);
            }

            fn visit_rvalue(&mut self, rvalue: &$($mut)? RValue) {
                self.super_rvalue(rvalue);
            }

            fn visit_op(&mut self, op: &$($mut)? Operand) {
                self.super_op(op);
            }

            fn visit_place(&mut self, place: &$($mut)? Place) {
                self.super_place(place);
            }

            fn visit_const(&mut self, c: &$($mut)? Const) {
                self.super_const(c);
            }

            fn visit_type(&mut self, ty: &$($mut)? Type) {
                self.super_type(ty);
            }

            fn super_module(&mut self, module: &$($mut)? Module) {
                let Module { decls, impls, bodies } = module;

                for decl in decls {
                    self.visit_decl(decl);
                }

                for impl_ in impls {
                    self.visit_impl(impl_);
                }

                for body in bodies {
                    self.visit_body(body);
                }
            }

            fn super_decl(&mut self, decl: &$($mut)? Decl) {
                let Decl { ty, .. } = decl;

                self.visit_type(ty);
            }

            fn super_impl(&mut self, impl_: &$($mut)? Impl) {
                let Impl { entries, .. } = impl_;

                for entry in entries {
                    self.visit_impl_entry(entry);
                }
            }

            fn super_body(&mut self, body: &$($mut)? Body) {
                let Body { locals, blocks, id, decl: _ } = body;

                for local in locals {
                    self.visit_local(local);
                }

                for block in blocks {
                    self.visit_block(block, *id);
                }
            }

            fn super_local(&mut self, local: &$($mut)? LocalData) {
                let LocalData { ty, .. } = local;

                self.visit_type(ty);
            }

            fn super_block(&mut self, block: &$($mut)? BlockData, body: BodyId) {
                let BlockData { stmts, term, id } = block;

                for (i, stmt) in stmts.into_iter().enumerate() {
                    self.visit_stmt(stmt, Location { body, block: *id, stmt: i });
                }

                self.visit_term(term, Location { body, block: *id, stmt: stmts.len() });
            }

            fn super_stmt(&mut self, stmt: &$($mut)? Stmt) {
                match stmt {
                    Stmt::Init(_) => {},
                    Stmt::Drop(_) => {},
                    Stmt::Assign(place, rvalue) => {
                        self.visit_place(place);
                        self.visit_rvalue(rvalue);
                    }
                    Stmt::Call(rets, func, args) => {
                        for place in rets {
                            self.visit_place(place);
                        }

                        self.visit_op(func);

                        for op in args {
                            self.visit_op(op);
                        }
                    }
                }
            }

            fn super_term(&mut self, term: &$($mut)? Term) {
                match term {
                    Term::Switch(op, _, _) => self.visit_op(op),
                    _ => {},
                }
            }

            fn super_rvalue(&mut self, rvalue: &$($mut)? RValue) {
                match rvalue {
                    RValue::Use(op) => self.visit_op(op),
                    RValue::AddrOf(place) => self.visit_place(place),
                    RValue::Cast(place, ty) => {
                        self.visit_place(place);
                        self.visit_type(ty);
                    }
                    RValue::Intrinsic(_, args) => {
                        for op in args {
                            self.visit_op(op);
                        }
                    }
                }
            }

            fn super_op(&mut self, op: &$($mut)? Operand) {
                match op {
                    Operand::Place(place) => self.visit_place(place),
                    Operand::Const(c) => self.visit_const(c),
                }
            }

            fn super_place(&mut self, place: &$($mut)? Place) {
                let Place { elems, .. } = place;

                for elem in elems {
                    match elem {
                        PlaceElem::Index(op) => self.visit_op(op),
                        _ => {},
                    }
                }
            }

            fn super_const(&mut self, c: &$($mut)? Const) {
                match c {
                    Const::Tuple(cs) => {
                        for c in cs {
                            self.visit_const(c);
                        }
                    },
                    Const::Ptr(c) => self.visit_const(c),
                    _ => {},
                }
            }

            fn super_type(&mut self, ty: &$($mut)? Type) {
                match ty {
                    Type::Ptr(ty) => self.visit_type(ty),
                    Type::Tuple(tys) => {
                        for ty in tys {
                            self.visit_type(ty);
                        }
                    },
                    Type::Union(tys) => {
                        for ty in tys {
                            self.visit_type(ty);
                        }
                    },
                    Type::Func(sig) => {
                        let Signature { params, rets } = sig;

                        for ty in params {
                            self.visit_type(ty);
                        }

                        for ty in rets {
                            self.visit_type(ty);
                        }
                    }
                    _ => {},
                }
            }
        }
    };
}

define_visitor!(Visitor);
define_visitor!(VisitorMut mut);
