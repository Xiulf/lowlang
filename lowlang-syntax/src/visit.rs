use crate::*;

#[macro_export]
macro_rules! make_visitor{
    ($name:ident, $($mut:ident)?) => {
        #[allow(unused_variables)]
        pub trait $name<'t> {
            fn visit_package(&mut self, package: & $($mut)? Package<'t>) {
                self.super_package(package)
            }

            fn visit_extern(&mut self, id: ItemId, extern_: & $($mut)? Extern<'t>) {
                self.super_extern(extern_)
            }

            fn visit_global(&mut self, id: ItemId, global: & $($mut)? Global<'t>) {
                self.super_global(global)
            }

            fn visit_body(&mut self, id: ItemId, body: & $($mut)? Body<'t>) {
                self.super_body(body)
            }

            fn visit_local(&mut self, local: & $($mut)? Local<'t>) {
                self.super_local(local)
            }

            fn visit_block(&mut self, block: & $($mut)? Block<'t>) {
                self.super_block(block)
            }

            fn visit_stmt(&mut self, stmt: & $($mut)? Stmt<'t>) {
                self.super_stmt(stmt)
            }

            fn visit_assign(&mut self, place: & $($mut)? Place, value: & $($mut)? Value<'t>) {
                self.super_assign(place, value)
            }

            fn visit_term(&mut self, term: & $($mut)? Terminator<'t>) {
                self.super_term(term)
            }

            fn visit_place(&mut self, place: & $($mut)? Place) {
                self.super_place(place)
            }

            fn visit_value(&mut self, value: & $($mut)? Value<'t>) {
                self.super_value(value)
            }

            fn visit_op(&mut self, op: & $($mut)? Operand<'t>) {
                self.super_op(op)
            }

            fn visit_const(&mut self, const_: & $($mut)? Const<'t>) {
                self.super_const(const_)
            }

            fn visit_ty(&mut self, ty: & $($mut)? Ty<'t>) {
                self.super_ty(ty)
            }

            fn visit_sig(&mut self, sig: & $($mut)? Signature<'t>) {
                self.super_sig(sig)
            }

            fn super_package(&mut self, package: & $($mut)? Package<'t>) {
                let Package { name: _, externs, globals, bodies } = package;

                for (id, extern_) in externs {
                    self.visit_extern(*id, extern_);
                }

                for (id, global) in globals {
                    self.visit_global(*id, global);
                }

                for (id, body) in bodies {
                    self.visit_body(*id, body);
                }
            }

            fn super_extern(&mut self, extern_: & $($mut)? Extern<'t>) {
                match extern_ {
                    Extern::Proc(_, sig) => self.visit_sig(sig),
                    Extern::Global(_, ty) => self.visit_ty(ty),
                }
            }

            fn super_global(&mut self, global: & $($mut)? Global<'t>) {
                let Global { name: _, export: _, attributes: _, init: _, ty } = global;

                self.visit_ty(ty);
            }

            fn super_body(&mut self, body: & $($mut)? Body<'t>) {
                let Body { id: _, name: _, export: _, attributes: _, conv: _, generics, locals, blocks } = body;

                for (_, param) in generics {
                    match param {
                        GenParam::Type(Some(ty)) => self.visit_ty(ty),
                        GenParam::Type(None) => {},
                        GenParam::Const(Some(const_)) => self.visit_const(const_),
                        GenParam::Const(None) => {},
                    }
                }

                for (_, local) in locals {
                    self.visit_local(local);
                }

                for (_, block) in blocks {
                    self.visit_block(block);
                }
            }

            fn super_local(&mut self, local: & $($mut)? Local<'t>) {
                let Local { id: _, kind: _, ty } = local;

                self.visit_ty(ty);
            }

            fn super_block(&mut self, block: & $($mut)? Block<'t>) {
                let Block { id: _, stmts, term } = block;

                for stmt in stmts {
                    self.visit_stmt(stmt);
                }

                self.visit_term(term);
            }

            fn super_stmt(&mut self, stmt: & $($mut)? Stmt<'t>) {
                match stmt {
                    Stmt::Assign(place, value) => self.visit_assign(place, value),
                    Stmt::Nop => {},
                }
            }

            fn super_assign(&mut self, place: & $($mut)? Place, value: & $($mut)? Value<'t>) {
                self.visit_place(place);
                self.visit_value(value);
            }

            fn super_term(&mut self, term: & $($mut)? Terminator<'t>) {
                match term {
                    Terminator::Unset => {},
                    Terminator::Abort => {},
                    Terminator::Return => {},
                    Terminator::Jump(_) => {},
                    Terminator::Call(places, proc, args, _) => {
                        for place in places {
                            self.visit_place(place);
                        }

                        self.visit_op(proc);

                        for arg in args {
                            self.visit_op(arg);
                        }
                    },
                    Terminator::Switch(pred, _, _) => self.visit_op(pred),
                }
            }

            fn super_place(&mut self, place: & $($mut)? Place) {
                let Place { base, elems } = place;

                match base {
                    PlaceBase::Local(_) => {},
                    PlaceBase::Global(_) => {},
                }

                for elem in elems {
                    match elem {
                        PlaceElem::Deref => {},
                        PlaceElem::Field(_) => {},
                        PlaceElem::Index(place) => self.visit_place(place),
                        PlaceElem::ConstIndex(_) => {}
                    }
                }
            }

            fn super_value(&mut self, value: & $($mut)? Value<'t>) {
                match value {
                    Value::Use(op) => self.visit_op(op),
                    Value::Ref(place) => self.visit_place(place),
                    Value::Cast(ty, op) => {
                        self.visit_ty(ty);
                        self.visit_op(op);
                    },
                    Value::Slice(place, lo, hi) => {
                        self.visit_place(place);
                        self.visit_op(lo);
                        self.visit_op(hi);
                    },
                    Value::Init(ty, ops) => {
                        self.visit_ty(ty);

                        for op in ops {
                            self.visit_op(op);
                        }
                    },
                    Value::BinOp(_, lhs, rhs) => {
                        self.visit_op(lhs);
                        self.visit_op(rhs);
                    },
                    Value::UnOp(_, op) => self.visit_op(op),
                    Value::NullOp(_, ty) => self.visit_ty(ty),
                }
            }

            fn super_op(&mut self, op: & $($mut)? Operand<'t>) {
                match op {
                    Operand::Place(place) => self.visit_place(place),
                    Operand::Constant(const_) => self.visit_const(const_),
                }
            }

            fn super_const(&mut self, const_: & $($mut)? Const<'t>) {
                match const_ {
                    Const::Unit => {},
                    Const::Scalar(_, ty) => self.visit_ty(ty),
                    Const::Bytes(_) => {},
                    Const::FuncAddr(_, gen) => {
                        for (_, arg) in gen {
                            match arg {
                                GenArg::Type(ty) => self.visit_ty(ty),
                                GenArg::Const(const_) => self.visit_const(const_),
                            }
                        }
                    },
                    Const::Param(_) => {},
                }
            }

            fn super_ty(&mut self, ty: & $($mut)? Ty<'t>) {}

            fn super_sig(&mut self, sig: & $($mut)? Signature<'t>) {
                let Signature(_, params, rets) = sig;

                for param in params {
                    self.visit_ty(param);
                }

                for ret in rets {
                    self.visit_ty(ret);
                }
            }
        }
    };
}

make_visitor!(Visitor,);
make_visitor!(VisitorMut, mut);
