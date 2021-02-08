#![allow(unused_variables)]

use crate::*;

macro_rules! make_visitor {
    ($name:ident $($mut:ident)?) => {
        pub trait $name {
            fn visit_module(&mut self, module: &$($mut)? Module) {
                self.super_module(module);
            }

            fn visit_decl(&mut self, decl: &$($mut)? Decl) {
                self.super_decl(decl);
            }

            fn visit_type_def(&mut self, id: DefId, def: &$($mut)? TypeDef) {
                self.super_type_def(def);
            }

            fn visit_variant(&mut self, variant: &$($mut)? Variant) {
                self.super_variant(variant);
            }

            fn visit_body(&mut self, id: DefId, body: &$($mut)? Body) {
                self.super_body(body);
            }

            fn visit_variable(&mut self, var: &$($mut)? Variable) {
                self.super_variable(var);
            }

            fn visit_block(&mut self, block: &$($mut)? BasicBlock) {
                self.super_block(block);
            }

            fn visit_instr(&mut self, instr: &$($mut)? Instr) {
                self.super_instr(instr);
            }

            fn visit_term(&mut self, term: &$($mut)? Term) {
                self.super_term(term);
            }

            fn visit_operand(&mut self, op: &$($mut)? Operand) {
                self.super_operand(op);
            }

            fn visit_const(&mut self, c: &$($mut)? Const) {
                self.super_const(c);
            }

            fn visit_var(&mut self, var: &$($mut)? Var) {
            }

            fn visit_type(&mut self, ty: &$($mut)? Ty) {
                self.super_type(ty);
            }

            fn super_module(&mut self, module: &$($mut)? Module) {
                let Module { subset, bodies } = module;
                let ModuleSubset { defs, types } = subset;

                for decl in defs {
                    self.visit_decl(decl);
                }

                for (&id, ty) in types {
                    self.visit_type_def(id, ty);
                }

                for (&id, body) in bodies {
                    self.visit_body(id, body);
                }
            }

            fn super_decl(&mut self, decl: &$($mut)? Decl) {
                let Decl { kind, .. } = decl;

                match kind {
                    DeclKind::Def(ty) => self.visit_type(ty),
                    DeclKind::Type => {},
                }
            }

            fn super_type_def(&mut self, decl: &$($mut)? TypeDef) {
                let TypeDef { variants } = decl;

                for variant in variants {
                    self.visit_variant(variant);
                }
            }

            fn super_variant(&mut self, variant: &$($mut)? Variant) {
                let Variant { tys } = variant;

                for ty in tys {
                    self.visit_type(ty);
                }
            }

            fn super_body(&mut self, decl: &$($mut)? Body) {
                let Body { vars, blocks, .. } = decl;

                for var in vars {
                    self.visit_variable(var);
                }

                for block in blocks {
                    self.visit_block(block);
                }
            }

            fn super_variable(&mut self, decl: &$($mut)? Variable) {
                let Variable { ty, .. } = decl;

                self.visit_type(ty);
            }

            fn super_block(&mut self, decl: &$($mut)? BasicBlock) {
                let BasicBlock { instrs, term, .. } = decl;

                for instr in instrs {
                    self.visit_instr(instr);
                }

                self.visit_term(term);
            }

            fn super_instr(&mut self, decl: &$($mut)? Instr) {
                // let Instr { outputs, args, .. } = decl;
                //
                // for out in outputs {
                //     self.visit_var(out);
                // }
                //
                // for arg in args {
                //     self.visit_operand(arg);
                // }
            }

            fn super_term(&mut self, decl: &$($mut)? Term) {
                match decl {
                    Term::Br(_, vars) => {
                        for var in vars {
                            self.visit_var(var);
                        }
                    }
                    Term::BrIf(cond, _, _, vars) => {
                        self.visit_operand(cond);

                        for var in vars {
                            self.visit_var(var);
                        }
                    },
                    Term::Return(ops) => {
                        for op in ops {
                            self.visit_operand(op);
                        }
                    }
                    Term::Unset => {},
                    Term::Abort => {},
                }
            }

            fn super_operand(&mut self, decl: &$($mut)? Operand) {
                match decl {
                    Operand::Var(v) => self.visit_var(v),
                    Operand::Const(c) => self.visit_const(c),
                }
            }

            fn super_const(&mut self, decl: &$($mut)? Const) {
            }

            fn super_type(&mut self, decl: &$($mut)? Ty) {
                match &$($mut)? decl.kind {
                    Type::Ptr(to) => self.visit_type(to),
                    Type::Tuple(tys) => {
                        for ty in tys {
                            self.visit_type(ty);
                        }
                    },
                    Type::Func(sig) => {
                        let Signature { params, rets } = sig;

                        for param in params {
                            self.visit_type(param);
                        }

                        for ret in rets {
                            self.visit_type(ret);
                        }
                    }
                    Type::Forall(_, ty) => self.visit_type(ty),
                    Type::Int(_, _) => {},
                    Type::Float(_) => {},
                    Type::Def(_) => {},
                    Type::Var(_) => {},
                }
            }
        }
    };
}

make_visitor!(Visitor);
make_visitor!(VisitorMut mut);
