use crate::*;

impl Module {
    pub fn declare(&mut self, name: impl Into<String>, linkage: Linkage, ty: Ty) -> DefId {
        let id = self.defs.next_idx();

        self.defs.push(Decl {
            id,
            linkage,
            name: name.into(),
            kind: DeclKind::Def(ty),
        });

        id
    }

    pub fn declare_type(&mut self, name: impl Into<String>, linkage: Linkage) -> DefId {
        let id = self.defs.next_idx();

        self.defs.push(Decl {
            id,
            linkage,
            name: name.into(),
            kind: DeclKind::Type,
        });

        id
    }

    pub fn define(&mut self, def: DefId) -> InstBuilder {
        let mut body = Body::default();

        if let DeclKind::Def(ty) = &self.defs[def].kind {
            let mut ty = ty;

            while let Type::Forall(_, r) = &ty.kind {
                ty = &**r;
            }

            if let Type::Func(sig) = &ty.kind {
                body.rets = sig.rets.clone();
            }
        }

        self.bodies.insert(def, body);

        InstBuilder::new(&self.subset, self.bodies.get_mut(&def).unwrap())
    }
}

impl Ty {
    pub fn ptr(self) -> Self {
        Ty::new(Type::Ptr(Box::new(self)))
    }

    pub const BOOL: Self = Ty::new(Type::Int(1, false));
    pub const U8: Self = Ty::new(Type::Int(8, false));
    pub const U16: Self = Ty::new(Type::Int(16, false));
    pub const U32: Self = Ty::new(Type::Int(32, false));
    pub const U64: Self = Ty::new(Type::Int(64, false));
    pub const U128: Self = Ty::new(Type::Int(128, false));
    pub const I8: Self = Ty::new(Type::Int(8, true));
    pub const I16: Self = Ty::new(Type::Int(16, true));
    pub const I32: Self = Ty::new(Type::Int(32, true));
    pub const I64: Self = Ty::new(Type::Int(64, true));
    pub const I128: Self = Ty::new(Type::Int(128, true));
}

impl Signature {
    pub fn new() -> Self {
        Signature {
            params: Vec::new(),
            rets: Vec::new(),
        }
    }

    pub fn param(mut self, ty: Ty) -> Self {
        self.params.push(ty);
        self
    }

    pub fn ret(mut self, ty: Ty) -> Self {
        self.rets.push(ty);
        self
    }
}

pub struct InstBuilder<'ir> {
    module: &'ir ModuleSubset,
    body: &'ir mut Body,
    block: Block,
}

pub trait ToOperandVec {
    fn to_operand_vec(self) -> Vec<Operand>;
}

impl ToOperandVec for Vec<Operand> {
    fn to_operand_vec(self) -> Vec<Operand> {
        self
    }
}

impl ToOperandVec for &[Operand] {
    fn to_operand_vec(self) -> Vec<Operand> {
        self.to_vec()
    }
}

impl<T: Into<Operand>> ToOperandVec for T {
    fn to_operand_vec(self) -> Vec<Operand> {
        vec![self.into()]
    }
}

macro_rules! impl_to_op_vec {
    () => {};
    ($a:ident, $($v:ident,)*) => {
        impl_to_op_vec!($($v,)*);

        impl<$a: Into<Operand> $(,$v: Into<Operand>)*> ToOperandVec for ($a, $($v,)*) {
            #[allow(non_snake_case)]
            fn to_operand_vec(self) -> Vec<Operand> {
                let ($a, $($v,)*) = self;
                vec![$a.into() $(,$v.into())*]
            }
        }
    }
}

impl_to_op_vec!(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,);

impl<'ir> InstBuilder<'ir> {
    pub fn new(module: &'ir ModuleSubset, body: &'ir mut Body) -> Self {
        InstBuilder {
            module,
            body,
            block: Block::default(),
        }
    }

    pub fn create_block(&mut self) -> Block {
        let id = self.body.blocks.next_idx();

        self.body.blocks.push(BasicBlock { id, ..BasicBlock::default() });

        id
    }

    pub fn set_block(&mut self, block: Block) {
        self.block = block;
    }

    pub fn add_param(&mut self, ty: Ty) -> Var {
        let param = self.create_var(ty);

        self.block().params.push(param);
        param
    }

    fn block(&mut self) -> &mut BasicBlock {
        &mut self.body.blocks[self.block]
    }

    fn create_var(&mut self, ty: Ty) -> Var {
        let id = self.body.vars.next_idx();

        self.body.vars.push(Variable { id, ty });

        id
    }

    pub fn abort(&mut self) {
        self.block().term = Term::Abort;
    }

    pub fn return_(&mut self, vals: impl ToOperandVec) {
        self.block().term = Term::Return(vals.to_operand_vec());
    }

    pub fn br(&mut self, to: Block, args: Vec<Var>) {
        self.block().term = Term::Br(to, args);
    }

    pub fn brif(&mut self, val: impl Into<Operand>, then: Block, else_: Block, args: Vec<Var>) {
        self.block().term = Term::BrIf(val.into(), then, else_, args);
    }

    pub fn const_(&mut self, const_: Const, ty: Ty) -> Var {
        let var = self.create_var(ty);

        self.block().instrs.push(Instr {
            kind: InstrKind::Const { res: var, const_ },
        });

        var
    }

    pub fn load(&mut self, ptr: Var) -> Var {
        let ptr_ty = &self.body.vars[ptr].ty;
        let pointee = if let Type::Ptr(to) = &ptr_ty.kind {
            (**to).clone()
        } else {
            panic!("cannot load a non-pointer type");
        };

        let var = self.create_var(pointee);

        self.block().instrs.push(Instr {
            kind: InstrKind::Load { res: var, ptr },
        });

        var
    }

    pub fn store(&mut self, ptr: Var, val: impl Into<Operand>) {
        self.block().instrs.push(Instr {
            kind: InstrKind::Store { ptr, val: val.into() },
        });
    }

    pub fn load_field(&mut self, val: Var, idx: usize) -> Var {
        let val_ty = &self.body.vars[val].ty;
        let field_ty = if let Type::Tuple(tys) = &val_ty.kind {
            tys[idx].clone()
        } else if let Type::Def(def) = &val_ty.kind {
            let typedef = &self.module.types[def];

            if typedef.variants.len() == 1 {
                typedef.variants[0].tys[idx].clone()
            } else {
                panic!("cannot load field of a non-structure type");
            }
        } else {
            panic!("cannot load field of a non-structure type");
        };

        let var = self.create_var(field_ty);

        self.block().instrs.push(Instr {
            kind: InstrKind::LoadField { res: var, val, field: idx },
        });

        var
    }

    pub fn call(&mut self, func: impl Into<Operand>, args: impl ToOperandVec) -> Vec<Var> {
        let func: Operand = func.into();
        let mut func_ty = match func {
            | Operand::Var(v) => &self.body.vars[v].ty,
            | Operand::Const(Const::Addr(v)) => match &self.module.defs[v].kind {
                | DeclKind::Def(ty) => ty,
                | _ => unreachable!(),
            },
            | _ => unreachable!(),
        };

        while let Type::Forall(_, ret) = &func_ty.kind {
            func_ty = &**ret;
        }

        let sig = if let Type::Func(sig) = &func_ty.kind {
            sig.clone()
        } else {
            panic!("cannot call a non-function type");
        };

        let rets = sig.rets.into_iter().map(|r| self.create_var(r)).collect::<Vec<_>>();
        let args = args.to_operand_vec();

        self.block().instrs.push(Instr {
            kind: InstrKind::Call {
                rets: rets.clone(),
                func: func.into(),
                args,
            },
        });

        rets
    }

    pub fn offset(&mut self, ptr: Var, by: impl Into<Operand>) -> Var {
        let ptr_ty = self.body.vars[ptr].ty.clone();
        let var = self.create_var(ptr_ty);

        self.block().instrs.push(Instr {
            kind: InstrKind::Offset { res: var, ptr, by: by.into() },
        });

        var
    }

    pub fn add(&mut self, l: Var, r: impl Into<Operand>) -> Var {
        let l_ty = self.body.vars[l].ty.clone();
        let var = self.create_var(l_ty);

        self.block().instrs.push(Instr {
            kind: InstrKind::Add {
                res: var,
                lhs: l,
                rhs: r.into(),
            },
        });

        var
    }

    pub fn sub(&mut self, l: Var, r: impl Into<Operand>) -> Var {
        let l_ty = self.body.vars[l].ty.clone();
        let var = self.create_var(l_ty);

        self.block().instrs.push(Instr {
            kind: InstrKind::Sub {
                res: var,
                lhs: l,
                rhs: r.into(),
            },
        });

        var
    }

    pub fn mul(&mut self, l: Var, r: impl Into<Operand>) -> Var {
        let l_ty = self.body.vars[l].ty.clone();
        let var = self.create_var(l_ty);

        self.block().instrs.push(Instr {
            kind: InstrKind::Mul {
                res: var,
                lhs: l,
                rhs: r.into(),
            },
        });

        var
    }

    pub fn div(&mut self, l: Var, r: impl Into<Operand>) -> Var {
        let l_ty = self.body.vars[l].ty.clone();
        let var = self.create_var(l_ty);

        self.block().instrs.push(Instr {
            kind: InstrKind::Div {
                res: var,
                lhs: l,
                rhs: r.into(),
            },
        });

        var
    }

    pub fn rem(&mut self, l: Var, r: impl Into<Operand>) -> Var {
        let l_ty = self.body.vars[l].ty.clone();
        let var = self.create_var(l_ty);

        self.block().instrs.push(Instr {
            kind: InstrKind::Rem {
                res: var,
                lhs: l,
                rhs: r.into(),
            },
        });

        var
    }

    pub fn shl(&mut self, l: Var, r: impl Into<Operand>) -> Var {
        let l_ty = self.body.vars[l].ty.clone();
        let var = self.create_var(l_ty);

        self.block().instrs.push(Instr {
            kind: InstrKind::Shl {
                res: var,
                lhs: l,
                rhs: r.into(),
            },
        });

        var
    }

    pub fn shr(&mut self, l: Var, r: impl Into<Operand>) -> Var {
        let l_ty = self.body.vars[l].ty.clone();
        let var = self.create_var(l_ty);

        self.block().instrs.push(Instr {
            kind: InstrKind::Shr {
                res: var,
                lhs: l,
                rhs: r.into(),
            },
        });

        var
    }

    pub fn and(&mut self, l: Var, r: impl Into<Operand>) -> Var {
        let l_ty = self.body.vars[l].ty.clone();
        let var = self.create_var(l_ty);

        self.block().instrs.push(Instr {
            kind: InstrKind::And {
                res: var,
                lhs: l,
                rhs: r.into(),
            },
        });

        var
    }

    pub fn or(&mut self, l: Var, r: impl Into<Operand>) -> Var {
        let l_ty = self.body.vars[l].ty.clone();
        let var = self.create_var(l_ty);

        self.block().instrs.push(Instr {
            kind: InstrKind::Or {
                res: var,
                lhs: l,
                rhs: r.into(),
            },
        });

        var
    }

    pub fn xor(&mut self, l: Var, r: impl Into<Operand>) -> Var {
        let l_ty = self.body.vars[l].ty.clone();
        let var = self.create_var(l_ty);

        self.block().instrs.push(Instr {
            kind: InstrKind::Xor {
                res: var,
                lhs: l,
                rhs: r.into(),
            },
        });

        var
    }

    pub fn eq(&mut self, l: Var, r: impl Into<Operand>) -> Var {
        let var = self.create_var(Ty::BOOL);

        self.block().instrs.push(Instr {
            kind: InstrKind::Cmp {
                cc: CondCode::Equal,
                res: var,
                lhs: l,
                rhs: r.into(),
            },
        });

        var
    }

    pub fn ne(&mut self, l: Var, r: impl Into<Operand>) -> Var {
        let var = self.create_var(Ty::BOOL);

        self.block().instrs.push(Instr {
            kind: InstrKind::Cmp {
                cc: CondCode::NotEqual,
                res: var,
                lhs: l,
                rhs: r.into(),
            },
        });

        var
    }

    pub fn lt(&mut self, l: Var, r: impl Into<Operand>) -> Var {
        let var = self.create_var(Ty::BOOL);

        self.block().instrs.push(Instr {
            kind: InstrKind::Cmp {
                cc: CondCode::Less,
                res: var,
                lhs: l,
                rhs: r.into(),
            },
        });

        var
    }

    pub fn le(&mut self, l: Var, r: impl Into<Operand>) -> Var {
        let var = self.create_var(Ty::BOOL);

        self.block().instrs.push(Instr {
            kind: InstrKind::Cmp {
                cc: CondCode::LessEqual,
                res: var,
                lhs: l,
                rhs: r.into(),
            },
        });

        var
    }

    pub fn gt(&mut self, l: Var, r: impl Into<Operand>) -> Var {
        let var = self.create_var(Ty::BOOL);

        self.block().instrs.push(Instr {
            kind: InstrKind::Cmp {
                cc: CondCode::Greater,
                res: var,
                lhs: l,
                rhs: r.into(),
            },
        });

        var
    }

    pub fn ge(&mut self, l: Var, r: impl Into<Operand>) -> Var {
        let var = self.create_var(Ty::BOOL);

        self.block().instrs.push(Instr {
            kind: InstrKind::Cmp {
                cc: CondCode::GreaterEqual,
                res: var,
                lhs: l,
                rhs: r.into(),
            },
        });

        var
    }
}
