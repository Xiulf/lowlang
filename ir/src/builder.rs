use crate::*;

impl Module {
    pub fn declare(&mut self, name: impl Into<String>, linkage: Linkage, ty: Type) -> DefId {
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
        self.bodies.insert(def, Body::default());

        InstBuilder::new(&self.subset, self.bodies.get_mut(&def).unwrap())
    }
}

impl Instr {
    pub fn new(name: impl Into<String>) -> Self {
        Instr {
            name: name.into(),
            outputs: Vec::new(),
            args: Vec::new(),
        }
    }

    pub fn out(mut self, out: Var) -> Self {
        self.outputs.push(out);
        self
    }

    pub fn outs(mut self, outputs: Vec<Var>) -> Self {
        self.outputs.extend(outputs);
        self
    }

    pub fn arg(mut self, arg: impl Into<Operand>) -> Self {
        self.args.push(arg.into());
        self
    }

    pub fn args(mut self, args: Vec<Operand>) -> Self {
        self.args.extend(args);
        self
    }
}

impl Type {
    pub fn ptr(self) -> Self {
        Type::Ptr(Box::new(self))
    }

    pub const BOOL: Type = Type::Int(1, false);
    pub const U8: Type = Type::Int(8, false);
    pub const U16: Type = Type::Int(16, false);
    pub const U32: Type = Type::Int(32, false);
    pub const U64: Type = Type::Int(64, false);
    pub const U128: Type = Type::Int(128, false);
    pub const I8: Type = Type::Int(8, true);
    pub const I16: Type = Type::Int(16, true);
    pub const I32: Type = Type::Int(32, true);
    pub const I64: Type = Type::Int(64, true);
    pub const I128: Type = Type::Int(128, true);
}

impl Signature {
    pub fn new() -> Self {
        Signature {
            params: Vec::new(),
            rets: Vec::new(),
        }
    }

    pub fn param(mut self, ty: Type) -> Self {
        self.params.push(ty);
        self
    }

    pub fn ret(mut self, ty: Type) -> Self {
        self.rets.push(ty);
        self
    }
}

pub struct InstBuilder<'ir> {
    module: &'ir ModuleSubset,
    body: &'ir mut Body,
    block: Block,
}

pub trait ConstOrVar: Into<Operand> {}

impl ConstOrVar for Const {
}

impl ConstOrVar for Var {
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

impl<T: ConstOrVar> ToOperandVec for T {
    fn to_operand_vec(self) -> Vec<Operand> {
        vec![self.into()]
    }
}

macro_rules! impl_to_op_vec {
    () => {};
    ($a:ident, $($v:ident,)*) => {
        impl_to_op_vec!($($v,)*);

        impl<$a: ConstOrVar $(,$v: ConstOrVar)*> ToOperandVec for ($a, $($v,)*) {
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

    pub fn add_param(&mut self, ty: Type) -> Var {
        let param = self.create_var(ty);

        self.block().params.push(param);
        param
    }

    fn block(&mut self) -> &mut BasicBlock {
        &mut self.body.blocks[self.block]
    }

    fn create_var(&mut self, ty: Type) -> Var {
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

    pub fn brnz(&mut self, val: impl ConstOrVar, to: Block) {
        self.block().instrs.push(Instr::new("brnz").arg(val).arg(to));
    }

    pub fn brz(&mut self, val: impl ConstOrVar, to: Block) {
        self.block().instrs.push(Instr::new("brz").arg(val).arg(to));
    }

    pub fn const_(&mut self, c: Const, ty: Type) -> Var {
        let var = self.create_var(ty.clone());

        self.block().instrs.push(Instr::new("const").arg(c).arg(ty).out(var));
        var
    }

    pub fn load(&mut self, ptr: Var) -> Var {
        let ptr_ty = &self.body.vars[ptr].ty;
        let pointee = if let Type::Ptr(to) = ptr_ty {
            (**to).clone()
        } else {
            panic!("cannot load a non-pointer type");
        };

        let var = self.create_var(pointee);

        self.block().instrs.push(Instr::new("load").arg(ptr).out(var));
        var
    }

    pub fn store(&mut self, ptr: Var, val: impl ConstOrVar) {
        self.block().instrs.push(Instr::new("store").arg(ptr).arg(val));
    }

    pub fn load_field(&mut self, val: Var, idx: usize) -> Var {
        let val_ty = &self.body.vars[val].ty;
        let field_ty = if let Type::Tuple(tys) = val_ty {
            tys[idx].clone()
        } else if let Type::Def(def) = val_ty {
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

        self.block()
            .instrs
            .push(Instr::new("load_field").arg(val).arg(Const::Scalar(idx as u128)).out(var));

        var
    }

    pub fn call(&mut self, func: impl ConstOrVar, args: impl ToOperandVec) -> Vec<Var> {
        let func: Operand = func.into();
        let mut func_ty = match func {
            | Operand::Var(v) => &self.body.vars[v].ty,
            | Operand::Const(Const::Addr(v)) => match &self.module.defs[v].kind {
                | DeclKind::Def(ty) => ty,
                | _ => unreachable!(),
            },
            | _ => unreachable!(),
        };

        while let Type::Forall(_, ret) = func_ty {
            func_ty = &**ret;
        }

        let sig = if let Type::Func(sig) = func_ty {
            sig.clone()
        } else {
            panic!("cannot call a non-function type");
        };

        let rets = sig.rets.into_iter().map(|r| self.create_var(r)).collect::<Vec<_>>();
        let args = args.to_operand_vec();

        self.block().instrs.push(Instr::new("call").arg(func).args(args).outs(rets.clone()));
        rets
    }

    pub fn offset(&mut self, ptr: Var, by: impl ConstOrVar) -> Var {
        let ptr_ty = self.body.vars[ptr].ty.clone();
        let var = self.create_var(ptr_ty);

        self.block().instrs.push(Instr::new("offset").arg(ptr).arg(by).out(var));
        var
    }

    pub fn add(&mut self, l: Var, r: impl ConstOrVar) -> Var {
        let l_ty = self.body.vars[l].ty.clone();
        let var = self.create_var(l_ty);

        self.block().instrs.push(Instr::new("add").arg(l).arg(r).out(var));
        var
    }

    pub fn sub(&mut self, l: Var, r: impl ConstOrVar) -> Var {
        let l_ty = self.body.vars[l].ty.clone();
        let var = self.create_var(l_ty);

        self.block().instrs.push(Instr::new("sub").arg(l).arg(r).out(var));
        var
    }

    pub fn mul(&mut self, l: Var, r: impl ConstOrVar) -> Var {
        let l_ty = self.body.vars[l].ty.clone();
        let var = self.create_var(l_ty);

        self.block().instrs.push(Instr::new("mul").arg(l).arg(r).out(var));
        var
    }

    pub fn div(&mut self, l: Var, r: impl ConstOrVar) -> Var {
        let l_ty = self.body.vars[l].ty.clone();
        let var = self.create_var(l_ty);

        self.block().instrs.push(Instr::new("div").arg(l).arg(r).out(var));
        var
    }

    pub fn rem(&mut self, l: Var, r: impl ConstOrVar) -> Var {
        let l_ty = self.body.vars[l].ty.clone();
        let var = self.create_var(l_ty);

        self.block().instrs.push(Instr::new("rem").arg(l).arg(r).out(var));
        var
    }

    pub fn eq(&mut self, l: Var, r: impl ConstOrVar) -> Var {
        let bool_ty = Type::Int(1, false);
        let var = self.create_var(bool_ty);

        self.block().instrs.push(Instr::new("eq").arg(l).arg(r).out(var));
        var
    }

    pub fn ne(&mut self, l: Var, r: impl ConstOrVar) -> Var {
        let bool_ty = Type::Int(1, false);
        let var = self.create_var(bool_ty);

        self.block().instrs.push(Instr::new("ne").arg(l).arg(r).out(var));
        var
    }

    pub fn lt(&mut self, l: Var, r: impl ConstOrVar) -> Var {
        let bool_ty = Type::Int(1, false);
        let var = self.create_var(bool_ty);

        self.block().instrs.push(Instr::new("lt").arg(l).arg(r).out(var));
        var
    }

    pub fn le(&mut self, l: Var, r: impl ConstOrVar) -> Var {
        let bool_ty = Type::Int(1, false);
        let var = self.create_var(bool_ty);

        self.block().instrs.push(Instr::new("le").arg(l).arg(r).out(var));
        var
    }

    pub fn gt(&mut self, l: Var, r: impl ConstOrVar) -> Var {
        let bool_ty = Type::Int(1, false);
        let var = self.create_var(bool_ty);

        self.block().instrs.push(Instr::new("gt").arg(l).arg(r).out(var));
        var
    }

    pub fn ge(&mut self, l: Var, r: impl ConstOrVar) -> Var {
        let bool_ty = Type::Int(1, false);
        let var = self.create_var(bool_ty);

        self.block().instrs.push(Instr::new("ge").arg(l).arg(r).out(var));
        var
    }
}
