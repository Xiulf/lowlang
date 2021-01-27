use crate::*;

pub struct InstBuilder<'body> {
    body: &'body mut Body,
    block: Block,
}

impl<'body> InstBuilder<'body> {
    pub fn new(body: &'body mut Body) -> Self {
        InstBuilder { body, block: Block::default() }
    }

    pub fn create_block(&mut self) -> Block {
        let id = self.body.blocks.next_idx();

        self.body.blocks.push(BasicBlock { id, ..BasicBlock::default() });

        id
    }

    pub fn set_block(&mut self, block: Block) {
        self.block = block;
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

    pub fn return_(&mut self) {
        self.block().term = Term::Return;
    }

    pub fn br(&mut self, to: Block) {
        self.block().term = Term::Br(to);
    }

    pub fn brnz(&mut self, val: Var, to: Block) {
        self.block().term = Term::BrNz(val, to);
    }

    pub fn brz(&mut self, val: Var, to: Block) {
        self.block().term = Term::BrZ(val, to);
    }

    pub fn const_(&mut self, c: Const, ty: Type) -> Var {
        let var = self.create_var(ty.clone());

        self.block().instrs.push(Instr::Const(var, c, ty));
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

        self.block().instrs.push(Instr::Load(var, ptr));
        var
    }

    pub fn store(&mut self, val: Var, ptr: Var) {
        self.block().instrs.push(Instr::Store(val, ptr));
    }

    pub fn call(&mut self, func: Var, args: Vec<Var>) -> Vec<Var> {
        let mut func_ty = &self.body.vars[func].ty;

        while let Type::Forall(_, ret) = func_ty {
            func_ty = &**ret;
        }

        let sig = if let Type::Func(sig) = func_ty {
            sig.clone()
        } else {
            panic!("cannot call a non-function type");
        };

        let rets = sig.rets.into_iter().map(|r| self.create_var(r)).collect::<Vec<_>>();

        self.block().instrs.push(Instr::Call(rets.clone(), func, args));
        rets
    }

    pub fn offset(&mut self, ptr: Var, by: Var) -> Var {
        let ptr_ty = self.body.vars[ptr].ty.clone();
        let var = self.create_var(ptr_ty);

        self.block().instrs.push(Instr::Offset(var, ptr, by));
        var
    }
}
