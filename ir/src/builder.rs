use crate::*;

pub struct Builder<'ir> {
    body: &'ir mut Body,
    current_block: Block,
}

impl<'ir> Builder<'ir> {
    pub fn new(body: &'ir mut Body) -> Self {
        Builder {
            body,
            current_block: Block::new(0),
        }
    }

    pub fn set_block(&mut self, block: Block) {
        self.current_block = block;
    }

    pub fn get_block(&self) -> Block {
        self.current_block
    }

    pub fn create_block(&mut self) -> Block {
        let block = self.body.blocks.next_idx();

        self.body.blocks.insert(
            block,
            BlockData {
                id: block,
                stmts: Vec::new(),
                term: Term::Abort,
            },
        );

        block
    }

    pub fn create_ret(&mut self, ty: Ty) -> Local {
        let local = self.body.locals.next_idx();

        self.body.locals.insert(
            local,
            LocalData {
                id: local,
                kind: LocalKind::Ret,
                ty,
            },
        );

        local
    }

    pub fn create_arg(&mut self, ty: Ty) -> Local {
        let local = self.body.locals.next_idx();

        self.body.locals.insert(
            local,
            LocalData {
                id: local,
                kind: LocalKind::Arg,
                ty,
            },
        );

        local
    }

    pub fn create_var(&mut self, ty: Ty) -> Local {
        let local = self.body.locals.next_idx();

        self.body.locals.insert(
            local,
            LocalData {
                id: local,
                kind: LocalKind::Var,
                ty,
            },
        );

        local
    }

    pub fn create_tmp(&mut self, ty: Ty) -> Local {
        let local = self.body.locals.next_idx();

        self.body.locals.insert(
            local,
            LocalData {
                id: local,
                kind: LocalKind::Tmp,
                ty,
            },
        );

        local
    }

    fn block(&mut self) -> &mut BlockData {
        &mut self.body.blocks[self.current_block]
    }

    pub fn local_ty(&self, local: Local) -> Ty {
        self.body.locals[local].ty.clone()
    }

    pub fn placed(&mut self, op: Operand, ty: Ty) -> Place {
        match op {
            Operand::Place(place) => place,
            Operand::Const(_) => {
                let tmp = self.create_tmp(ty);
                let tmp = Place::new(tmp);

                self.use_op(tmp.clone(), op);
                tmp
            }
        }
    }

    pub fn term(&self) -> &Term {
        &self.body.blocks[self.current_block].term
    }

    pub fn abort(&mut self) {
        self.block().term = Term::Abort;
    }

    pub fn return_(&mut self) {
        self.block().term = Term::Return;
    }

    pub fn jump(&mut self, to: Block) {
        self.block().term = Term::Jump(to);
    }

    pub fn switch(&mut self, op: Operand, vals: Vec<u128>, blocks: Vec<Block>) {
        self.block().term = Term::Switch(op, vals, blocks);
    }

    pub fn use_op(&mut self, place: Place, op: Operand) {
        self.block()
            .stmts
            .push(Stmt::Assign(place, RValue::Use(op)));
    }

    pub fn addrof(&mut self, place: Place, of: Place) {
        self.block()
            .stmts
            .push(Stmt::Assign(place, RValue::AddrOf(of)));
    }

    pub fn intrinsic(&mut self, place: Place, name: impl Into<String>, args: Vec<Operand>) {
        self.block()
            .stmts
            .push(Stmt::Assign(place, RValue::Intrinsic(name.into(), args)));
    }

    pub fn call(&mut self, rets: Vec<Place>, func: Operand, args: Vec<Operand>) {
        self.block().stmts.push(Stmt::Call(rets, func, args));
    }

    pub fn set_discr(&mut self, place: Place, val: u128) {
        self.block().stmts.push(Stmt::SetDiscr(place, val))
    }

    pub fn get_discr(&mut self, place: Place, val: Place) {
        self.block()
            .stmts
            .push(Stmt::Assign(place, RValue::GetDiscr(val)));
    }
}
