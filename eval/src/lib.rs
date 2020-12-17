use index_vec::IndexVec;
use ir::*;

pub fn evaluate(module: &Module, body: &Body, target: &target_lexicon::Triple) -> Vec<Const> {
    let mut eval_ctx = EvalCtx::new(module, body, target);

    eval_ctx.eval();
    eval_ctx.finish()
}

pub struct EvalCtx<'ir> {
    module: &'ir Module,
    body: &'ir Body,
    target: &'ir target_lexicon::Triple,
    locals: IndexVec<Local, Const>,
    current_block: Block,
    status: EvalStatus,
}

pub enum EvalStatus {
    Busy,
    Done,
    Error,
}

impl<'ir> EvalCtx<'ir> {
    pub fn new(module: &'ir Module, body: &'ir Body, target: &'ir target_lexicon::Triple) -> Self {
        EvalCtx {
            module,
            body,
            target,
            locals: body
                .locals
                .iter()
                .map(|l| Const::Undefined(l.ty.clone()))
                .collect(),
            current_block: body.blocks.first().unwrap().id,
            status: EvalStatus::Busy,
        }
    }

    pub fn finish(self) -> Vec<Const> {
        self.body
            .rets()
            .map(|r| self.locals[r.id].clone())
            .collect()
    }

    pub fn eval(&mut self) {
        while let EvalStatus::Busy = self.status {
            let block = &self.body.blocks[self.current_block];

            for stmt in &block.stmts {
                self.eval_stmt(stmt);
            }

            self.eval_term(&block.term);
        }
    }

    fn eval_stmt(&mut self, stmt: &'ir Stmt) {
        match stmt {
            Stmt::Assign(place, rvalue) => {
                let val = self.eval_rvalue(rvalue);

                self.store(place, val);
            }
            Stmt::Call(..) => unimplemented!(),
        }
    }

    fn eval_term(&mut self, term: &'ir Term) {
        match term {
            Term::Abort => {
                for r in self.body.rets() {
                    self.locals[r.id] = Const::Undefined(r.ty.clone());
                }

                self.status = EvalStatus::Error;
            }
            Term::Return => {
                self.status = EvalStatus::Done;
            }
            Term::Jump(block) => {
                self.current_block = *block;
            }
            Term::Switch(op, vals, blocks) => {
                let op = self.eval_op(op);

                if let Const::Scalar(s, _) = op {
                    let mut block = *blocks.last().unwrap();

                    for (val, b) in vals.iter().zip(blocks) {
                        if *val == s {
                            block = *b;
                        }
                    }

                    self.current_block = block;
                } else {
                    unreachable!();
                }
            }
        }
    }

    fn eval_rvalue(&mut self, rvalue: &'ir RValue) -> Const {
        match rvalue {
            RValue::Use(op) => self.eval_op(op),
            RValue::AddrOf(place) => {
                let val = self.load(place);

                Const::Ptr(Box::new(val))
            }
            RValue::Cast(place, _) => self.load(place),
            RValue::Intrinsic(..) => unimplemented!(),
        }
    }

    fn eval_op(&mut self, op: &'ir Operand) -> Const {
        match op {
            Operand::Place(p) => self.load(p),
            Operand::Const(c) => c.clone(),
        }
    }

    fn load(&self, place: &'ir Place) -> Const {
        let mut val = self.locals[place.local].clone();

        for elem in &place.elems {
            match elem {
                PlaceElem::Deref => match val {
                    Const::Ptr(to) => {
                        val = *to;
                    }
                    _ => unreachable!(),
                },
                PlaceElem::Field(idx) => match val {
                    Const::Tuple(mut cs) => {
                        val = cs.swap_remove(*idx);
                    }
                    _ => unreachable!(),
                },
                PlaceElem::Index(_idx) => unimplemented!(),
            }
        }

        val
    }

    fn store(&mut self, place: &'ir Place, val: Const) {
        let mut ptr = &mut self.locals[place.local];

        for elem in &place.elems {
            match elem {
                PlaceElem::Deref => match ptr {
                    Const::Ptr(to) => {
                        ptr = &mut **to;
                    }
                    _ => unreachable!(),
                },
                PlaceElem::Field(idx) => match ptr {
                    Const::Tuple(cs) => {
                        ptr = &mut cs[*idx];
                    }
                    Const::Undefined(ty) => {
                        *ptr = init_undefined(ty.clone(), self.target);

                        if let Const::Tuple(cs) = ptr {
                            ptr = &mut cs[*idx];
                        }
                    }
                    _ => unreachable!(),
                },
                PlaceElem::Index(_idx) => unimplemented!(),
            }
        }

        *ptr = val;
    }
}

fn init_undefined(ty: Type, target: &target_lexicon::Triple) -> Const {
    match ty {
        Type::Tuple(tys) => Const::Tuple(tys.into_iter().map(|t| Const::Undefined(t)).collect()),
        Type::Type(t) => Const::Tuple(vec![
            Const::Undefined(layout::ptr_sized_int(target)),
            Const::Undefined(layout::ptr_sized_int(target)),
            Const::Undefined(layout::ptr_sized_int(target)),
            Const::Undefined(Type::Ptr(Box::new(Type::Vwt(t)))),
        ]),
        Type::Vwt(t) => Const::Tuple(vec![
            Const::Undefined(layout::copy_fn_type(&t)),
            Const::Undefined(layout::copy_fn_type(&t)),
            Const::Undefined(layout::drop_fn_type(&t)),
        ]),
        _ => Const::Undefined(ty),
    }
}
