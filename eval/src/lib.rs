#![feature(ptr_metadata)]

pub mod heap;
pub mod stack;
pub mod val;

use arena::{ArenaMap, Idx};

pub struct VM<'a> {
    db: &'a dyn ir::db::IrDatabase,
    module: &'a ir::Module,
    frames: Vec<Frame<'a>>,
    stack: stack::Stack,
    heap: heap::Heap,
}

struct Frame<'a> {
    db: &'a dyn ir::db::IrDatabase,
    module: &'a ir::Module,
    stack: *mut stack::Stack,
    heap: *mut heap::Heap,
    body: ir::BodyId,
    block: ir::Block,
    inst: usize,
    vars: ArenaMap<Idx<ir::VarInfo>, val::Val>,
}

enum Action {
    Abort,
    Apply(ir::BodyId, Vec<val::Val>),
    Return(Vec<val::Val>),
}

#[derive(Debug)]
pub enum VmError {
    Aborted,
    Stack(stack::StackError),
}

impl<'a> VM<'a> {
    pub fn new(db: &'a dyn ir::db::IrDatabase, module: &'a ir::Module) -> Self {
        Self {
            db,
            module,
            frames: Vec::new(),
            stack: stack::Stack::default(),
            heap: heap::Heap::default(),
        }
    }

    pub fn with_max_stack(db: &'a dyn ir::db::IrDatabase, module: &'a ir::Module, max_stack: usize) -> Self {
        Self {
            db,
            module,
            frames: Vec::new(),
            stack: stack::Stack::new(max_stack),
            heap: heap::Heap::default(),
        }
    }

    pub fn apply(&mut self, id: ir::BodyId, args: Vec<val::Val>) -> Result<Vec<val::Val>, VmError> {
        self.run(Action::Apply(id, args))
    }

    fn run(&mut self, mut action: Action) -> Result<Vec<val::Val>, VmError> {
        loop {
            match action {
                | Action::Abort => return Err(VmError::Aborted),
                | Action::Return(vals) => {
                    self.frames.pop().unwrap();

                    if self.frames.is_empty() {
                        return Ok(vals);
                    } else {
                        action = self.frames.last_mut().unwrap().ret(vals)?;
                    }
                },
                | Action::Apply(id, args) => {
                    let body = &self.module[id];
                    let block = ir::Block::ENTRY;
                    let mut vars = ArenaMap::default();

                    for (p, arg) in body[block].params.iter().zip(args) {
                        vars.insert(p.0, arg);
                    }

                    self.frames.push(Frame {
                        db: self.db,
                        module: self.module,
                        stack: &mut self.stack,
                        heap: &mut self.heap,
                        body: id,
                        inst: 0,
                        block,
                        vars,
                    });

                    action = self.frames.last_mut().unwrap().eval()?;
                },
            }
        }
    }
}

impl<'a> Frame<'a> {
    fn stack(&mut self) -> &mut stack::Stack {
        unsafe { &mut *self.stack }
    }

    fn heap(&mut self) -> &mut heap::Heap {
        unsafe { &mut *self.heap }
    }

    fn eval(&mut self) -> Result<Action, VmError> {
        let body = &self.module[self.body];
        let block = &body[self.block];

        loop {
            if self.inst == block.instrs.len() {
                match self.eval_term(block.term.as_ref().unwrap())? {
                    | Some(action) => return Ok(action),
                    | None => self.inst = 0,
                }
            } else {
                match self.eval_inst(&block.instrs[self.inst])? {
                    | Some(action) => return Ok(action),
                    | None => self.inst += 1,
                }
            }
        }
    }

    fn ret(&mut self, vals: Vec<val::Val>) -> Result<Action, VmError> {
        let body = &self.module[self.body];
        let block = &body[self.block];
        let inst = &block.instrs[self.inst];

        if let ir::Instr::Apply { rets, .. } = inst {
            for (r, val) in rets.iter().zip(vals) {
                self.vars.insert(r.0, val);
            }
        }

        self.inst += 1;
        self.eval()
    }

    fn eval_term(&mut self, term: &ir::Term) -> Result<Option<Action>, VmError> {
        match term {
            | ir::Term::Unreachable => Ok(Some(Action::Abort)),
            | ir::Term::Return { vals } => {
                let vals = vals.iter().map(|v| self.vars[v.0].clone()).collect();

                Ok(Some(Action::Return(vals)))
            },
            | ir::Term::Br { to } => {
                self.branch(to);

                Ok(None)
            },
            | ir::Term::Switch { pred, cases, default } => {
                let i = self.vars[pred.0].int();

                for case in cases {
                    if case.val == i as u128 {
                        self.branch(&case.to);

                        return Ok(None);
                    }
                }

                self.branch(default);

                Ok(None)
            },
        }
    }

    fn branch(&mut self, target: &ir::BrTarget) {
        self.block = target.block;

        let body = &self.module[self.body];
        let block = &body[target.block];

        for (p, arg) in block.params.iter().zip(&target.args) {
            self.vars.insert(p.0, self.vars[arg.0].clone());
        }
    }

    fn eval_inst(&mut self, inst: &ir::Instr) -> Result<Option<Action>, VmError> {
        match *inst {
            | ir::Instr::StackAlloc { ret, ty } => {
                let layout = self.db.layout_of(ty);
                let ptr = self.stack().alloc(layout.size.bytes() as usize)?;

                self.vars.insert(ret.0, val::Val::StackPtr(ptr));
            },
            | ir::Instr::BoxAlloc { ret, ty } => {
                let layout = self.db.layout_of(ty);
                let res = self.heap().alloc(layout.size.bytes() as usize);

                self.vars.insert(ret.0, val::Val::Box(res.ptr.ptr(), res.generation));
            },
            | ir::Instr::FuncRef { ret, func } => {
                if let Some(id) = self.module[func].body {
                    self.vars.insert(ret.0, val::Val::Func(val::Func::Local(id)));
                } else {
                    unimplemented!("foreign functions");
                }
            },
            | _ => unimplemented!("{}", inst.display(self.db, &self.module[self.body])),
        }

        Ok(None)
    }
}

impl From<stack::StackError> for VmError {
    fn from(e: stack::StackError) -> Self {
        Self::Stack(e)
    }
}
