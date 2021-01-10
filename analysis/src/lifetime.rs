use crate::Analyzer;
use ir::visitor::Visitor;
use std::collections::{HashMap, HashSet};
use transform::Transform;

pub struct LifetimeAnalyzer {
    alive: HashSet<ir::Local>,
    blocks: HashMap<ir::Block, HashSet<ir::Local>>,
    ties: HashMap<ir::Local, ir::Local>,
    annotations: Vec<Annotation>,
}

pub struct Annotation {
    state: bool,
    local: ir::Local,
    loc: ir::Location,
}

pub struct LifetimeAnnotator {
    annotations: Vec<Annotation>,
}

impl LifetimeAnalyzer {
    pub fn new() -> Self {
        LifetimeAnalyzer {
            alive: HashSet::new(),
            blocks: HashMap::new(),
            ties: HashMap::new(),
            annotations: Vec::new(),
        }
    }
}

impl Analyzer for LifetimeAnalyzer {
    type Output = LifetimeAnnotator;

    fn analyze(&mut self, module: &ir::Module) -> Self::Output {
        self.visit_module(module);

        LifetimeAnnotator {
            annotations: std::mem::replace(&mut self.annotations, Vec::new()),
        }
    }
}

impl Transform for LifetimeAnnotator {
    fn apply(&mut self, module: &mut ir::Module) {
        for ann in self.annotations.drain(..).rev() {
            let body = &mut module.bodies[ann.loc.body];

            if let ir::LocalKind::Var | ir::LocalKind::Tmp = body.locals[ann.local].kind {
                let block = &mut body.blocks[ann.loc.block];
                let stmt = if ann.state {
                    ir::Stmt::Init(ann.local)
                } else {
                    ir::Stmt::Drop(ann.local)
                };

                if ann.loc.stmt == block.stmts.len() {
                    block.stmts.push(stmt);
                } else {
                    block.stmts.insert(ann.loc.stmt, stmt);
                }
            }
        }
    }
}

impl Visitor for LifetimeAnalyzer {
    fn visit_body(&mut self, body: &ir::Body) {
        let root = body.blocks.first().unwrap().id;

        self.alive.clear();
        self.blocks.clear();
        self.ties.clear();
        self.find_inits(body, Vec::new(), root);
        println!();
    }
}

impl LifetimeAnalyzer {
    fn reverse_anns(&mut self, start: usize) {
        self.annotations[start..].reverse();
    }

    fn find_inits(&mut self, body: &ir::Body, mut prev: Vec<ir::Block>, block: ir::Block) {
        let data = &body.blocks[block];

        for (i, stmt) in data.stmts.iter().enumerate() {
            let loc = ir::Location {
                body: body.id,
                stmt: i,
                block,
            };

            match stmt {
                ir::Stmt::Init(local) => {
                    self.alive.insert(*local);
                }
                ir::Stmt::Drop(_) => {}
                ir::Stmt::Assign(place, rvalue) => {
                    self.place_lifetime(place, loc, true);
                    self.rvalue_lifetime(rvalue, loc, true);

                    if let ir::RValue::AddrOf(rhs) = rvalue {
                        if place.elems.is_empty() && rhs.elems.is_empty() {
                            self.ties.insert(place.local, rhs.local);
                        }
                    }
                }
                ir::Stmt::SetDiscr(place, _) => {
                    self.place_lifetime(place, loc, true);
                }
                ir::Stmt::Call(rets, func, args) => {
                    for ret in rets {
                        self.place_lifetime(ret, loc, true);
                    }

                    self.op_lifetime(func, loc, true);

                    for arg in args {
                        self.op_lifetime(arg, loc, true);
                    }
                }
            }
        }

        let succ = match &data.term {
            ir::Term::Abort => Vec::new(),
            ir::Term::Return => Vec::new(),
            ir::Term::Jump(next) => vec![*next],
            ir::Term::Switch(op, _, next) => {
                self.op_lifetime(
                    op,
                    ir::Location {
                        body: body.id,
                        stmt: data.stmts.len(),
                        block,
                    },
                    true,
                );

                next.clone()
            }
        };

        if succ.is_empty() {
            let alive = HashSet::with_capacity(self.alive.len());
            let alive = std::mem::replace(&mut self.alive, alive);

            self.find_drops(body, prev, block, block);
            self.alive = alive;
        } else {
            prev.push(block);

            for next in succ {
                self.find_inits(body, prev.clone(), next);
            }
        }
    }

    fn find_drops(
        &mut self,
        body: &ir::Body,
        mut prev: Vec<ir::Block>,
        block: ir::Block,
        next_block: ir::Block,
    ) {
        if let Some(alive) = self.blocks.get(&block) {
            self.alive.extend(alive.iter().copied());

            if let Some(next) = prev.pop() {
                self.find_drops(body, prev, next, block);
            }

            return;
        }

        println!("find_drops {}", block);
        let data = &body.blocks[block];

        match &data.term {
            ir::Term::Abort => {}
            ir::Term::Return => {}
            ir::Term::Jump(_) => {}
            ir::Term::Switch(op, _, _) => {
                self.op_lifetime(
                    op,
                    ir::Location {
                        body: body.id,
                        block: next_block,
                        stmt: 0,
                    },
                    false,
                );
            }
        }

        let start = self.annotations.len();

        for (i, stmt) in data.stmts.iter().enumerate().rev() {
            let loc = ir::Location {
                body: body.id,
                stmt: i + 1,
                block,
            };

            match stmt {
                ir::Stmt::Init(_) => {}
                ir::Stmt::Drop(local) => {
                    self.alive.insert(*local);
                }
                ir::Stmt::Assign(place, rvalue) => {
                    self.place_lifetime(place, loc, false);
                    self.rvalue_lifetime(rvalue, loc, false);
                }
                ir::Stmt::SetDiscr(place, _) => {
                    self.place_lifetime(place, loc, false);
                }
                ir::Stmt::Call(rets, func, args) => {
                    for ret in rets {
                        self.place_lifetime(ret, loc, false);
                    }

                    self.op_lifetime(func, loc, false);

                    for arg in args {
                        self.op_lifetime(arg, loc, false);
                    }
                }
            }
        }

        self.reverse_anns(start);
        self.blocks.insert(block, self.alive.clone());

        if let Some(next) = prev.pop() {
            self.find_drops(body, prev, next, block);
        }
    }

    fn rvalue_lifetime(&mut self, rvalue: &ir::RValue, loc: ir::Location, state: bool) {
        match rvalue {
            ir::RValue::Use(op) => self.op_lifetime(op, loc, state),
            ir::RValue::AddrOf(place) => self.place_lifetime(place, loc, state),
            ir::RValue::GetDiscr(place) => self.place_lifetime(place, loc, state),
            ir::RValue::Cast(place, _) => self.place_lifetime(place, loc, state),
            ir::RValue::Intrinsic(_, args) => {
                for arg in args {
                    self.op_lifetime(arg, loc, state);
                }
            }
        }
    }

    fn place_lifetime(&mut self, place: &ir::Place, loc: ir::Location, state: bool) {
        if !state {
            println!("{}: {}", place.local, self.alive.contains(&place.local));
        }

        if !self.alive.contains(&place.local) {
            self.alive.insert(place.local);
            self.annotations.push(Annotation {
                local: place.local,
                loc,
                state,
            });

            if let Some(tie) = self.ties.get(&place.local) {
                if !self.alive.contains(tie) {
                    self.alive.insert(*tie);
                    self.annotations.push(Annotation {
                        local: *tie,
                        loc,
                        state: false,
                    });
                }
            }
        }

        for elem in &place.elems {
            match elem {
                ir::PlaceElem::Deref => {}
                ir::PlaceElem::Field(_) => {}
                ir::PlaceElem::Index(op) => self.op_lifetime(op, loc, state),
                ir::PlaceElem::Downcast(_) => {}
            }
        }
    }

    fn op_lifetime(&mut self, op: &ir::Operand, loc: ir::Location, state: bool) {
        match op {
            ir::Operand::Place(place) => self.place_lifetime(place, loc, state),
            ir::Operand::Const(_) => {}
        }
    }
}
