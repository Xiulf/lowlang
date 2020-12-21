use crate::Analyzer;
use ir::visitor::Visitor;
use std::collections::HashSet;
use transform::Transform;

pub struct LifetimeAnalyzer {
    alive: HashSet<ir::Local>,
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

        self.block_lifetimes(body, Vec::new(), root);
    }
}

impl LifetimeAnalyzer {
    fn block_lifetimes(&mut self, body: &ir::Body, mut prev: Vec<ir::Block>, block: ir::Block) {
        let data = &body.blocks[block];

        for (i, stmt) in data.stmts.iter().enumerate() {
            let loc = ir::Location {
                body: body.id,
                stmt: i,
                block,
            };

            match stmt {
                ir::Stmt::Init(_) => {}
                ir::Stmt::Drop(_) => {}
                ir::Stmt::Assign(place, rvalue) => {
                    self.place_lifetime(place, loc, true);
                    self.rvalue_lifetime(rvalue, loc, true);
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
    }

    fn rvalue_lifetime(&mut self, rvalue: &ir::RValue, loc: ir::Location, state: bool) {
        match rvalue {
            ir::RValue::Use(op) => self.op_lifetime(op, loc, state),
            ir::RValue::AddrOf(place) => self.place_lifetime(place, loc, state),
            ir::RValue::Cast(place, _) => self.place_lifetime(place, loc, state),
            ir::RValue::Intrinsic(_, args) => {
                for arg in args {
                    self.op_lifetime(arg, loc, state);
                }
            }
        }
    }

    fn place_lifetime(&mut self, place: &ir::Place, loc: ir::Location, state: bool) {
        if !self.alive.contains(&place.local) {
            self.alive.insert(place.local);
            self.annotations.push(Annotation {
                local: place.local,
                loc,
                state,
            });
        }

        for elem in &place.elems {
            match elem {
                ir::PlaceElem::Deref => {}
                ir::PlaceElem::Field(_) => {}
                ir::PlaceElem::Index(op) => self.op_lifetime(op, loc, state),
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
