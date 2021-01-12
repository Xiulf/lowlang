use crate::Analyzer;
use ir::graph::Diamond;
use ir::visitor::Visitor;
use std::collections::{HashMap, HashSet};
use transform::Transform;

pub struct LifetimeAnalyzer {
    alive: HashSet<ir::Local>,
    dead: HashSet<ir::Local>,
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
            dead: HashSet::new(),
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
        self.alive.clear();
        self.dead.clear();
        self.ties.clear();

        let dias = body.diamonds().collect::<Vec<_>>();

        for (i, &dia) in dias.iter().enumerate() {
            self.find_inits(body, dia, &dias[i + 1..]);
        }

        for (i, &dia) in dias.iter().enumerate().rev() {
            self.find_drops(body, dia, &dias[..i]);
        }

        println!();
    }
}

impl LifetimeAnalyzer {
    fn find_inits(&mut self, body: &ir::Body, dia: Diamond, dias: &[Diamond]) {
        match dia {
            Diamond::Closed { start, end } => {
                let mut vars = HashSet::new();
                let mut after = HashSet::new();

                Self::find_vars(body, dia, false, &mut vars);
                Self::find_vars(body, dias[0], true, &mut after);

                let both = vars.intersection(&after);

                for &local in both {
                    self.alive.insert(local);
                    self.annotations.push(Annotation {
                        local,
                        state: true,
                        loc: ir::Location {
                            body: body.id,
                            block: start,
                            stmt: 0,
                        },
                    });
                }

                self.find_inits_block(body, start, Some(end));
            }
            Diamond::Open { start } => {
                self.find_inits_block(body, start, None);
            }
        }
    }

    fn find_inits_block(&mut self, body: &ir::Body, block: ir::Block, end: Option<ir::Block>) {
        if Some(block) == end {
            return;
        }

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

        match &data.term {
            ir::Term::Switch(op, _, _) => {
                self.op_lifetime(
                    op,
                    ir::Location {
                        body: body.id,
                        stmt: data.stmts.len(),
                        block,
                    },
                    true,
                );
            }
            _ => {}
        }
    }

    fn find_drops(&mut self, body: &ir::Body, dia: Diamond, dias: &[Diamond]) {
        match dia {
            Diamond::Closed { .. } => {
                println!("find_drops: {:?}", dia);
            }
            Diamond::Open { start } => {
                self.find_drops_block(body, start, None);
            }
        }
    }

    fn find_drops_block(&mut self, body: &ir::Body, block: ir::Block, end: Option<ir::Block>) {
        let data = &body.blocks[block];
        let alive = std::mem::replace(&mut self.alive, HashSet::new());

        match &data.term {
            ir::Term::Switch(op, _, _) => {
                self.op_lifetime(
                    op,
                    ir::Location {
                        body: body.id,
                        block: end.unwrap(),
                        stmt: 0,
                    },
                    false,
                );
            }
            _ => {}
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
                ir::Stmt::Drop(_) => {}
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

        let _ = std::mem::replace(&mut self.alive, alive);
    }

    fn find_vars(body: &ir::Body, dia: Diamond, all: bool, vars: &mut HashSet<ir::Local>) {
        match dia {
            Diamond::Closed { start, end } => {
                Self::find_vars_block(body, start, if all { None } else { Some(end) }, vars);
            }
            Diamond::Open { start } => Self::find_vars_block(body, start, None, vars),
        }
    }

    fn find_vars_block(
        body: &ir::Body,
        block: ir::Block,
        end: Option<ir::Block>,
        vars: &mut HashSet<ir::Local>,
    ) {
        if Some(block) == end {
            return;
        }

        let data = &body.blocks[block];

        for stmt in &data.stmts {
            match stmt {
                ir::Stmt::Init(_) => {}
                ir::Stmt::Drop(_) => {}
                ir::Stmt::Assign(place, rvalue) => {
                    Self::find_vars_place(place, vars);
                    Self::find_vars_rvalue(rvalue, vars);
                }
                ir::Stmt::SetDiscr(place, _) => {
                    Self::find_vars_place(place, vars);
                }
                ir::Stmt::Call(rets, func, args) => {
                    for ret in rets {
                        Self::find_vars_place(ret, vars);
                    }

                    Self::find_vars_op(func, vars);

                    for arg in args {
                        Self::find_vars_op(arg, vars);
                    }
                }
            }
        }

        for next in data.successors() {
            Self::find_vars_block(body, next, end, vars);
        }
    }

    fn find_vars_place(place: &ir::Place, vars: &mut HashSet<ir::Local>) {
        vars.insert(place.local);

        for elem in &place.elems {
            if let ir::PlaceElem::Index(op) = elem {
                Self::find_vars_op(op, vars);
            }
        }
    }

    fn find_vars_op(op: &ir::Operand, vars: &mut HashSet<ir::Local>) {
        if let ir::Operand::Place(place) = op {
            Self::find_vars_place(place, vars);
        }
    }

    fn find_vars_rvalue(rvalue: &ir::RValue, vars: &mut HashSet<ir::Local>) {
        match rvalue {
            ir::RValue::Use(op) => Self::find_vars_op(op, vars),
            ir::RValue::AddrOf(place) => Self::find_vars_place(place, vars),
            ir::RValue::GetDiscr(place) => Self::find_vars_place(place, vars),
            ir::RValue::Cast(place, _) => Self::find_vars_place(place, vars),
            ir::RValue::Intrinsic(_, args) => {
                for arg in args {
                    Self::find_vars_op(arg, vars);
                }
            }
        }
    }

    fn reverse_anns(&mut self, start: usize) {
        self.annotations[start..].reverse();
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
