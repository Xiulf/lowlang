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
            let block = &mut body.blocks[ann.loc.block];
            let stmt = if ann.state {
                ir::Stmt::VarLive(ann.local)
            } else {
                ir::Stmt::VarDead(ann.local)
            };

            if ann.loc.stmt == block.stmts.len() {
                block.stmts.push(stmt);
            } else {
                block.stmts.insert(ann.loc.stmt, stmt);
            }
        }
    }
}

impl Visitor for LifetimeAnalyzer {}
