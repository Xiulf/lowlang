use crate::Transformer;
use lowlang_syntax::*;
use lowlang_syntax::visit::VisitorMut;

pub struct BlockMerger<'t> {
    current_body: Option<*mut Body<'t>>,
    current_block: Option<*mut Block<'t>>,
    changed: bool,
}

impl<'t> BlockMerger<'t> {
    pub fn new() -> BlockMerger<'t> {
        BlockMerger {
            current_body: None,
            current_block: None,
            changed: false,
        }
    }

    fn body(&mut self) -> &mut Body<'t> {
        unsafe { &mut *self.current_body.unwrap() }
    }

    fn block(&mut self) -> &mut Block<'t> {
        unsafe { &mut *self.current_block.unwrap() }
    }
}

impl<'t> Transformer<'t> for BlockMerger<'t> {
    fn transform(&mut self, package: &mut Package<'t>) -> bool {
        self.visit_package(package);
        self.changed
    }

    fn reset(&mut self) {
        self.current_body = None;
        self.current_block = None;
        self.changed = false;
    }
}

impl<'t> VisitorMut<'t> for BlockMerger<'t> {
    #[inline]
    fn visit_body(&mut self, body: &mut Body<'t>) {
        self.current_body = Some(body);
        self.super_body(body);
    }

    #[inline]
    fn visit_block(&mut self, block: &mut Block<'t>) {
        self.current_block = Some(block);
        self.super_block(block);
    }

    fn visit_term(&mut self, term: &mut Terminator<'t>) {
        if let Terminator::Jump(target) = term {
            let target = self.body().blocks[target].clone();

            self.block().stmts.extend(target.stmts);
            *term = target.term;
            self.changed = true;
        }
    }
}
