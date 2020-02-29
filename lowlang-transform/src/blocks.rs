use crate::Transformer;
use lowlang_syntax::*;
use lowlang_syntax::visit::Visitor;
use std::collections::BTreeMap;

pub struct BlockRemover {
    count: BTreeMap<BlockId, usize>,
}

impl BlockRemover {
    pub fn new() -> BlockRemover {
        BlockRemover {
            count: BTreeMap::new(),
        }
    }
}

impl<'t> Transformer<'t> for BlockRemover {
    fn transform(&mut self, package: &mut Package<'t>) -> bool {
        let mut changed = false;

        for (_, body) in &mut package.bodies {
            self.count.clear();
            self.count.insert(BlockId::FIRST, 1);

            self.visit_body(body);

            for (id, count) in &self.count {
                if *count == 0 {
                    body.blocks.remove(id);
                    changed = true;
                }
            }
        }

        changed
    }

    fn reset(&mut self) {
        self.count.clear();
    }
}

impl<'t> Visitor<'t> for BlockRemover {
    #[inline]
    fn visit_block(&mut self, block: &Block<'t>) {
        if !self.count.contains_key(&block.id) {
            self.count.insert(block.id, 0);
        }

        self.super_block(block);
    }

    #[inline]
    fn visit_term(&mut self, term: &Terminator<'t>) {
        match term {
            Terminator::Jump(target) => *self.count.entry(*target).or_default() += 1,
            Terminator::Call(_, _, _, target) => *self.count.entry(*target).or_default() += 1,
            Terminator::Switch(_, _, targets) => {
                for target in targets {
                    *self.count.entry(*target).or_default() += 1;
                }
            },
            _ => {},
        }
    }
}
