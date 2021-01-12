use crate::*;
use index_vec::IndexSlice;
use linked_hash_set::LinkedHashSet;

pub struct Diamonds<'ir> {
    blocks: &'ir IndexSlice<Block, [BlockData]>,
    block: Option<Block>,
}

#[derive(Debug, Clone, Copy)]
pub enum Diamond {
    Open { start: Block },
    Closed { start: Block, end: Block },
}

impl Body {
    pub fn diamonds(&self) -> Diamonds {
        Diamonds {
            blocks: &self.blocks,
            block: Some(Block::new(0)),
        }
    }
}

impl BlockData {
    pub fn successors(&self) -> Vec<Block> {
        match &self.term {
            Term::Jump(id) => vec![*id],
            Term::Switch(_, _, blocks) => blocks.clone(),
            _ => Vec::new(),
        }
    }

    pub fn all_successors(&self, blocks: &IndexSlice<Block, [BlockData]>) -> LinkedHashSet<Block> {
        let mut succ = self.successors();
        let mut set = LinkedHashSet::new();

        if succ.is_empty() {
            set.insert(self.id);
        } else if succ.len() == 1 {
            let next = succ.remove(0);

            set.extend(blocks[next].all_successors(blocks));
        } else {
            let mut dias = Diamonds {
                blocks,
                block: Some(self.id),
            };

            let dia = dias.next().unwrap();

            match dia {
                Diamond::Open { .. } => {}
                Diamond::Closed { end, .. } => {
                    set.extend(blocks[end].all_successors(blocks));
                }
            }
        }

        set
    }
}

impl<'ir> Iterator for Diamonds<'ir> {
    type Item = Diamond;

    fn next(&mut self) -> Option<Self::Item> {
        let block = self.block?;
        let mut succ = self.blocks[block].successors();

        if succ.is_empty() {
            self.block = None;

            Some(Diamond::Open { start: block })
        } else if succ.len() == 1 {
            self.block = succ.pop();

            Some(Diamond::Closed {
                start: block,
                end: block,
            })
        } else {
            let mut all_blocks = succ
                .into_iter()
                .map(|s| self.blocks[s].all_successors(self.blocks))
                .fold_first(|a, b| a.intersection(&b).copied().collect())
                .unwrap();

            if let Some(end) = all_blocks.pop_front() {
                self.block = Some(end);

                Some(Diamond::Closed { start: block, end })
            } else {
                self.block = None;

                Some(Diamond::Open { start: block })
            }
        }
    }
}
