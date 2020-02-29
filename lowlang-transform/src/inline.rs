use crate::Transformer;
use lowlang_syntax::*;
use lowlang_syntax::visit::VisitorMut;
use std::collections::BTreeMap;

pub struct Inliner<'t> {
    to_inline: BTreeMap<ItemId, Body<'t>>,
    current_body: Option<*mut Body<'t>>,
    current_block: Option<*mut Block<'t>>,
    changed: bool,
}

impl<'t> Inliner<'t> {
    pub fn new() -> Inliner<'t> {
        Inliner {
            to_inline: BTreeMap::new(),
            current_body: None,
            current_block: None,
            changed: false,
        }
    }

    #[inline]
    fn body(&mut self) -> &mut Body<'t> {
        unsafe { &mut *self.current_body.unwrap() }
    }

    #[inline]
    fn block(&mut self) -> &mut Block<'t> {
        unsafe { &mut *self.current_block.unwrap() }
    }
}

impl<'t> Transformer<'t> for Inliner<'t> {
    fn transform(&mut self, package: &mut Package<'t>) -> bool {
        if self.to_inline.is_empty() {
            for (id, body) in &package.bodies {
                if body.attributes.inline {
                    self.to_inline.insert(*id, body.clone());
                }
            }
        }

        self.visit_package(package);
        self.changed
    }

    fn reset(&mut self) {
        self.current_body = None;
        self.current_block = None;
        self.changed = false;
    }
}

impl<'t> VisitorMut<'t> for Inliner<'t> {
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
        if let Terminator::Call(places, Operand::Constant(Const::FuncAddr(Addr::Id(proc), _)), args, target) = term {
            if let Some(mut inlined) = self.to_inline.get(proc).cloned() {
                let max_local = self.body().max_local_id();
                let max_block = self.body().max_block_id();
                let locals = inlined.locals.iter().map(|(id, _)| (*id, *id + max_local)).collect();
                let rets = places.iter().zip(inlined.rets()).map(|(place, ret)| (ret.id, place.clone())).collect();
                let args = args.iter().zip(inlined.args()).map(|(op, arg)| (arg.id, op.clone())).collect();
                let blocks = inlined.blocks.iter().skip(1).map(|(id, _)| (*id, *id + max_block)).collect();

                replace_ops(&mut inlined, args);
                replace_places(&mut inlined, rets);

                crate::vars::replace(&mut inlined, &locals);

                replace_blocks(&mut inlined, blocks);
                replace_returns(&mut inlined, *target);

                let mut blocks = inlined.blocks.into_iter();
                let (_, first) = blocks.next().unwrap();

                self.block().stmts.extend(first.stmts);
                self.block().term = first.term;
                self.body().locals.extend(inlined.locals.into_iter().filter(|l| l.1.kind == LocalKind::Tmp || l.1.kind == LocalKind::Var));
                self.body().blocks.extend(blocks.map(|(_, b)| (b.id, b)));
                self.changed = true;
            }
        }
    }
}

fn replace_ops<'t>(body: &mut Body<'t>, with: BTreeMap<LocalId, Operand<'t>>) {
    struct Replacer<'t>(BTreeMap<LocalId, Operand<'t>>);

    Replacer(with).visit_body(body);

    impl<'t> VisitorMut<'t> for Replacer<'t> {
        fn visit_op(&mut self, op: &mut Operand<'t>) {
            match op {
                Operand::Place(place) => {
                    match &place.base {
                        PlaceBase::Local(id) => if let Some(new) = self.0.get(id) {
                            if let Operand::Place(new) = new {
                                *place = place.merge(new);
                            } else {
                                *op = new.clone();
                            }
                        },
                        _ => {},
                    }
                },
                _ => {},
            }

            self.super_op(op);
        }
    }
}

fn replace_places(body: &mut Body, with: BTreeMap<LocalId, Place>) {
    struct Replacer(BTreeMap<LocalId, Place>);

    Replacer(with).visit_body(body);

    impl<'t> VisitorMut<'t> for Replacer {
        fn visit_place(&mut self, place: &mut Place) {
            match &place.base {
                PlaceBase::Local(id) => if let Some(new) = self.0.get(id) {
                    *place = place.merge(new);
                },
                _ => {},
            }

            self.super_place(place);
        }
    }
}

fn replace_blocks(body: &mut Body, with: BTreeMap<BlockId, BlockId>) {
    struct Replacer(BTreeMap<BlockId, BlockId>);

    Replacer(with).visit_body(body);

    impl<'t> VisitorMut<'t> for Replacer {
        fn visit_block(&mut self, block: &mut Block<'t>) {
            if let Some(new) = self.0.get(&block.id) {
                block.id = *new;
            }

            self.super_block(block);
        }

        fn visit_term(&mut self, term: &mut Terminator<'t>) {
            match term {
                Terminator::Jump(to) => if let Some(new) = self.0.get(to) {
                    *to = *new;
                },
                Terminator::Call(_, _, _, to) => if let Some(new) = self.0.get(to) {
                    *to = *new;
                },
                Terminator::Switch(_, _, tos) => {
                    for to in tos {
                        if let Some(new) = self.0.get(to) {
                            *to = *new;
                        }
                    }
                },
                _ => {},
            }
        }
    }
}

fn replace_returns(body: &mut Body, target: BlockId) {
    struct Replacer(BlockId);

    Replacer(target).visit_body(body);

    impl<'t> VisitorMut<'t> for Replacer {
        fn visit_term(&mut self, term: &mut Terminator<'t>) {
            if let Terminator::Return = term {
                *term = Terminator::Jump(self.0);
            }
        }
    }
}
