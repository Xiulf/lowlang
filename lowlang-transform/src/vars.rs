use crate::Transformer;
use lowlang_syntax::*;
use lowlang_syntax::visit::VisitorMut;
use std::collections::BTreeMap;

pub fn replace(body: &mut Body, with: &BTreeMap<LocalId, LocalId>) {
    Replacer { with }.visit_body(body);
}

pub struct VarRemover {
    count: BTreeMap<LocalId, usize>,
}

pub struct Replacer<'a> {
    with: &'a BTreeMap<LocalId, LocalId>,
}

impl VarRemover {
    pub fn new() -> VarRemover {
        VarRemover {
            count: BTreeMap::new(),
        }
    }
}

impl<'t> Transformer<'t> for VarRemover {
    fn transform(&mut self, package: &mut Package<'t>) -> bool {
        let mut changed = false;

        for (_, body) in &mut package.bodies {
            self.count.clear();

            for arg in body.args() { self.count.insert(arg.id, 1); }
            for ret in body.rets() { self.count.insert(ret.id, 1); }

            self.visit_body(body);

            for (id, count) in &self.count {
                if *count == 0 {
                    body.locals.remove(id);
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

impl<'t> VisitorMut<'t> for VarRemover {
    #[inline]
    fn visit_local(&mut self, local: &mut Local<'t>) {
        if !self.count.contains_key(&local.id) {
            self.count.insert(local.id, 0);
        }
    }

    #[inline]
    fn visit_place(&mut self, place: &mut Place) {
        match &place.base {
            PlaceBase::Local(id) => *self.count.entry(*id).or_default() += 1,
            _ => {},
        }

        self.super_place(place);
    }
}

impl<'a, 't> VisitorMut<'t> for Replacer<'a> {
    #[inline]
    fn visit_body(&mut self, body: &mut Body<'t>) {
        self.super_body(body);

        body.locals = body.locals.iter().map(|(_, l)| (l.id, l.clone())).collect();
    }

    #[inline]
    fn visit_local(&mut self, local: &mut Local<'t>) {
        if let Some(new) = self.with.get(&local.id) {
            local.id = *new;
        }

        self.super_local(local);
    }

    #[inline]
    fn visit_place(&mut self, place: &mut Place) {
        match &mut place.base {
            PlaceBase::Local(id) => if let Some(new) = self.with.get(id) {
                *id = *new;
            },
            _ => {},
        }

        self.super_place(place);
    }
}
