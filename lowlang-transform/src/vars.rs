use lowlang_syntax::*;
use lowlang_syntax::visit::VisitorMut;
use std::collections::BTreeMap;

pub fn replace(body: &mut Body, with: &BTreeMap<LocalId, LocalId>) {
    Replacer { with }.visit_body(body);
}

pub struct Replacer<'a> {
    with: &'a BTreeMap<LocalId, LocalId>,
}

impl<'a, 't> VisitorMut<'t> for Replacer<'a> {
    fn visit_body(&mut self, body: &mut Body<'t>) {
        self.super_body(body);

        body.locals = body.locals.iter().map(|(_, l)| (l.id, l.clone())).collect();
    }

    fn visit_local(&mut self, local: &mut Local<'t>) {
        if let Some(new) = self.with.get(&local.id) {
            local.id = *new;
        }

        self.super_local(local);
    }

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
