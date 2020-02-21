use crate::*;
use crate::visit::VisitorMut;
use std::collections::HashMap;

pub fn post_process(package: &mut Package) {
    let mut names = HashMap::new();

    for (id, extern_) in &package.externs {
        match extern_ {
            Extern::Proc(name, _) => { names.insert(name.clone(), (*id, false)); },
            Extern::Global(name, _) => { names.insert(name.clone(), (*id, true)); },
        }
    }

    for (id, global) in &package.globals {
        names.insert(global.name.clone(), (*id, true));
    }

    for (id, body) in &package.bodies {
        names.insert(body.name.clone(), (*id, false));
    }

    Replace { names, c2g: None, g2c: None }.visit_package(package);
}

struct Replace {
    names: HashMap<String, (ItemId, bool)>,
    c2g: Option<ItemId>,
    g2c: Option<ItemId>,
}

impl<'t> VisitorMut<'t> for Replace {
    fn visit_place(&mut self, place: &mut Place) {
        match &mut place.base {
            PlaceBase::Global(addr) => match addr {
                Addr::Name(name) => {
                    if !self.names[name].1 {
                        self.g2c = Some(self.names[name].0);
                    } else {
                        *addr = Addr::Id(self.names[name].0);
                    }
                },
                _ => {},
            },
            _ => {},
        }

        self.super_place(place);
    }

    fn visit_op(&mut self, op: &mut Operand<'t>) {
        self.super_op(op);

        if let Some(id) = self.c2g.take() {
            *op = Operand::Place(Place {
                base: PlaceBase::Global(Addr::Id(id)),
                elems: Vec::new(),
            });
        }

        if let Some(id) = self.g2c.take() {
            *op = Operand::Constant(Const::FuncAddr(Addr::Id(id), Default::default()));
        }
    }

    fn visit_const(&mut self, const_: &mut Const<'t>) {
        match const_ {
            Const::FuncAddr(addr, _) => match addr {
                Addr::Name(name) => {
                    if self.names[name].1 {
                        self.c2g = Some(self.names[name].0);
                    } else {
                        *addr = Addr::Id(self.names[name].0);
                    }
                },
                _ => {},
            },
            _ => {},
        }
    }
}
