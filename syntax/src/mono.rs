use crate::*;
use crate::visit::VisitorMut;
use crate::util::Mut;
use std::collections::BTreeMap;

pub fn monomorphize<'t>(package: &mut Package<'t>, tcx: &TyCtx<'t>) {
    let mut poly_bodies = BTreeMap::new();

    for (id, body) in &package.bodies {
        if body.is_polymorphic() {
            poly_bodies.insert(*id, body.clone());
        }
    }

    let mut body_subst = BTreeMap::new();

    while monomorphize_one(Mut(package), &poly_bodies, &mut body_subst, tcx) {}

    for (id, _) in poly_bodies {
        package.bodies.remove(&id);
    }
}

type Subst<'t> = BTreeMap<String, GenArg<'t>>;

struct Mono<'a, 't> {
    tcx: &'a TyCtx<'t>,
    package: &'a mut Package<'t>,
    poly_bodies: &'a BTreeMap<ItemId, Body<'t>>,
    body_subst: &'a mut BTreeMap<(ItemId, Subst<'t>), ItemId>,
    next_id: ItemId,
    changed: bool,
}

impl<'a, 't> VisitorMut<'t> for Mono<'a, 't> {
    fn visit_body(&mut self, id: ItemId, body: &mut Body<'t>) {
        if !self.changed && !self.poly_bodies.contains_key(&id) {
            self.super_body(body);
        }
    }
    
    fn visit_const(&mut self, const_: &mut Const<'t>) {
        fn get<'a>(map: &BTreeMap<(ItemId, Subst<'a>), ItemId>, id: &ItemId, subst: &Subst<'a>) -> Option<ItemId> {
            map.iter().filter(|((e_id, e_subst), _)| e_id == id && {
                e_subst.iter().zip(subst.iter()).all(|((ap, at), (bp, bt))| ap == bp && {
                    at == bt
                })
            }).map(|(_, v)| *v).next()
        }

        if !self.changed {
            match const_ {
                Const::FuncAddr(func, subst) if !subst.is_empty() => {
                    if let Some(next_id) = get(self.body_subst, func, subst) {
                        *func = next_id;
                        subst.clear();
                        self.changed = true;
                    } else {
                        self.package.bodies.extend(Some((
                            self.next_id,
                            {
                                let mut body = self.poly_bodies[func].clone();

                                body.generics.clear();
                                Substitue { subst, tcx: self.tcx }.visit_body(self.next_id, &mut body);

                                body
                            },
                        )));

                        self.body_subst.insert((*func, subst.clone()), self.next_id);
                        *func = self.next_id;
                        subst.clear();
                        self.changed = true;
                    }
                },
                _ => {},
            }
        }
    }
}

fn monomorphize_one<'t>(
    package: Mut<Package<'t>>,
    poly_bodies: &BTreeMap<ItemId, Body<'t>>,
    body_subst: &mut BTreeMap<(ItemId, Subst<'t>), ItemId>,
    tcx: &TyCtx<'t>,
) -> bool {
    let mut mono = Mono {
        tcx,
        next_id: package.next_id(),
        changed: false,
        package: package.get_mut(),
        poly_bodies,
        body_subst,
    };

    mono.visit_package(package.get_mut());
    mono.changed
}

struct Substitue<'a, 't> {
    tcx: &'a TyCtx<'t>,
    subst: &'a Subst<'t>,
}

impl<'a, 't> VisitorMut<'t> for Substitue<'a, 't> {
    fn visit_body(&mut self, _id: ItemId, body: &mut Body<'t>) {
        body.name = format!("{}<{}>", body.name, self.subst.iter().map(|(k, v)| format!("{}={}", k, v)).collect::<Vec<_>>().join(", "));
        self.super_body(body);
    }

    fn visit_const(&mut self, const_: &mut Const<'t>) {
        match const_ {
            Const::Param(name) if self.subst.contains_key(name) => match &self.subst[name] {
                GenArg::Const(c) => *const_ = c.clone(),
                GenArg::Type(_) => panic!("expected a constant"),
            },
            _ => {},
        }

        self.super_const(const_);
    }

    fn visit_ty(&mut self, ty: &mut Ty<'t>) {
        *ty = subst_ty(ty, self.subst, self.tcx);
        self.super_ty(ty);
    }
}

fn subst_ty<'t>(ty: &Ty<'t>, subst: &Subst<'t>, tcx: &TyCtx<'t>) -> Ty<'t> {
    match &**ty {
        Type::Param(name) if subst.contains_key(name) => match &subst[name] {
            GenArg::Type(ty) => *ty,
            GenArg::Const(_) => panic!("expected a type"),
        },
        _ => *ty,
    }
}
