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
    body_subst: &'a mut BTreeMap<(ItemId, String), ItemId>,
    next_id: ItemId,
    changed: bool,
}

fn subst_string(name: &str, subst: &Subst) -> String {
    format!("{}<{}>", name, subst.iter().map(|(k, v)| format!("{}={}", k, v)).collect::<Vec<_>>().join(", "))
}

impl<'a, 't> VisitorMut<'t> for Mono<'a, 't> {
    fn visit_body(&mut self, id: ItemId, body: &mut Body<'t>) {
        if !self.changed && !self.poly_bodies.contains_key(&id) {
            self.super_body(body);
        }
    }
    
    fn visit_const(&mut self, const_: &mut Const<'t>) {
        if !self.changed {
            match const_ {
                Const::FuncAddr(func, subst) if !subst.is_empty() => {
                    let name = subst_string(&self.poly_bodies[&func.id()].name, subst);

                    if let Some(next_id) = self.body_subst.get(&(func.id(), name.clone())) {
                        *func.id_mut() = *next_id;
                        subst.clear();
                        self.changed = true;
                    } else {
                        self.package.bodies.extend(Some((
                            self.next_id,
                            {
                                let mut body = self.poly_bodies[&func.id()].clone();

                                body.generics.clear();
                                Substitue { subst, tcx: self.tcx }.visit_body(self.next_id, &mut body);

                                body
                            },
                        )));

                        self.body_subst.insert((func.id(), name), self.next_id);
                        *func.id_mut() = self.next_id;
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
    body_subst: &mut BTreeMap<(ItemId, String), ItemId>,
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
        body.name = subst_string(&body.name, self.subst);
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
    use intern::Intern;

    match &**ty {
        Type::Param(name) if subst.contains_key(name) => match &subst[name] {
            GenArg::Type(ty) => *ty,
            GenArg::Const(_) => panic!("expected a type"),
        },
        Type::Ref(to) => {
            let new_to = subst_ty(to, subst, tcx);

            if to == &new_to {
                *ty
            } else {
                Type::Ref(new_to).intern(tcx)
            }
        },
        Type::Slice(of) => {
            let new_of = subst_ty(of, subst, tcx);

            if of == &new_of {
                *ty
            } else {
                Type::Slice(new_of).intern(tcx)
            }
        },
        Type::Array(of, len) => {
            let new_of = subst_ty(of, subst, tcx);

            if of == &new_of {
                *ty
            } else {
                Type::Array(new_of, *len).intern(tcx)
            }
        },
        Type::Vector(of, len) => {
            let new_of = subst_ty(of, subst, tcx);

            if of == &new_of {
                *ty
            } else {
                Type::Vector(new_of, *len).intern(tcx)
            }
        },
        Type::Proc(sig) => {
            let mut changed = false;

            let new_params = sig.1.iter().map(|p| {
                let p2 = subst_ty(p, subst, tcx);

                if &p2 != p {
                    changed = true;
                }

                p2
            }).collect();

            let new_rets = sig.2.iter().map(|r| {
                let r2 = subst_ty(r, subst, tcx);

                if &r2 != r {
                    changed = true;
                }

                r2
            }).collect();

            if changed {
                Type::Proc(Signature(sig.0, new_params, new_rets)).intern(tcx)
            } else {
                *ty
            }
        },
        _ => *ty,
    }
}
