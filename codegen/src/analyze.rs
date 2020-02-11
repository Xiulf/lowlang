use crate::{FunctionCtx, Backend};
use std::collections::BTreeMap;

#[derive(PartialEq)]
pub enum SsaKind {
    Ssa,
    NotSsa,
}

pub fn analyze(fx: &FunctionCtx<impl Backend>) -> BTreeMap<syntax::LocalId, SsaKind> {
    let mut mapping = fx.body.locals.iter().map(|(id, local)| {
        if fx.clif_type(local.ty.layout(fx.layouts)).is_some() {
            (*id, SsaKind::Ssa)
        } else {
            (*id, SsaKind::NotSsa)
        }
    }).collect();

    for (_, block) in &fx.body.blocks {
        for stmt in &block.stmts {
            match stmt {
                syntax::Stmt::Assign(_, value) => match value {
                    syntax::Value::Ref(place) => {
                        analyze_non_ssa_place(&mut mapping, place);
                    },
                    _ => {},
                },
            }
        }
    }

    mapping
}

fn analyze_non_ssa_place(mapping: &mut BTreeMap<syntax::LocalId, SsaKind>, place: &syntax::Place) {
    match &place.base {
        syntax::PlaceBase::Local(id) => {
            mapping.insert(*id, SsaKind::NotSsa);
        },
        syntax::PlaceBase::Global(_) => {},
    }
}
