use crate::{FunctionCtx, Backend};
use lowlang_syntax as ast;
use std::collections::BTreeMap;

#[derive(PartialEq)]
pub enum SsaKind {
    Ssa,
    NotSsa,
}

pub fn analyze(fx: &FunctionCtx<impl Backend>) -> BTreeMap<ast::LocalId, SsaKind> {
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
                ast::Stmt::Assign(_, value) => match value {
                    ast::Value::Ref(place) => {
                        analyze_non_ssa_place(&mut mapping, &place);
                    },
                    _ => {},
                },
            }
        }
    }

    mapping
}

fn analyze_non_ssa_place(mapping: &mut BTreeMap<ast::LocalId, SsaKind>, place: &ast::Place) {
    match &place.base {
        ast::PlaceBase::Local(id) => {
            mapping.insert(*id, SsaKind::NotSsa);
        },
        ast::PlaceBase::Global(_) => {},
    }
}
