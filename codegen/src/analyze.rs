use super::*;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SsaKind {
    Ssa,
    NotSsa,
}

pub fn analyze<'ctx>(
    fx: &FunctionCtx<'_, 'ctx, '_, impl Backend<'ctx>>,
) -> HashMap<ir::Local, SsaKind> {
    let mut map = fx
        .body
        .locals
        .iter()
        .map(|data| {
            let layout = ir::layout::layout_of(&data.ty, &fx.target);

            if fx.ir_type(&layout).is_some() || fx.ir_pair_type(&layout).is_some() {
                (data.id, SsaKind::Ssa)
            } else {
                (data.id, SsaKind::NotSsa)
            }
        })
        .collect::<HashMap<_, _>>();

    for block in &fx.body.blocks {
        for stmt in &block.stmts {
            if let ir::Stmt::Assign(_, rvalue) = stmt {
                if let ir::RValue::AddrOf(place) = rvalue {
                    map.insert(place.local, SsaKind::NotSsa);
                }
            } else if let ir::Stmt::Call(places, ..) = stmt {
                for place in places {
                    let dest_layout =
                        ir::layout::layout_of(&fx.body.locals[place.local].ty, &fx.target);

                    if !abi::can_return_to_ssa_var(fx, &dest_layout) {
                        map.insert(place.local, SsaKind::NotSsa);
                    }
                }
            }
        }
    }

    map
}
