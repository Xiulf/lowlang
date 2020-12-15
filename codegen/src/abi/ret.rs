use super::*;
use ir::layout::TyLayout;

pub fn can_return_to_ssa_var<'ctx>(
    fx: &FunctionCtx<'_, 'ctx, '_, impl Backend<'ctx>>,
    dest_layout: &TyLayout,
) -> bool {
    match get_pass_mode(fx, dest_layout) {
        PassMode::NoPass | PassMode::ByVal(_) | PassMode::ByValPair(_, _) => true,
        PassMode::ByRef { size: _ } => false,
    }
}
