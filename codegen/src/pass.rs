use crate::{FunctionCtx, Backend};
use crate::ptr::Pointer;
use crate::value::Value;
use syntax::layout::Layout;
use intern::Intern;
use cranelift_codegen::ir;
use cranelift_module::Module;

pub enum PassMode {
    ByRef,
    ByVal(ir::Type),
    NoPass,
}

pub fn pass_mode(module: &Module<impl Backend>, layout: Layout) -> PassMode {
    match &*syntax::Type::untern(layout.details().ty) {
        syntax::Type::Bool |
        syntax::Type::Char |
        syntax::Type::Int(_) |
        syntax::Type::UInt(_) |
        syntax::Type::Float(_) |
        syntax::Type::Ref(_) |
        syntax::Type::Proc(_) => PassMode::ByVal(crate::clif_type(module, layout).unwrap()),
        _ if layout.details().size == 0 => PassMode::NoPass,
        _ => PassMode::ByRef,
    }
}

pub fn value_for_param(
    fx: &mut FunctionCtx<impl Backend>,
    start_ebb: ir::Ebb,
    layout: Layout
) -> Option<Value> {
    match pass_mode(fx.module, layout) {
        PassMode::NoPass => None,
        PassMode::ByVal(clif_type) => {
            let ebb_param = fx.builder.append_ebb_param(start_ebb, clif_type);

            Some(Value::new_val(ebb_param, layout))
        },
        PassMode::ByRef => {
            let ebb_param = fx.builder.append_ebb_param(start_ebb, fx.pointer_type);

            Some(Value::new_ref(Pointer::addr(ebb_param), layout))
        },
    }
}

pub fn value_for_arg(
    fx: &mut FunctionCtx<impl Backend>,
    arg: Value
) -> Option<ir::Value> {
    match pass_mode(fx.module, arg.layout) {
        PassMode::ByVal(_) => Some(arg.load_scalar(fx)),
        PassMode::ByRef => Some(arg.on_stack(fx).get_addr(fx)),
        PassMode::NoPass => None,
    }
}

pub fn call_sig(
    module: &Module<impl Backend>,
    sig: &syntax::Signature
) -> ir::Signature {
    let mut sign = module.make_signature();
    
    for ret in &sig.2 {
        match pass_mode(module, ret.layout()) {
            PassMode::NoPass => {},
            PassMode::ByVal(ty) => sign.returns.push(ir::AbiParam::new(ty)),
            PassMode::ByRef => sign.params.push(ir::AbiParam::new(module.target_config().pointer_type())),
        }
    }

    for param in &sig.1 {
        match pass_mode(module, param.layout()) {
            PassMode::NoPass => {},
            PassMode::ByVal(ty) => sign.params.push(ir::AbiParam::new(ty)),
            PassMode::ByRef => sign.params.push(ir::AbiParam::new(module.target_config().pointer_type())),
        }
    }

    sign
}
