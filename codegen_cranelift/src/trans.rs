use crate::*;
use clif::{InstBuilder, Module};

macro_rules! intrinsic {
    (
        $fx:ident, $name:ident, $args:expr, $place:ident, [
            $(($($tokens:tt)+)),*$(,)?
        ]
    ) => {
        intrinsic!(@munch $fx, $name, $args, $place, [$(($($tokens)*),)*], []);
    };
    (@munch $fx:ident, $name:ident, $args:expr, $place:ident, [], [$($p:pat => $val:expr,)*]) => {
        match $name.as_str() {
            $($p => $val,)*
            _ => panic!("unknown intrinsic {}", $name),
        }
    };
    (@munch $fx:ident, $name:ident, $args:expr, $place:ident, [(simple $n:literal ($($params:ident),*) => $func:ident), $($rest:tt)*], [$($out:tt)*]) => {
        intrinsic!(@munch $fx, $name, $args, $place, [$($rest)*], [$($out)*
            $n => {
                if let [$($params),*] = $args {
                    let val = $fx.bcx.ins().$func($($params),*);
                    let val = value::Value::new_val(val, $place.layout.clone());

                    $place.store($fx, val);
                } else {
                    panic!("incorrect number of arguments for intrinsic {}", $n);
                }
            },
        ]);
    };
    (@munch $fx:ident, $name:ident, $args:expr, $place:ident, [(complex $n:literal ($($params:ident),*) => $func:expr), $($rest:tt)*], [$($out:tt)*]) => {
        intrinsic!(@munch $fx, $name, $args, $place, [$($rest)*], [$($out)*
            $n => {
                if let [$($params),*] = $args {
                    let val = $func;

                    $place.store($fx, val);
                } else {
                    panic!("incorrect number of arguments for intrinsic {}", $n);
                }
            },
        ]);
    };
}

impl<'ctx> TransMethods<'ctx> for ClifBackend<'ctx> {
    type Backend = Self;

    fn switch_to_block(fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>, block: clif::Block) {
        fx.bcx.switch_to_block(block);
    }

    fn trans_init(
        _fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>,
        _place: place::Place<'ctx>,
    ) {
    }

    fn trans_drop(
        _fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>,
        _place: place::Place<'ctx>,
    ) {
    }

    fn trans_place(
        fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>,
        place: &ir::Place,
    ) -> place::Place<'ctx> {
        let mut res = fx.locals[&place.local].clone();

        for elem in &place.elems {
            match elem {
                ir::PlaceElem::Deref => res = res.deref(fx),
                ir::PlaceElem::Field(idx) => res = res.field(fx, *idx),
                ir::PlaceElem::Index(idx) => {
                    let idx = Self::trans_op(fx, idx, None);

                    res = res.index(fx, idx);
                }
            }
        }

        res
    }

    fn trans_const(
        fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>,
        c: &ir::Const,
        into: Option<place::Place<'ctx>>,
    ) -> value::Value<'ctx> {
        let ty = ir::const_type(fx.ir, c);
        let layout = ir::layout::layout_of(&ty, &fx.target);

        if let Some(into) = into {
            match c {
                ir::Const::Undefined(_) => into.to_value(fx),
                ir::Const::Scalar(s, _) => {
                    let val = value::Value::new_const(*s, fx, layout);

                    into.store(fx, val.clone());
                    val
                }
                ir::Const::Tuple(vals) if vals.is_empty() => value::Value::new_unit(),
                _ => unimplemented!(),
            }
        } else {
            match c {
                ir::Const::Undefined(_) => {
                    let slot = fx.bcx.create_stack_slot(clif::StackSlotData::new(
                        clif::StackSlotKind::ExplicitSlot,
                        layout.size.bytes() as u32,
                    ));

                    value::Value::new_ref(ptr::Pointer::stack(slot), layout)
                }
                ir::Const::Scalar(s, _) => value::Value::new_const(*s, fx, layout),
                ir::Const::Tuple(vals) if vals.is_empty() => value::Value::new_unit(),
                ir::Const::Addr(decl) => {
                    if let Some((func, _)) = fx.func_ids.get(decl) {
                        let ptr_type = fx.module.target_config().pointer_type();
                        let func = fx.mcx.module.declare_func_in_func(*func, &mut fx.bcx.func);
                        let func = fx.bcx.ins().func_addr(ptr_type, func);

                        value::Value::new_val(func, layout)
                    } else {
                        let data = fx.data_ids[decl];
                        let ptr_type = fx.module.target_config().pointer_type();
                        let global = fx.mcx.module.declare_data_in_func(data, &mut fx.bcx.func);
                        let global = fx.bcx.ins().global_value(ptr_type, global);

                        value::Value::new_val(global, layout)
                    }
                }
                _ => unimplemented!(),
            }
        }
    }

    fn trans_rvalue(
        fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>,
        place: place::Place<'ctx>,
        rvalue: &ir::RValue,
    ) {
        match rvalue {
            ir::RValue::Use(op) => {
                Self::trans_op(fx, op, Some(place));
            }
            ir::RValue::AddrOf(val) => {
                let val = Self::trans_place(fx, val);

                val.write_place_ref(fx, place);
            }
            ir::RValue::Cast(val, to) => {
                let layout = ir::layout::layout_of(to, &fx.target);
                let val = Self::trans_place(fx, val).to_value(fx);
                let val = val.cast(fx, layout);

                place.store(fx, val);
            }
            ir::RValue::Intrinsic(name, args) => {
                let args = args
                    .iter()
                    .map(|a| {
                        let a = Self::trans_op(fx, a, None);

                        value_for_arg!(
                            fx,
                            a,
                            match a.on_stack(fx) {
                                (ptr, None) => abi::EmptySinglePair::Single(ptr.get_addr(fx)),
                                (ptr, Some(meta)) =>
                                    abi::EmptySinglePair::Pair(ptr.get_addr(fx), meta),
                            }
                        )
                    })
                    .flatten()
                    .collect::<Vec<_>>();

                intrinsic!(fx, name, args[..], place, [
                    (simple "add_i8"(a, b) => iadd),
                    (simple "sub_i8"(a, b) => isub),
                    (simple "mul_i8"(a, b) => imul),
                    (simple "div_i8"(a, b) => sdiv),
                    (simple "rem_i8"(a, b) => srem),
                    (simple "add_i16"(a, b) => iadd),
                    (simple "sub_i16"(a, b) => isub),
                    (simple "mul_i16"(a, b) => imul),
                    (simple "div_i16"(a, b) => sdiv),
                    (simple "rem_i16"(a, b) => srem),
                    (simple "add_i32"(a, b) => iadd),
                    (simple "sub_i32"(a, b) => isub),
                    (simple "mul_i32"(a, b) => imul),
                    (simple "div_i32"(a, b) => sdiv),
                    (simple "rem_i32"(a, b) => srem),
                    (simple "add_i64"(a, b) => iadd),
                    (simple "sub_i64"(a, b) => isub),
                    (simple "mul_i64"(a, b) => imul),
                    (simple "div_i64"(a, b) => sdiv),
                    (simple "rem_i64"(a, b) => srem),
                    (simple "add_i128"(a, b) => iadd),
                    (simple "sub_i128"(a, b) => isub),
                    (simple "mul_i128"(a, b) => imul),
                    (simple "div_i128"(a, b) => sdiv),
                    (simple "rem_i128"(a, b) => srem),
                    (simple "add_u8"(a, b) => iadd),
                    (simple "sub_u8"(a, b) => isub),
                    (simple "mul_u8"(a, b) => imul),
                    (simple "div_u8"(a, b) => udiv),
                    (simple "rem_u8"(a, b) => urem),
                    (simple "add_u16"(a, b) => iadd),
                    (simple "sub_u16"(a, b) => isub),
                    (simple "mul_u16"(a, b) => imul),
                    (simple "div_u16"(a, b) => udiv),
                    (simple "rem_u16"(a, b) => urem),
                    (simple "add_u32"(a, b) => iadd),
                    (simple "sub_u32"(a, b) => isub),
                    (simple "mul_u32"(a, b) => imul),
                    (simple "div_u32"(a, b) => udiv),
                    (simple "rem_u32"(a, b) => urem),
                    (simple "add_u64"(a, b) => iadd),
                    (simple "sub_u64"(a, b) => isub),
                    (simple "mul_u64"(a, b) => imul),
                    (simple "div_u64"(a, b) => udiv),
                    (simple "rem_u64"(a, b) => urem),
                    (simple "add_u128"(a, b) => iadd),
                    (simple "sub_u128"(a, b) => isub),
                    (simple "mul_u128"(a, b) => imul),
                    (simple "div_u128"(a, b) => udiv),
                    (simple "rem_u128"(a, b) => urem),
                    (complex "lt_i32"(a, b) => {
                        let val = fx.bcx.ins().icmp(clif::IntCC::SignedLessThan, a, b);
                        let val = fx.bcx.ins().bint(clif::types::I8, val);
                        value::Value::new_val(val, place.layout.clone())
                    }),
                    (complex "le_i32"(a, b) => {
                        let val = fx.bcx.ins().icmp(clif::IntCC::SignedLessThanOrEqual, a, b);
                        let val = fx.bcx.ins().bint(clif::types::I8, val);
                        value::Value::new_val(val, place.layout.clone())
                    }),
                    (complex "gt_i32"(a, b) => {
                        let val = fx.bcx.ins().icmp(clif::IntCC::SignedGreaterThan, a, b);
                        let val = fx.bcx.ins().bint(clif::types::I8, val);
                        value::Value::new_val(val, place.layout.clone())
                    }),
                    (complex "ge_i32"(a, b) => {
                        let val = fx.bcx.ins().icmp(clif::IntCC::SignedGreaterThanOrEqual, a, b);
                        let val = fx.bcx.ins().bint(clif::types::I8, val);
                        value::Value::new_val(val, place.layout.clone())
                    }),
                    (complex "eq_i32"(a, b) => {
                        let val = fx.bcx.ins().icmp(clif::IntCC::Equal, a, b);
                        let val = fx.bcx.ins().bint(clif::types::I8, val);
                        value::Value::new_val(val, place.layout.clone())
                    }),
                    (complex "ne_i32"(a, b) => {
                        let val = fx.bcx.ins().icmp(clif::IntCC::NotEqual, a, b);
                        let val = fx.bcx.ins().bint(clif::types::I8, val);
                        value::Value::new_val(val, place.layout.clone())
                    }),
                    (complex "memcpy"(dst, src, n) => {
                        fx.bcx.call_memcpy(fx.mcx.module.target_config(), dst, src, n);
                        value::Value::new_unit()
                    }),
                    (simple "ptr_offset"(otr, offset) => iadd),
                    (complex "stack_alloc"(n) => {
                        let mut malloc = fx.module.make_signature();
                        let ptr_type = fx.module.target_config().pointer_type();

                        malloc.returns.push(clif::AbiParam::new(ptr_type));
                        malloc.params.push(clif::AbiParam::new(ptr_type));

                        let malloc = fx.mcx.module.declare_function("malloc", clif::Linkage::Import, &malloc).unwrap();
                        let malloc = fx.mcx.module.declare_func_in_func(malloc, &mut fx.bcx.func);
                        let inst = fx.bcx.ins().call(malloc, &[n]);
                        let val = fx.bcx.inst_results(inst)[0];

                        value::Value::new_val(val, place.layout.clone())
                    }),
                    (complex "stack_free"(ptr) => {
                        let mut free = fx.module.make_signature();
                        let ptr_type = fx.module.target_config().pointer_type();

                        free.params.push(clif::AbiParam::new(ptr_type));

                        let free = fx.mcx.module.declare_function("free", clif::Linkage::Import, &free).unwrap();
                        let free = fx.mcx.module.declare_func_in_func(free, &mut fx.bcx.func);

                        fx.bcx.ins().call(free, &[ptr]);
                        value::Value::new_unit()
                    }),
                ]);
            }
        }
    }

    fn trans_term(fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>, term: &ir::Term) {
        match term {
            ir::Term::Abort => {
                fx.bcx.ins().trap(clif::TrapCode::User(0));
            }
            ir::Term::Return => {
                let rets = fx
                    .body
                    .rets()
                    .map(|r| abi::value_for_ret(fx, r.id))
                    .flatten()
                    .collect::<Vec<_>>();

                fx.bcx.ins().return_(&rets);
            }
            ir::Term::Jump(to) => {
                fx.bcx.ins().jump(fx.blocks[to], &[]);
            }
            ir::Term::Switch(op, vals, blocks) => {
                let mut switch = clif::Switch::new();
                let otherwise = fx.blocks[blocks.last().unwrap()];
                let val = Self::trans_op(fx, op, None).load_scalar(fx);

                for (val, block) in vals.iter().zip(blocks) {
                    switch.set_entry(*val, fx.blocks[block]);
                }

                switch.emit(&mut fx.bcx, val, otherwise)
            }
        }
    }

    fn trans_call(
        fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>,
        rets: Vec<place::Place<'ctx>>,
        func: &ir::Operand,
        args: Vec<value::Value<'ctx>>,
    ) {
        let ret_modes = rets
            .into_iter()
            .map(|r| (abi::get_pass_mode(fx.mcx, r.layout()), r))
            .collect::<Vec<_>>();

        let ret_ptrs = ret_modes
            .iter()
            .filter_map(|(m, p)| match m {
                abi::PassMode::ByRef { size: _ } => Some(p.as_ptr().get_addr(fx)),
                _ => None,
            })
            .collect::<Vec<_>>();

        let args = ret_ptrs
            .into_iter()
            .chain(
                args.into_iter()
                    .map(|a| {
                        value_for_arg!(
                            fx,
                            a,
                            match a.on_stack(fx) {
                                (ptr, None) => abi::EmptySinglePair::Single(ptr.get_addr(fx)),
                                (ptr, Some(meta)) => {
                                    abi::EmptySinglePair::Pair(ptr.get_addr(fx), meta)
                                }
                            }
                        )
                    })
                    .flatten(),
            )
            .collect::<Vec<_>>();

        let inst = if let ir::Operand::Const(ir::Const::Addr(id)) = func {
            let func = fx.func_ids[id].0;
            let func = fx.mcx.module.declare_func_in_func(func, &mut fx.bcx.func);

            fx.bcx.ins().call(func, &args)
        } else {
            let func_ty = ir::operand_type(fx.ir, fx.body, func);
            let sig = crate::mk_signature(fx.mcx, &func_ty.signature());
            let sig = fx.bcx.import_signature(sig);
            let func = Self::trans_op(fx, func, None).load_scalar(fx);

            fx.bcx.ins().call_indirect(sig, func, &args)
        };

        let mut res = fx
            .bcx
            .inst_results(inst)
            .iter()
            .copied()
            .collect::<Vec<_>>()
            .into_iter();

        for (ret_mode, place) in ret_modes {
            match ret_mode {
                abi::PassMode::NoPass => {}
                abi::PassMode::ByRef { .. } => {}
                abi::PassMode::ByVal(_) => {
                    let ret_val = res.next().unwrap();
                    let ret_val = value::Value::new_val(ret_val, place.layout.clone());

                    place.store(fx, ret_val);
                }
                abi::PassMode::ByValPair(_, _) => {
                    let val1 = res.next().unwrap();
                    let val2 = res.next().unwrap();
                    let ret_val = value::Value::new_val_pair(val1, val2, place.layout.clone());

                    place.store(fx, ret_val);
                }
            }
        }
    }
}
