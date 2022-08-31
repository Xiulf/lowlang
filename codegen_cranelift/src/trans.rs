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

    fn trans_init(_fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>, _place: place::Place<'ctx>) {
    }

    fn trans_drop(_fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>, _place: place::Place<'ctx>) {
    }

    fn trans_place(fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>, place: &ir::Place) -> place::Place<'ctx> {
        let mut res = fx.locals[&place.local].clone();

        for elem in &place.elems {
            match elem {
                | ir::PlaceElem::Deref => res = res.deref(fx),
                | ir::PlaceElem::Field(idx) => res = res.field(fx, *idx),
                | ir::PlaceElem::Index(idx) => {
                    let idx = Self::trans_op(fx, idx, None);

                    res = res.index(fx, idx);
                },
                | ir::PlaceElem::Downcast(idx) => {
                    res = res.downcast_variant(fx, *idx);
                },
            }
        }

        res
    }

    fn trans_const(fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>, c: &ir::Const, into: Option<place::Place<'ctx>>) -> value::Value<'ctx> {
        let ty = ir::const_type(fx.ir, c);
        let layout = ir::layout::layout_of(&ty, &fx.target);

        if let Some(into) = into {
            match c {
                | ir::Const::Undefined(_) => into.to_value(fx),
                | ir::Const::Scalar(s, _) => {
                    let val = value::Value::new_const(*s, fx, layout);

                    into.store(fx, val.clone());
                    val
                },
                | ir::Const::Tuple(vals) if vals.is_empty() => value::Value::new_unit(),
                | ir::Const::Variant(idx, cs, _) => {
                    let as_variant = into.clone().downcast_variant(fx, *idx);

                    for (i, c) in cs.iter().enumerate() {
                        let place = as_variant.clone().field(fx, i);

                        Self::trans_const(fx, c, Some(place));
                    }

                    Self::trans_set_discr(fx, into.clone(), *idx as u128);

                    into.to_value(fx)
                },
                | ir::Const::Ptr(to) => {
                    let to_layout = layout.pointee(&fx.target);
                    let data = Self::alloc_const(fx.mcx, to, to_layout, None);
                    let ptr_ty = fx.module.target_config().pointer_type();
                    let global = fx.mcx.module.declare_data_in_func(data, &mut fx.bcx.func);
                    let global = fx.bcx.ins().global_value(ptr_ty, global);
                    let val = value::Value::new_val(global, layout);

                    into.store(fx, val.clone());
                    val
                },
                | _ => unimplemented!(),
            }
        } else {
            match c {
                | ir::Const::Undefined(_) => {
                    let slot = fx
                        .bcx
                        .create_stack_slot(clif::StackSlotData::new(clif::StackSlotKind::ExplicitSlot, layout.size.bytes() as u32));

                    value::Value::new_ref(ptr::Pointer::stack(slot), layout)
                },
                | ir::Const::Scalar(s, _) => value::Value::new_const(*s, fx, layout),
                | ir::Const::Tuple(vals) if vals.is_empty() => value::Value::new_unit(),
                | ir::Const::Addr(decl) => {
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
                },
                | ir::Const::Variant(idx, cs, ty) => {
                    let layout = ir::layout::layout_of(ty, &fx.target);
                    let place = place::Place::new_stack(fx, layout);
                    let as_variant = place.clone().downcast_variant(fx, *idx);

                    for (i, c) in cs.iter().enumerate() {
                        let place = as_variant.clone().field(fx, i);

                        Self::trans_const(fx, c, Some(place));
                    }

                    Self::trans_set_discr(fx, place.clone(), *idx as u128);

                    place.to_value(fx)
                },
                | ir::Const::Ptr(to) => {
                    let to_layout = layout.pointee(&fx.target);
                    let data = Self::alloc_const(fx.mcx, to, to_layout, None);
                    let ptr_ty = fx.module.target_config().pointer_type();
                    let global = fx.mcx.module.declare_data_in_func(data, &mut fx.bcx.func);
                    let global = fx.bcx.ins().global_value(ptr_ty, global);

                    value::Value::new_val(global, layout)
                },
                | _ => unimplemented!(),
            }
        }
    }

    fn trans_set_discr(fx: &mut FunctionCtx<'_, 'ctx, '_, Self::Backend>, place: place::Place<'ctx>, val: u128) {
        if let ir::Type::Box(_) = place.layout.ty.kind {
            let place = place.deref(fx);

            return Self::trans_set_discr(fx, place, val);
        }

        match place.layout.variants.clone() {
            | ir::layout::Variants::Single { index } => {
                assert_eq!(index, val as usize);
            },
            | ir::layout::Variants::Multiple {
                tag: _,
                tag_field,
                tag_encoding: ir::layout::TagEncoding::Direct,
                variants: _,
            } => {
                let ptr = place.field(fx, tag_field);
                let discr = value::Value::new_const(val, fx, ptr.layout.clone());

                ptr.store(fx, discr);
            },
            | ir::layout::Variants::Multiple {
                tag: _,
                tag_field,
                tag_encoding:
                    ir::layout::TagEncoding::Niche {
                        dataful_variant,
                        niche_variants,
                        niche_start,
                    },
                variants: _,
            } => {
                if val != dataful_variant as u128 {
                    let niche = place.field(fx, tag_field);
                    let niche_value = val - *niche_variants.start() as u128;
                    let niche_value = niche_value.wrapping_add(niche_start);
                    let niche_val = value::Value::new_const(niche_value, fx, niche.layout.clone());

                    niche.store(fx, niche_val);
                }
            },
        }
    }

    fn trans_rvalue(fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>, place: place::Place<'ctx>, rvalue: &ir::RValue) {
        match rvalue {
            | ir::RValue::Use(op) => {
                Self::trans_op(fx, op, Some(place));
            },
            | ir::RValue::AddrOf(val) => {
                let val = Self::trans_place(fx, val);

                val.write_place_ref(fx, place);
            },
            | ir::RValue::Cast(val, to) => {
                let layout = ir::layout::layout_of(to, &fx.target);
                let val = Self::trans_place(fx, val).to_value(fx);
                let val = val.cast(fx, layout);

                place.store(fx, val);
            },
            | ir::RValue::GetDiscr(val) => {
                let mut val = Self::trans_place(fx, val).to_value(fx);

                while let ir::Type::Box(_) = &val.layout.ty.kind {
                    val = val.deref(fx);
                }

                if let ir::layout::Abi::Uninhabited = val.layout.abi {
                    unreachable!();
                }

                let (_tag_scalar, tag_field, tag_encoding) = match &val.layout.variants {
                    | ir::layout::Variants::Single { index } => {
                        let val = value::Value::new_const(*index as u128, fx, place.layout.clone());

                        place.store(fx, val);
                        return;
                    },
                    | ir::layout::Variants::Multiple {
                        tag,
                        tag_field,
                        tag_encoding,
                        variants: _,
                    } => (tag.clone(), *tag_field, tag_encoding.clone()),
                };

                let tag = val.field(fx, tag_field);

                match tag_encoding {
                    | ir::layout::TagEncoding::Direct => {
                        place.store(fx, tag);
                    },
                    | ir::layout::TagEncoding::Niche { .. } => unimplemented!(),
                }
            },
            | ir::RValue::Intrinsic(name, args) => {
                let args2 = args
                    .iter()
                    .map(|a| {
                        let a = Self::trans_op(fx, a, None);

                        value_for_arg!(fx, a, match a.on_stack(fx) {
                            | (ptr, None) => abi::EmptySinglePair::Single(ptr.get_addr(fx)),
                            | (ptr, Some(meta)) => abi::EmptySinglePair::Pair(ptr.get_addr(fx), meta),
                        })
                    })
                    .flatten()
                    .collect::<Vec<_>>();

                intrinsic!(fx, name, args2[..], place, [
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
                    (complex "ptr_offset"(ptr, offset) => {
                        let ptr_ty = ir::operand_type(fx.ir, fx.body, &args[0]);
                        let pointee = ir::layout::layout_of(&ptr_ty, &fx.target).pointee(&fx.target);
                        let offset = fx.bcx.ins().imul_imm(offset, pointee.size.bytes() as i64);
                        let val = fx.bcx.ins().iadd(ptr, offset);
                        value::Value::new_val(val, place.layout.clone())
                    }),
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
                    (complex "box_alloc"(n) => {
                        let mut malloc = fx.module.make_signature();
                        let ptr_type = fx.module.target_config().pointer_type();

                        malloc.returns.push(clif::AbiParam::new(ptr_type));
                        malloc.params.push(clif::AbiParam::new(ptr_type));

                        let malloc = fx.mcx.module.declare_function("malloc", clif::Linkage::Import, &malloc).unwrap();
                        let malloc = fx.mcx.module.declare_func_in_func(malloc, &mut fx.bcx.func);
                        let inst = fx.bcx.ins().call(malloc, &[n]);
                        let val = fx.bcx.inst_results(inst)[0];
                        let n = fx.bcx.ins().iconst(ptr_type, ptr_type.bytes() as i64 * 3);
                        let inst = fx.bcx.ins().call(malloc, &[n]);
                        let ptr = fx.bcx.inst_results(inst)[0];
                        let one = fx.bcx.ins().iconst(ptr_type, 1);
                        let zero = fx.bcx.ins().iconst(ptr_type, 0);

                        fx.bcx.ins().store(clif::MemFlags::trusted(), val, ptr, 0);
                        fx.bcx.ins().store(clif::MemFlags::trusted(), one, ptr, ptr_type.bytes() as i32);
                        fx.bcx.ins().store(clif::MemFlags::trusted(), zero, ptr, ptr_type.bytes() as i32);

                        value::Value::new_val(ptr, place.layout.clone())
                    }),
                    (complex "box_free"(ptr) => {
                        let mut free = fx.module.make_signature();
                        let ptr_type = fx.module.target_config().pointer_type();

                        free.params.push(clif::AbiParam::new(ptr_type));

                        let free = fx.mcx.module.declare_function("free", clif::Linkage::Import, &free).unwrap();
                        let free = fx.mcx.module.declare_func_in_func(free, &mut fx.bcx.func);
                        let strong_count = fx.bcx.ins().load(ptr_type, clif::MemFlags::trusted(), ptr, ptr_type.bytes() as i32);
                        let strong_count = fx.bcx.ins().irsub_imm(strong_count, 1);
                        let if_zero = fx.bcx.create_block();
                        let if_else = fx.bcx.create_block();
                        let exit = fx.bcx.create_block();

                        fx.bcx.ins().brz(strong_count, if_zero, &[]);
                        fx.bcx.ins().jump(if_else, &[]);

                        fx.bcx.switch_to_block(if_zero);

                        let val = fx.bcx.ins().load(ptr_type, clif::MemFlags::trusted(), ptr, 0);

                        fx.bcx.ins().call(free, &[val]);
                        fx.bcx.ins().call(free, &[ptr]);
                        fx.bcx.ins().jump(exit, &[]);

                        fx.bcx.switch_to_block(if_else);
                        fx.bcx.ins().store(clif::MemFlags::trusted(), strong_count, ptr, ptr_type.bytes() as i32);
                        fx.bcx.ins().jump(exit, &[]);

                        fx.bcx.switch_to_block(exit);
                        value::Value::new_unit()
                    }),
                    (complex "box_copy"(ptr) => {
                        let ptr_type = fx.module.target_config().pointer_type();
                        let strong_count = fx.bcx.ins().load(ptr_type, clif::MemFlags::trusted(), ptr, ptr_type.bytes() as i32);
                        let strong_count = fx.bcx.ins().iadd_imm(strong_count, 1);

                        fx.bcx.ins().store(clif::MemFlags::trusted(), strong_count, ptr, ptr_type.bytes() as i32);
                        value::Value::new_unit()
                    }),
                ]);
            },
        }
    }

    fn trans_term(fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>, term: &ir::Term) {
        match term {
            | ir::Term::Abort => {
                fx.bcx.ins().trap(clif::TrapCode::User(0));
            },
            | ir::Term::Return => {
                let rets = fx.body.rets().map(|r| abi::value_for_ret(fx, r.id)).flatten().collect::<Vec<_>>();

                fx.bcx.ins().return_(&rets);
            },
            | ir::Term::Jump(to) => {
                fx.bcx.ins().jump(fx.blocks[to], &[]);
            },
            | ir::Term::Switch(op, vals, blocks) => {
                let mut switch = clif::Switch::new();
                let otherwise = fx.blocks[blocks.last().unwrap()];
                let val = Self::trans_op(fx, op, None);
                let val = val.load_scalar(fx);

                for (val, block) in vals.iter().zip(blocks) {
                    switch.set_entry(*val, fx.blocks[block]);
                }

                switch.emit(&mut fx.bcx, val, otherwise)
            },
        }
    }

    fn trans_call(fx: &mut FunctionCtx<'_, 'ctx, '_, ClifBackend<'ctx>>, rets: Vec<place::Place<'ctx>>, func: &ir::Operand, args: Vec<value::Value<'ctx>>) {
        let ret_modes = rets.into_iter().map(|r| (abi::get_pass_mode(fx.mcx, r.layout()), r)).collect::<Vec<_>>();

        let ret_ptrs = ret_modes
            .iter()
            .filter_map(|(m, p)| match m {
                | abi::PassMode::ByRef { size: _ } => Some(p.as_ptr().get_addr(fx)),
                | _ => None,
            })
            .collect::<Vec<_>>();

        let args = ret_ptrs
            .into_iter()
            .chain(
                args.into_iter()
                    .map(|a| {
                        value_for_arg!(fx, a, match a.on_stack(fx) {
                            | (ptr, None) => abi::EmptySinglePair::Single(ptr.get_addr(fx)),
                            | (ptr, Some(meta)) => {
                                abi::EmptySinglePair::Pair(ptr.get_addr(fx), meta)
                            },
                        })
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

        let mut res = fx.bcx.inst_results(inst).iter().copied().collect::<Vec<_>>().into_iter();

        for (ret_mode, place) in ret_modes {
            match ret_mode {
                | abi::PassMode::NoPass => {},
                | abi::PassMode::ByRef { .. } => {},
                | abi::PassMode::ByVal(_) => {
                    let ret_val = res.next().unwrap();
                    let ret_val = value::Value::new_val(ret_val, place.layout.clone());

                    place.store(fx, ret_val);
                },
                | abi::PassMode::ByValPair(_, _) => {
                    let val1 = res.next().unwrap();
                    let val2 = res.next().unwrap();
                    let ret_val = value::Value::new_val_pair(val1, val2, place.layout.clone());

                    place.store(fx, ret_val);
                },
            }
        }
    }
}
