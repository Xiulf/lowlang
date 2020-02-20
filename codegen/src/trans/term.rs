use crate::{FunctionCtx, Backend};
use cranelift_codegen::ir::{self, InstBuilder};

impl<'a, 't, 'l, B: Backend> FunctionCtx<'a, 't, 'l, B> {
    pub fn trans_term(&mut self, term: &syntax::Terminator<'t>) {
        match term {
            syntax::Terminator::Unset => panic!("unset terminator"),
            syntax::Terminator::Abort => {
                self.builder.ins().trap(ir::TrapCode::User(0));
            },
            syntax::Terminator::Jump(target) => {
                self.builder.ins().jump(self.blocks[&target], &[]);
            },
            syntax::Terminator::Return => {
                let rets = self.body.rets().iter().filter_map(|ret| {
                    match crate::pass::pass_mode(self.module, ret.ty.layout(self.layouts)) {
                        crate::pass::PassMode::ByVal(_) => {
                            let place = self.locals[&ret.id];
                            let ret_val = place.to_value(self).load_scalar(self);

                            Some(ret_val)
                        },
                        _ => None
                    }
                }).collect::<Vec<_>>();

                self.builder.ins().return_(&rets);
            },
            syntax::Terminator::Switch(pred, values, targets) => {
                let pred = self.trans_op(pred).load_scalar(self);
                let mut switch = cranelift_frontend::Switch::new();

                for (i, value) in values.iter().enumerate() {
                    switch.set_entry(*value as u64, self.blocks[&targets[i]]);
                }

                let otherwise = self.blocks[targets.last().unwrap()];

                switch.emit(&mut self.builder, pred, otherwise);
            },
            syntax::Terminator::Call(places, proc, args, target) => {
                let places = places.iter().map(|place| self.trans_place(place)).collect::<Vec<_>>();
                let args = args.iter().map(|arg| self.trans_op(arg)).collect::<Vec<_>>();
                let (ret_tys, func_place) = match proc {
                    syntax::Operand::Constant(syntax::Const::FuncAddr(id, _)) => {
                        (self.func_ids[id].2.clone(), None)
                    },
                    syntax::Operand::Place(place) => {
                        let place = self.trans_place(place);
                        let rets = match &*place.layout.ty {
                            syntax::Type::Proc(sig) => sig.2.clone(),
                            _ => unreachable!(),
                        }.iter().map(|ty| ty.layout(self.layouts)).collect();

                        (rets, Some(place))
                    },
                    _ => unreachable!(),
                };

                let output_modes = ret_tys.iter().map(|r| crate::pass::pass_mode(self.module, *r)).collect::<Vec<_>>();
                let ret_ptrs = output_modes.iter().zip(places.iter()).filter_map(|(m, p)| match m {
                    crate::pass::PassMode::ByRef => Some(p.as_ptr(self).get_addr(self)),
                    crate::pass::PassMode::ByVal(_) => None,
                    crate::pass::PassMode::NoPass => None,
                }).collect::<Vec<_>>();

                let args = ret_ptrs.into_iter().chain(
                    args.into_iter().map(|arg| crate::pass::value_for_arg(self, arg).into_iter()).flatten()
                ).collect::<Vec<_>>();

                let call_inst = match proc {
                    syntax::Operand::Constant(syntax::Const::FuncAddr(id, _)) => {
                        let id = self.func_ids[id].0;
                        let func = self.module.declare_func_in_func(id, self.builder.func);

                        self.builder.ins().call(func, &args)
                    },
                    syntax::Operand::Place(_) => {
                        let place = func_place.unwrap();
                        let place_sig = match &*place.layout.ty {
                            syntax::Type::Proc(sig) => sig.clone(),
                            _ => unreachable!(),
                        };

                        let sig = crate::pass::call_sig(self.module, self.layouts, &place_sig);
                        let sig = self.builder.import_signature(sig);
                        let func = place.to_value(self).load_scalar(self);

                        self.builder.ins().call_indirect(sig, func, &args)
                    },
                    _ => unreachable!(),
                };

                let mut i = 0;

                for ((mode, ret_ty), place) in output_modes.iter().zip(ret_tys.iter()).zip(places.iter()) {
                    match mode {
                        crate::pass::PassMode::ByVal(_) => {
                            let ret_val = self.builder.inst_results(call_inst)[i];

                            place.store(self, crate::value::Value::new_val(ret_val, *ret_ty));
                            i += 1;
                        },
                        _ => (),
                    }
                }

                self.builder.ins().jump(self.blocks[target], &[]);
            },
        }
    }
}
