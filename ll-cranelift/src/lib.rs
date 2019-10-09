use syntax::*;
use cranelift::prelude::*;
use cranelift_module::{DataContext, Linkage, Module};
use cranelift_simplejit::{SimpleJITBackend, SimpleJITBuilder};
use std::collections::HashMap;

pub struct JIT {
    bctx: FunctionBuilderContext,
    dctx: DataContext,
    ctx: codegen::Context,
    module: Module<SimpleJITBackend>,
    fn_tys: HashMap<String, (Vec<types::Type>, types::Type)>,
}

struct Translator<'a> {
    fn_tys: &'a HashMap<String, (Vec<types::Type>, types::Type)>,
    builder: FunctionBuilder<'a>,
    variables: HashMap<LocalId, (VariableOrStackSlot, types::Type, syntax::Type)>,
    blocks: HashMap<BlockId, Ebb>,
    dctx: &'a mut DataContext,
    module: &'a mut Module<SimpleJITBackend>,
    ret_ty: types::Type,
}

#[derive(Clone, Copy)]
enum VariableOrStackSlot {
    Variable(Variable),
    StackSlot(codegen::ir::StackSlot),
}

impl JIT {
    pub fn new() -> JIT {
        let builder = SimpleJITBuilder::new(cranelift_module::default_libcall_names());
        let module = Module::new(builder);
        
        JIT {
            fn_tys: HashMap::new(),
            bctx: FunctionBuilderContext::new(),
            dctx: DataContext::new(),
            ctx: module.make_context(),
            module,
        }
    }
    
    pub fn compile(&mut self, program: &Program) -> Result<HashMap<String, *const u8>, String> {
        let mut map = HashMap::new();
        
        for func in &program.fns {
            let params = func.params.iter().map(|p| convert_ty(&p.1, self.module.target_config().pointer_type())).collect();
            let ret = convert_ty(&func.ret, self.module.target_config().pointer_type());
            
            self.fn_tys.insert(func.name.text.clone(), (params, ret));
            
            map.insert(func.name.text.clone(), self.compile_fn(func)?);
        }
        
        Ok(map)
    }
    
    fn compile_fn(&mut self, f: &Function) -> Result<*const u8, String> {
        self.translate(f);
        
        let id = self.module.declare_function(
            &f.name.text,
            Linkage::Export,
            &self.ctx.func.signature
        ).map_err(|e| e.to_string())?;
        
        println!("{}", self.ctx.func.display(None));
        
        self.module.define_function(id, &mut self.ctx).map_err(|e| e.to_string())?;
        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions();
        
        let code = self.module.get_finalized_function(id);
        
        Ok(code)
    }
    
    fn translate(&mut self, f: &Function) {
        for (_, ty) in &f.params {
            let ty = convert_ty(ty, self.module.target_config().pointer_type());
            
            self.ctx.func.signature.params.push(AbiParam::new(ty));
        }
        
        let ret_ty = convert_ty(&f.ret, self.module.target_config().pointer_type());
        
        self.ctx.func.signature.returns.push(AbiParam::new(ret_ty.clone()));
        
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.bctx);
        let mut blocks = HashMap::new();
        
        for block in &f.blocks {
            blocks.insert(block.id, builder.create_ebb());
        }
        
        let entry_ebb = blocks[&BlockId(0)];
        
        builder.append_ebb_params_for_function_params(entry_ebb);
        builder.switch_to_block(entry_ebb);
        builder.seal_block(entry_ebb);
        
        let variables = {
            let mut vars = HashMap::new();
            let mut index = 0;
            
            for (i, (id, ty)) in f.params.iter().enumerate() {
                let val = builder.ebb_params(entry_ebb)[i];
                let var = declare_param(
                    ty.clone(),
                    convert_ty(ty, self.module.target_config().pointer_type()),
                    &mut builder,
                    &mut vars,
                    &mut index,
                    *id
                );
                
                builder.def_var(var, val);
            }
            
            for (id, ty) in f.bindings.iter() {
                let slot = builder.create_stack_slot(codegen::ir::StackSlotData::new(
                    codegen::ir::StackSlotKind::ExplicitSlot,
                    ty.size() as u32
                ));
                
                let ty2 = convert_ty(ty, self.module.target_config().pointer_type());
                
                vars.insert(*id, (VariableOrStackSlot::StackSlot(slot), ty2, ty.clone()));
            }
            
            vars
        };
        
        let mut trans = Translator {
            fn_tys: &self.fn_tys,
            builder,
            variables,
            blocks,
            ret_ty,
            dctx: &mut self.dctx,
            module: &mut self.module,
        };
        
        for block in &f.blocks {
            trans.translate_block(block);
        }
        
        trans.builder.finalize();
    }
}

impl VariableOrStackSlot {
    fn variable(self) -> Variable {
        match self {
            VariableOrStackSlot::Variable(v) => v,
            VariableOrStackSlot::StackSlot(_) => panic!("value is not a variable"),
        }
    }
    
    fn stack_slot(self) -> codegen::ir::StackSlot {
        match self {
            VariableOrStackSlot::StackSlot(v) => v,
            VariableOrStackSlot::Variable(_) => panic!("value is not a stack slot"),
        }
    }
}

impl<'a> Translator<'a> {
    fn translate_block(&mut self, block: &BasicBlock) {
        let ebb = self.blocks[&block.id];
        
        self.builder.switch_to_block(ebb);
        
        for stmt in &block.statements {
            self.translate_stmt(stmt);
        }
        
        self.translate_term(&block.terminator);
        self.builder.seal_block(ebb);
    }
    
    fn translate_stmt(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Assign(place, value) => {
                let value = self.translate_rvalue(value);  
                
                self.translate_assign(place, value);
            },
            Statement::StorageLive(_) |
            Statement::StorageDead(_) => (),
            Statement::Free(_) => unimplemented!(),
        }
    }
    
    fn translate_assign(&mut self, place: &Place, value: Value) {
        if place.projection.is_empty() {
            let PlaceBase::Local(id, _) = &place.base;
            
            match self.variables[id].0 {
                VariableOrStackSlot::Variable(v) => {
                    self.builder.def_var(v, value);
                },
                VariableOrStackSlot::StackSlot(ss) => {
                    self.builder.ins().stack_store(
                        value,
                        ss,
                        0,
                    );
                },
            }
        } else {
            let place = self.translate_place(place);              
            
            self.builder.ins().store(
                codegen::ir::MemFlags::new(),
                place,
                value,
                0,
            );
        }
    }
    
    fn translate_term(&mut self, term: &Terminator) {
        match term {
            Terminator::Return => {
                let ret = self.builder.ins().stack_load(
                    self.ret_ty,
                    self.variables[&LocalId(0)].0.stack_slot(),
                    0
                );
                
                self.builder.ins().return_(&[ret]);
            },
            Terminator::Goto(id) => {
                self.builder.ins().jump(self.blocks[id], &[]);
            },
            Terminator::Unreachable => {
                self.builder.ins().trap(TrapCode::UnreachableCodeReached);
            },
            Terminator::Abort => {
                self.builder.ins().resumable_trap(TrapCode::Interrupt);
            },
            Terminator::Call(callee, args, goto, unwind) => {
                let callee = self.translate_operand(callee);
                let args = args.iter().map(|a| self.translate_operand(a)).collect::<Vec<_>>();
                
                if let Some((place, goto)) = goto {
                    let mut sig = self.module.make_signature();
                    
                    // for _arg in &args {
                    //     sig.params.push(AbiParam::new(self.int));
                    // }
                    let ty = convert_ty(&resolve_ty_place(self, place), self.module.target_config().pointer_type());
                    
                    sig.returns.push(AbiParam::new(ty));
                    
                    let sig = self.builder.import_signature(sig);
                    let res = self.builder.ins().call_indirect(sig, callee, &args[..]);
                    
                    self.translate_assign(place, self.builder.inst_results(res)[0]);
                    self.builder.ins().jump(self.blocks[goto], &[]);
                } else {
                    unimplemented!();
                }
            },
            Terminator::Assert(op, expected, goto, unwind) => {
                let op = self.translate_operand(op);
                
                if *expected {
                    self.builder.ins().brnz(op, self.blocks[goto], &[]);
                    
                    if let Some(unwind) = unwind {
                        self.builder.ins().jump(self.blocks[unwind], &[]);
                    }
                } else {
                    self.builder.ins().brz(op, self.blocks[goto], &[]);
                    
                    if let Some(unwind) = unwind {
                        self.builder.ins().jump(self.blocks[unwind], &[]);
                    }
                }
            },
            _ => unimplemented!()
        }
    }
    
    fn translate_place(&mut self, place: &Place) -> Value {
        let mut val = match &place.base {
            PlaceBase::Local(id, _) => match self.variables[id] {
                (VariableOrStackSlot::Variable(v), _, _) => self.builder.use_var(v),
                (VariableOrStackSlot::StackSlot(ss), ty, _) => self.builder.ins().stack_load(
                    ty,
                    ss,
                    0
                ),
            },
        };
        
        for proj in &place.projection {
            match proj {
                PlaceElem::Deref(_) => {
                    val = self.builder.ins().load(
                        types::INVALID,
                        codegen::ir::MemFlags::new(),
                        val,
                        0,
                    );
                },
                PlaceElem::Field(num, _) => {
                    unimplemented!();
                },
            }
        }
        
        val
    }
    
    fn translate_operand(&mut self, op: &Operand) -> Value {
        match op {
            Operand::Copy(place, _) => {
                let value = self.translate_place(place);
                
                self.builder.ins().copy(value)
            },
            Operand::Move(place, _) => {
                self.translate_place(place)
            },
            Operand::Constant(c, _) => {
                self.translate_const(c)
            },
        }
    }
    
    fn translate_rvalue(&mut self, rvalue: &RValue) -> Value {
        match rvalue {
            RValue::Use(op) => self.translate_operand(op),
            RValue::Ref(place, _) => {
                match &place.base {
                    PlaceBase::Local(id, _) => match self.variables[id].0 {
                        VariableOrStackSlot::StackSlot(ss) => {
                            self.builder.ins().stack_addr(
                                self.module.target_config().pointer_type(),
                                ss,
                                0,
                            )
                        },
                        _ => unimplemented!()
                    }
                }
            },
            RValue::Binary(op, lhs, rhs, _) => {
                let lhs = self.translate_operand(lhs);
                let rhs = self.translate_operand(rhs);
                
                match op {
                    BinOp::Add => self.builder.ins().iadd(lhs, rhs),
                    BinOp::Lt => self.builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs),
                    _ => unimplemented!(),
                }
            },
            _ => unimplemented!()
        }
    }
    
    fn translate_const(&mut self, c: &Constant) -> Value {
        match c {
            Constant::Bool(v) => self.builder.ins().bconst(types::B1, *v),
            Constant::UInt(v, UIntTy::U8) => self.builder.ins().iconst(types::I8, *v as i64),
            Constant::UInt(v, UIntTy::U16) => self.builder.ins().iconst(types::I16, *v as i64),
            Constant::UInt(v, UIntTy::U32) => self.builder.ins().iconst(types::I32, *v as i64),
            Constant::UInt(v, UIntTy::U64) => self.builder.ins().iconst(types::I64, *v as i64),
            Constant::Int(v, IntTy::I8) => self.builder.ins().iconst(types::I8, *v),
            Constant::Int(v, IntTy::I16) => self.builder.ins().iconst(types::I16, *v),
            Constant::Int(v, IntTy::I32) => self.builder.ins().iconst(types::I32, *v),
            Constant::Int(v, IntTy::I64) => self.builder.ins().iconst(types::I64, *v),
            Constant::Float(v, FloatTy::F32) => self.builder.ins().f32const(*v as f32),
            Constant::Float(v, FloatTy::F64) => self.builder.ins().f64const(*v),
            Constant::Item(ident) => {
                let ty = &self.fn_tys[&ident.text];
                let mut sig = self.module.make_signature();
                
                for ty in &ty.0 {
                    sig.params.push(AbiParam::new(*ty));
                }
                
                sig.returns.push(AbiParam::new(ty.1));
                
                let f = self.module.declare_function(&ident.text, Linkage::Import, &sig).unwrap();
                let f = self.module.declare_func_in_func(f, &mut self.builder.func);
                
                self.builder.ins().func_addr(
                    self.module.target_config().pointer_type(),
                    f
                )
            },
            _ => unimplemented!()
        }
    }
}

fn translate_uninit(builder: &mut FunctionBuilder, ty: &syntax::Type) -> Value {
    match ty {
        syntax::Type::Bool => builder.ins().bconst(types::B1, false),
        syntax::Type::Unit => builder.ins().null(types::INVALID),
        syntax::Type::Int(IntTy::I8) => builder.ins().iconst(types::I8, 0),
        syntax::Type::Int(IntTy::I16) => builder.ins().iconst(types::I16, 0),
        syntax::Type::Int(IntTy::I32) => builder.ins().iconst(types::I32, 0),
        syntax::Type::Int(IntTy::I64) => builder.ins().iconst(types::I64, 0),
        syntax::Type::UInt(UIntTy::U8) => builder.ins().iconst(types::I8, 0),
        syntax::Type::UInt(UIntTy::U16) => builder.ins().iconst(types::I16, 0),
        syntax::Type::UInt(UIntTy::U32) => builder.ins().iconst(types::I32, 0),
        syntax::Type::UInt(UIntTy::U64) => builder.ins().iconst(types::I64, 0),
        _ => unimplemented!()
    }
}

fn declare_param(
    ast_ty: syntax::Type,
    ty: cranelift::prelude::Type,
    builder: &mut FunctionBuilder,
    variables: &mut HashMap<LocalId, (VariableOrStackSlot, types::Type, syntax::Type)>,
    index: &mut u32,
    id: LocalId
) -> Variable {
    let var = Variable::with_u32(*index);
    
    variables.insert(id, (VariableOrStackSlot::Variable(var), ty, ast_ty));
    builder.declare_var(var, ty);
    *index += 1;
    
    var
}

fn convert_ty(ty: &syntax::Type, ptr_ty: cranelift::prelude::Type) -> cranelift::prelude::Type {
    use cranelift::prelude::types::*;
    
    match ty {
        syntax::Type::Bool => B1,
        syntax::Type::Int(IntTy::I8) => I8,
        syntax::Type::Int(IntTy::I16) => I16,
        syntax::Type::Int(IntTy::I32) => I32,
        syntax::Type::Int(IntTy::I64) => I64,
        syntax::Type::UInt(UIntTy::U8) => I8,
        syntax::Type::UInt(UIntTy::U16) => I16,
        syntax::Type::UInt(UIntTy::U32) => I32,
        syntax::Type::UInt(UIntTy::U64) => I64,
        syntax::Type::Float(FloatTy::F32) => F32,
        syntax::Type::Float(FloatTy::F64) => F64,
        syntax::Type::Ptr(_) => ptr_ty,
        syntax::Type::Unit => INVALID,
        syntax::Type::Tuple(..) => INVALID,
        syntax::Type::Fn(..) => INVALID,
    }
}

fn resolve_ty_place(f: &Translator, place: &Place) -> syntax::Type {
    let mut ty = match &place.base {
        PlaceBase::Local(id, _) => f.variables[id].2.clone(),
    };
    
    for proj in &place.projection {
        match proj {
            PlaceElem::Deref(_) => match ty {
                syntax::Type::Ptr(t) => ty = *t,
                _ => panic!()
            },
            PlaceElem::Field(i, _) => match ty {
                syntax::Type::Tuple(tys) => ty = tys.into_iter().nth(*i).unwrap(),
                _ => panic!()
            },
        }
    }
    
    ty
}