use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::AnyTypeEnum;

pub struct Compiler {
    builder: Builder,
    context: Context,
    module: Module,
}

impl Compiler {
    pub fn compile(module_name: &str, lib: &syntax::Program) {
        let context = Context::create();
        let compiler = Compiler {
            module: Module::create(module_name),
            builder: context.create_builder(),
            context,
        };
        
        for func in &lib.fns {
            compiler.compile_fn(func);
        }
    }
    
    fn compile_fn(&self, func: &syntax::Function) {
        let param_tys = func.params.iter().map(|p| p.1.clone()).collect();
        let fn_type = self.construct_type(&syntax::Type::Fn(param_tys, Box::new(func.ret.clone())));
        let fn_val = self.module.add_function(&func.name.text, fn_type.into_function_type(), None);
        
        println!("{:#?}", fn_val);
    }
    
    fn construct_type(&self, ty: &syntax::Type) -> AnyTypeEnum {
        match ty {
            syntax::Type::Unit => AnyTypeEnum::VoidType(self.context.void_type()),
            syntax::Type::Bool => AnyTypeEnum::IntType(self.context.i8_type()),
            syntax::Type::UInt(syntax::UIntTy::U8) |
            syntax::Type::Int(syntax::IntTy::I8) => AnyTypeEnum::IntType(self.context.i8_type()),
            syntax::Type::UInt(syntax::UIntTy::U16) |
            syntax::Type::Int(syntax::IntTy::I16) => AnyTypeEnum::IntType(self.context.i16_type()),
            syntax::Type::UInt(syntax::UIntTy::U32) |
            syntax::Type::Int(syntax::IntTy::I32) => AnyTypeEnum::IntType(self.context.i32_type()),
            syntax::Type::UInt(syntax::UIntTy::U64) |
            syntax::Type::Int(syntax::IntTy::I64) => AnyTypeEnum::IntType(self.context.i64_type()),
            syntax::Type::Float(syntax::FloatTy::F32) => AnyTypeEnum::FloatType(self.context.f32_type()),
            syntax::Type::Float(syntax::FloatTy::F64) => AnyTypeEnum::FloatType(self.context.f64_type()),
            _ => unimplemented!("{}", ty)
        }
    }
}