use syntax::*;
use cranelift::prelude::*;
use cranelift_module::{DataContext, Linkage, Module};
use cranelift_simplejit::{SimpleJITBackend, SimpleJITBuilder};

pub struct JIT {
    bctx: FunctionBuilderContext,
    dctx: DataContext,
    ctx: codegen::Context,
    module: Module<SimpleJITBackend>,
}

impl JIT {
    pub fn new() -> JIT {
        let builder = SimpleJITBuilder::new(cranelift_module::default_libcall_names());
        let module = Module::new(builder);
        
        JIT {
            bctx: FunctionBuilderContext::new(),
            dctx: DataContext::new(),
            ctx: module.make_context(),
            module,
        }
    }
    
    pub fn compile(&mut self, program: &Program) -> Result<*const u8, String> {
        unimplemented!();
    }
}