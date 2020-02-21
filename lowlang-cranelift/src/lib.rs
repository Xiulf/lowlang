pub mod ptr;
pub mod place;
pub mod value;
pub mod analyze;
pub mod pass;
pub mod trans;
pub mod error;

use error::Error;
use lowlang_syntax as syntax;
use lowlang_syntax::layout::TyLayout;
pub use cranelift_module::{Backend, Module, FuncId, DataId};
use cranelift_frontend::FunctionBuilder;
use cranelift_codegen::ir::{self, types};
use cranelift_codegen::settings;
use std::collections::BTreeMap;

pub fn compile<'t>(
    package: &syntax::Package<'t>,
    types: &'t syntax::ty::TyCtx<'t>,
    target: &str,
    optimize: bool,
    out_file: std::path::PathBuf
) -> Result<(), Error> {
    use settings::Configurable as _;
    use std::str::FromStr as _;
    let mut flags_builder = settings::builder();

    if optimize {
        flags_builder.set("opt_level", "speed").unwrap();
    }

    let isa = cranelift_codegen::isa::lookup(target_lexicon::triple!(target)).unwrap()
        .finish(settings::Flags::new(flags_builder));

    let builder = cranelift_object::ObjectBuilder::new(
        isa,
        package.name.clone(),
        cranelift_object::ObjectTrapCollection::Enabled,
        cranelift_module::default_libcall_names(),
    ).unwrap();
    
    let module = Module::<cranelift_object::ObjectBackend>::new(builder);
    let layout_interner = syntax::layout::LayoutInterner::new();
    let layouts = syntax::layout::LayoutCtx::new(types, &layout_interner, module.isa().triple());
    let product = trans::translate(module, &layouts, package)?;
    let mut tmp_name = out_file.clone();
        tmp_name.set_extension("o");

    assemble(product, tmp_name.as_ref());
    link(tmp_name.as_ref(), out_file.as_ref());

    Ok(())
}

pub fn assemble(product: cranelift_object::ObjectProduct, out_file: &std::path::Path) {
    use std::io::Write;
    let bytes = product.emit().unwrap();

    std::fs::File::create(out_file).unwrap().write_all(&bytes).unwrap();
}

pub fn link(obj_file: &std::path::Path, out_file: &std::path::Path) {
    let _status = std::process::Command::new("cc")
        .args(&[obj_file, std::path::Path::new("-o"), out_file])
        .status()
        .unwrap();
}

pub struct FunctionCtx<'a, 't, 'l, B: Backend> {
    pub layouts: &'a syntax::layout::LayoutCtx<'t, 'l>,
    pub module: &'a mut Module<B>,
    pub builder: FunctionBuilder<'a>,
    pub pointer_type: types::Type,
    pub package: *const syntax::Package<'t>,
    pub body: &'a syntax::Body<'t>,
    pub func_ids: &'a BTreeMap<syntax::ItemId, (FuncId, ir::Signature, Vec<TyLayout<'t, 'l>>)>,
    pub data_ids: &'a BTreeMap<syntax::ItemId, (DataId, TyLayout<'t, 'l>)>,
    pub blocks: BTreeMap<syntax::BlockId, ir::Ebb>,
    pub locals: BTreeMap<syntax::LocalId, place::Place<'t, 'l>>,
    bytes_count: &'a mut usize,
}

impl<'a, 't, 'l, B: Backend> FunctionCtx<'a, 't, 'l, B> {
    pub fn clif_type(&self, layout: TyLayout<'t, 'l>) -> Option<types::Type> {
        self::clif_type(self.module, layout)
    }

    pub unsafe fn package(&self) -> &syntax::Package<'t> {
        &*self.package
    }
}

pub fn clif_type<'t, 'l>(module: &Module<impl Backend>, layout: TyLayout<'t, 'l>) -> Option<types::Type> {
    use syntax::{Type, IntSize, FloatSize};

    match &*layout.ty {
        Type::Unit => None,
        Type::Bool => Some(types::I8),
        Type::Char => Some(types::I32),
        Type::Int(size) | Type::UInt(size) => match size {
            IntSize::Bits8 => Some(types::I8),
            IntSize::Bits16 => Some(types::I16),
            IntSize::Bits32 => Some(types::I32),
            IntSize::Bits64 => Some(types::I64),
            IntSize::Bits128 => Some(types::I128),
            IntSize::Size => Some(match module.target_config().pointer_width {
                target_lexicon::PointerWidth::U16 => types::I16,
                target_lexicon::PointerWidth::U32 => types::I32,
                target_lexicon::PointerWidth::U64 => types::I64,
            }),
        },
        Type::Float(size) => match size {
            FloatSize::Bits32 => Some(types::F32),
            FloatSize::Bits64 => Some(types::F64),
            FloatSize::Size => Some(match module.target_config().pointer_width {
                target_lexicon::PointerWidth::U16 => types::F32,
                target_lexicon::PointerWidth::U32 => types::F32,
                target_lexicon::PointerWidth::U64 => types::F64,
            }),
        },
        Type::Ref(_) => Some(module.target_config().pointer_type()),
        Type::Proc(_) => Some(module.target_config().pointer_type()),
        _ => None,
    }
}
