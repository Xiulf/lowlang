use ir::db::IrDatabase;
use tempfile::NamedTempFile;

#[no_mangle]
pub fn compile_module(db: &dyn IrDatabase, ir: &ir::Module, object_file: &mut NamedTempFile) {
}
