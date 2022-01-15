#![feature(once_cell, generic_associated_types)]

pub mod type_info;
pub mod value_witness_table;

pub use type_info::*;
pub use value_witness_table::*;

use ir::db::IrDatabase;

pub trait BackendMethods: Sized {
    type DataId: Copy;
    type FuncId: Copy;
    type FnBuilder<'a>: FnBuilder<'a, Self> where Self: 'a;

    fn db(&self) -> &dyn IrDatabase;

    fn type_infos(&self) -> &TypeInfos<Self::DataId>;
    fn value_witness_tables(&self) -> &ValueWitnessTables<Self::DataId, Self::FuncId>;

    fn alloc_type_info(&self, type_info: &TypeInfo<Self::DataId>) -> Self::DataId;
    fn alloc_value_witness_table(&self, vwt: &ValueWitnessTable<Self::FuncId>) -> Self::DataId;

    fn create_vwt_fn<'a>(&'a mut self, nparams: usize) -> Self::FnBuilder<'a>;

    fn copy_trivial(&mut self) -> Self::FuncId;
    fn move_trivial(&mut self) -> Self::FuncId;
    fn drop_trivial(&mut self) -> Self::FuncId;
}

pub trait FnBuilder<'a, B: BackendMethods> {
    type Value: Copy;

    fn get_param(&self, param: usize) -> Self::Value;

    fn memcopy(&mut self, src: Self::Value, dst: Self::Value, len: Result<u64, Self::Value>);
    fn memmove(&mut self, src: Self::Value, dst: Self::Value, len: Result<u64, Self::Value>);

    fn finish(self) -> B::FuncId;
}
