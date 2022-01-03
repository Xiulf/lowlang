#![feature(once_cell)]

pub mod type_info;
pub mod value_witness_table;

pub use type_info::*;
pub use value_witness_table::*;

use ir::db::IrDatabase;

pub trait BackendMethods: Sized {
    type DataId: Copy;
    type FuncId: Copy;

    fn db(&self) -> &dyn IrDatabase;

    fn type_infos(&self) -> &TypeInfos<Self>;
    fn value_witness_tables(&self) -> &ValueWitnessTables<Self>;

    fn alloc_type_info(&self, type_info: &TypeInfo<Self>) -> Self::DataId;
    fn alloc_value_witness_table(&self, vwt: &ValueWitnessTable<Self>) -> Self::DataId;
}
