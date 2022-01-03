#![feature(once_cell)]

pub mod type_info;
pub mod value_witness_table;

pub use type_info::*;
pub use value_witness_table::*;

pub trait BackendMethods: Sized {
    type DataId;
    type FuncId;

    fn type_infos(&mut self) -> &mut TypeInfos<Self>;
    fn value_witness_tables(&mut self) -> &mut ValueWitnessTables<Self>;

    fn alloc_type_info(&mut self, type_info: &TypeInfo<Self>) -> Self::DataId;
    fn alloc_value_witness_table(&mut self, vwt: &ValueWitnessTable<Self>) -> Self::DataId;
}
