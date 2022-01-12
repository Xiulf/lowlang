use std::hash::Hash;
use std::collections::HashMap;

pub trait Backend {
	type DataId: Copy + Eq + Hash;
	type FuncId: Copy + Eq + Hash;
}

type TypeId = u32;

pub struct State<B: Backend> {
	type_info: HashMap<TypeId, TypeInfo>,
}

pub enum TypeInfo<B: Backend> {
	Trivial(u64, u64),
	Concrete {
		vwt: VwtId,
		fields: Vec<u64>,
	},
	Generic {
		copy_fn: B::FuncId,
		move_fn: B::FuncId,
		drop_fn: B::FuncId,
		mk_generics: B::FuncId,
		mk_vwt: B::FuncId,
		mk_info: B::FuncId,
	}
}

pub struct Vwt<B: Backend> {
	pub size: u64,
	pub align: u64,
	pub stride: u64,
	pub copy_fn: B::FuncId,
	pub move_fn: B::FuncId,
	pub drop_fn: B::FuncId,
}