#[repr(C)]
pub struct TypeInfo {
	pub vwt: *const ValueWitnessTable,
	pub flags: usize,
}

#[repr(C)]
pub struct ValueWitnessTable {
	pub size: usize,
	pub align: usize,
	pub stride: usize,
	pub copy_fn: unsafe extern "C" fn(*mut u8, *const u8, *const TypeInfo),
	pub move_fn: unsafe extern "C" fn(*mut u8, *const u8, *const TypeInfo),
	pub drop_fn: unsafe extern "C" fn(*mut u8, *const TypeInfo),
}

unsafe impl Sync for TypeInfo {}
unsafe impl Sync for ValueWitnessTable {}

pub unsafe extern "C" fn copy_trivial(dst: *mut u8, src: *const u8, ti: *const TypeInfo) {
	::std::ptr::copy_nonoverlapping(src, dst, (*(*ti).vwt).size);
}

pub unsafe extern "C" fn move_trivial(dst: *mut u8, src: *const u8, ti: *const TypeInfo) {
	::std::ptr::copy(src, dst, (*(*ti).vwt).size);
}

pub unsafe extern "C" fn copy_move_nop(_dst: *mut u8, _src: *const u8, _ti: *const TypeInfo) {}

pub unsafe extern "C" fn drop_trivial(_val: *mut u8, _ti: *const TypeInfo) {}

pub const FLAG_TRIVIAL: usize = 1 << 0;

pub static TRIVIAL_TIS: [TypeInfo; 6] = [
	TypeInfo {
		vwt: &TRIVIAL_VWTS[0],
		flags: FLAG_TRIVIAL,
	},
	TypeInfo {
		vwt: &TRIVIAL_VWTS[1],
		flags: FLAG_TRIVIAL,
	},
	TypeInfo {
		vwt: &TRIVIAL_VWTS[2],
		flags: FLAG_TRIVIAL,
	},
	TypeInfo {
		vwt: &TRIVIAL_VWTS[3],
		flags: FLAG_TRIVIAL,
	},
	TypeInfo {
		vwt: &TRIVIAL_VWTS[4],
		flags: FLAG_TRIVIAL,
	},
	TypeInfo {
		vwt: &TRIVIAL_VWTS[5],
		flags: FLAG_TRIVIAL,
	},
];

pub static TRIVIAL_VWTS: [ValueWitnessTable; 6] = [
	ValueWitnessTable {
		size: 0,
		align: 1,
		stride: 0,
		copy_fn: copy_move_nop,
		move_fn: copy_move_nop,
		drop_fn: drop_trivial,
	},
	ValueWitnessTable {
		size: 1,
		align: 1,
		stride: 1,
		copy_fn: copy_trivial,
		move_fn: move_trivial,
		drop_fn: drop_trivial,
	},
	ValueWitnessTable {
		size: 2,
		align: 2,
		stride: 2,
		copy_fn: copy_trivial,
		move_fn: move_trivial,
		drop_fn: drop_trivial,
	},
	ValueWitnessTable {
		size: 4,
		align: 4,
		stride: 4,
		copy_fn: copy_trivial,
		move_fn: move_trivial,
		drop_fn: drop_trivial,
	},
	ValueWitnessTable {
		size: 8,
		align: 8,
		stride: 8,
		copy_fn: copy_trivial,
		move_fn: move_trivial,
		drop_fn: drop_trivial,
	},
	ValueWitnessTable {
		size: 16,
		align: 8,
		stride: 16,
		copy_fn: copy_trivial,
		move_fn: move_trivial,
		drop_fn: drop_trivial,
	},
];