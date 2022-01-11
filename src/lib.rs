#![allow(dead_code, non_snake_case)]

pub mod type_info;

use type_info::*;

struct Pair<T> {
    x: T,
    y: T,
}

struct TypeInfoPair {
    vwt: *const ValueWitnessTable,
    flags: usize,
    T: *const TypeInfo,
    x: usize,
    y: usize,
}

unsafe extern "C" fn copy_pair(dst: *mut u8, src: *const u8, TP: *const TypeInfoPair) {
    let T = (*TP).T;
    let x = (*TP).x;
    let y = (*TP).y;

    ((*(*T).vwt).copy_fn)(dst.add(x), src.add(x), T);
    ((*(*T).vwt).copy_fn)(dst.add(y), src.add(y), T);
}

unsafe extern "C" fn move_pair(dst: *mut u8, src: *const u8, TP: *const TypeInfoPair) {
    let T = (*TP).T;
    let x = (*TP).x;
    let y = (*TP).y;

    ((*(*T).vwt).move_fn)(dst.add(x), src.add(x), T);
    ((*(*T).vwt).move_fn)(dst.add(y), src.add(y), T);
}

unsafe extern "C" fn drop_pair(val: *mut u8, TP: *const TypeInfoPair) {
    let T = (*TP).T;
    let x = (*TP).x;
    let y = (*TP).y;

    ((*(*T).vwt).drop_fn)(val.add(x), T);
    ((*(*T).vwt).drop_fn)(val.add(y), T);
}

unsafe fn mk_pair_vwt(T: *const TypeInfo) -> ValueWitnessTable {
    if (*T).flags & FLAG_TRIVIAL == 1 {
        ValueWitnessTable {
            size: (*(*T).vwt).size * 2,
            align: (*(*T).vwt).align,
            stride: (*(*T).vwt).stride * 2,
            copy_fn: copy_trivial,
            move_fn: move_trivial,
            drop_fn: drop_trivial,
        }
    } else {
        let copy_pair = copy_pair as unsafe extern "C" fn(*mut u8, *const u8, *const TypeInfoPair);
        let move_pair = move_pair as unsafe extern "C" fn(*mut u8, *const u8, *const TypeInfoPair);
        let drop_pair = drop_pair as unsafe extern "C" fn(*mut u8, *const TypeInfoPair);

        ValueWitnessTable {
            size: (*(*T).vwt).size * 2,
            align: (*(*T).vwt).align,
            stride: (*(*T).vwt).stride * 2,
            copy_fn: ::std::mem::transmute(copy_pair),
            move_fn: ::std::mem::transmute(move_pair),
            drop_fn: ::std::mem::transmute(drop_pair),
        }
    }
}

unsafe fn mk_pair_ti(vwt: *const ValueWitnessTable, T: *const TypeInfo) -> TypeInfoPair {
    TypeInfoPair {
        vwt,
        T,
        flags: (*T).flags,
        x: 0,
        y: (*(*T).vwt).stride,
    }
}

///	export identity : $<type T>([in] T) -> [out] T
///	
///	body identity {
///	
///	entry(ret : $*T, x: $*T):
///		copy_addr x [take], ret [init]
///		return
///	}
unsafe fn identity(ret: *mut u8, x: *mut u8, T: *const TypeInfo) {
    ((*(*T).vwt).move_fn)(ret, x, T);
}

///	export second : $<type T>([in] Pair<$T>) -> [out] T
///	
///	body second {
///	
///	entry(ret : $*T, pair: $*Pair<$T>):
///		y = struct_addr pair, y
///		copy_addr y, ret [init]
///		drop_addr pair
///		return
///	}
unsafe fn second(ret: *mut u8, pair: *mut u8, T: *const TypeInfo) {
    let TP_VWT = mk_pair_vwt(T);
    let TP = mk_pair_ti(&TP_VWT, T);
    let field = pair.add((*(*T).vwt).stride);

    ((*(*T).vwt).move_fn)(ret, field, T);
    ((*TP.vwt).drop_fn)(pair, &TP as *const _ as *const TypeInfo);
}

///	export pair : $<type T>([in] T) -> [out] Pair<$T>
///	
///	body pair {
///	
///	entry(ret : $*Pair<$T>, val: $*T):
///		x = struct_addr ret, x
///		y = struct_addr ret, y
///		copy_addr val, x [init]
///		copy_addr val [take], y [init]
///		return
///	}
unsafe fn pair(ret: *mut u8, x: *mut u8, T: *const TypeInfo) {
    ((*(*T).vwt).copy_fn)(ret, x, T);
    ((*(*T).vwt).move_fn)(ret.add((*(*T).vwt).stride), x, T);
}

///	export combo : $<type T>([in] T) -> [out] Pair<$T>
///	
///	body combo {
///	
///	entry(ret : $*Pair<$T>, x : $*T):
///		pair = func_ref Pair
///		identity = func_ref identity
///		tmp = stack_alloc $T
///		apply pair<$T>(tmp, x)
///		apply identity<$Pair<$T>>(ret, tmp)
///		stack_free tmp
///		return
///	}
unsafe fn combo(ret: *mut u8, x: *mut u8, T: *const TypeInfo) {
    let TP_VWT = mk_pair_vwt(T);
    let TP = mk_pair_ti(&TP_VWT, T);

    let tmp = ::std::alloc::alloc(::std::alloc::Layout::from_size_align(
        (*TP.vwt).size,
        (*TP.vwt).align,
    ).unwrap());

    pair(tmp, x, T);
    identity(ret, tmp, &TP as *const _ as *const TypeInfo);

    ::std::alloc::dealloc(tmp, ::std::alloc::Layout::from_size_align(
        (*TP.vwt).size,
        (*TP.vwt).align,
    ).unwrap());
}

#[test]
fn test_identity() {
    let i32_info = &TRIVIAL_TIS[3];
    let mut ret: i32 = 0;
    let arg: i32 = 124;

    unsafe {
        identity(&mut ret as *mut _ as *mut u8, &arg as *const _ as *mut u8, i32_info);
    }

    assert_eq!(ret, 124);
}

#[test]
fn test_second() {
    let i32_info = &TRIVIAL_TIS[3];
    let mut ret: i32 = 0;
    let arg: Pair<i32> = Pair { x: 24, y: 12 };

    unsafe {
        second(&mut ret as *mut _ as *mut u8, &arg as *const _ as *mut u8, i32_info);
    }

    assert_eq!(ret, 12);
}

#[test]
fn test_pair() {
    let i32_info = &TRIVIAL_TIS[3];
    let mut ret: Pair<i32> = Pair { x: 0, y: 0 };
    let arg: i32 = 17;

    unsafe {
        pair(&mut ret as *mut _ as *mut u8, &arg as *const _ as *mut u8, i32_info);
    }

    assert_eq!(ret.x, 17);
    assert_eq!(ret.y, 17);
}

#[test]
fn test_combo() {
    let i32_info = &TRIVIAL_TIS[3];
    let mut ret: Pair<i32> = Pair { x: 0, y: 0 };
    let arg: i32 = 17;

    unsafe {
        combo(&mut ret as *mut _ as *mut u8, &arg as *const _ as *mut u8, i32_info);
    }

    assert_eq!(ret.x, 17);
    assert_eq!(ret.y, 17);
}
