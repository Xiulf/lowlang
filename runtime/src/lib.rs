#![no_std]

extern crate core;

use core::mem::size_of;
use libc::{c_void, free, malloc};

#[repr(transparent)]
pub struct Box {
    ptr: *mut BoxInner,
}

#[repr(transparent)]
pub struct Weak {
    ptr: *mut BoxInner,
}

struct BoxInner {
    ptr: *mut c_void,
    strong_count: usize,
    weak_count: usize,
}

pub unsafe extern "C" fn box_alloc(size: usize) -> Box {
    let val = malloc(size);
    let ptr = malloc(size_of::<BoxInner>()) as *mut BoxInner;

    *ptr = BoxInner {
        ptr: val,
        strong_count: 1,
        weak_count: 0,
    };

    Box { ptr }
}

pub unsafe extern "C" fn box_copy(boxed: Box) {
    (*boxed.ptr).strong_count += 1;
}

pub unsafe extern "C" fn box_free(boxed: Box) {
    let strong_count = (*boxed.ptr).strong_count - 1;

    if strong_count == 0 {
        free((*boxed.ptr).ptr);
        free(boxed.ptr as *mut c_void);
    } else {
        (*boxed.ptr).strong_count = strong_count;
    }
}

pub unsafe extern "C" fn weak_alloc(boxed: Box) -> Weak {
    (*boxed.ptr).weak_count += 1;

    Weak { ptr: boxed.ptr }
}

pub unsafe extern "C" fn weak_copy(boxed: Weak) {
    (*boxed.ptr).weak_count += 1;
}

pub unsafe extern "C" fn weak_free(boxed: Weak) {
    (*boxed.ptr).weak_count -= 1;
}
