module test

local type Pair
local type List

export main : $(i32, **u8) -> i32
export identity : $<type T>([in] T) -> [out] T
export first : $<type T>([in] Pair<$T>) -> [out] T
local  print_args : $(i32, **u8) -> ()
import puts : $(*u8) -> i32
; import write : $(i32, *u8, usize) -> isize


struct Pair<type T> {
    a : $T,
    b : $T,
}

enum List<type T> {
    Nil,
    Cons : $(T, List<$T>),
}


body main {

entry(argc : $i32, argv : $**u8):
    identity = func_ref identity
    argv = apply identity<$**u8>(argv)
    print_args = func_ref print_args
    apply print_args(argc, argv)
    zero = const_int 0, $i32
    return zero
}


body print_args {

entry(argc : $i32, argv : $**u8):
    zero = const_int 0, $i32
    check = intrinsic "gt_i32"(argc, zero)
    switch check, 0: exit, default call

call:
    arg = load argv
    puts = func_ref puts
    _ = apply puts(arg)
    one = const_int 1, $i32
    argc = intrinsic "sub_i32"(argc, one)
    one = const_int 1, $isize
    argv = intrinsic "ptr_offset"<$i32>(argv, one)
    br entry(argc, argv)

exit:
    return
}


body identity <type T> {

entry(ret : $*T, x : $*T):
    copy_addr x, ret [init]
    return
}


body first <type T> {

entry(ret : $*T, pair : $*Pair<$T>):
    a = struct_addr pair, b
    copy_addr a, ret [init]
    return
}
