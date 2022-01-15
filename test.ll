module test

type Pair
type List

export main : $(i32, **u8) -> i32
export identity : $<type T>([in] T) -> [out] T
export second : $<type T>([in] Pair<$T>) -> [out] T
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
    n120 = const_int 120, $i32
    pair = struct $Pair<$i32>(a: n120, b: argc)
    second = func_ref second
    t1 = stack_alloc $Pair<$i32>
    t2 = stack_alloc $i32
    store pair, t1
    apply second<$i32>(t2, t1)
    argc = load t2
    stack_free t2
    stack_free t1
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
    copy_addr x [take], ret [init]
    return
}


body second <type T> {

entry(ret : $*T, pair : $*Pair<$T>):
    a = struct_addr pair, b
    identity = func_ref identity
    apply identity<$T>(ret, a)
    return
}
