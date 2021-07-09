module test

; type Pair
; type UnionTest
; type Option

export main : $(i32, **u8) -> i32
; export identity : $<type T>([in] T) -> [out] T
local  print_args : $(i32, **u8) -> ()
import puts : $(*u8) -> i32
; import write : $(i32, *u8, usize) -> isize


; struct Pair<type T> {
;     a : $T,
;     b : $T,
; }
; 
; union UnionTest {
;     a : $i32,
;     b : $u16,
; }
; 
; enum Option<type T> {
;     None,
;     Some : $(T),
; }


body main {

entry(argc : $i32, argv : $**u8):
    ; union = stack_alloc $UnionTest

    ; identity = func_ref identity
    ; argv = apply identity<$**u8>(argv)
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


; body identity <type T> {
; 
; entry(ret : $*T, x : $*T):
;     copy_addr x, ret [init]
;     return
; }
