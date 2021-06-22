module "test"

export "main" : $(i32, **u8) -> i32
local  "print_args" : $(i32, **u8) -> ()
import "puts" : $(*u8) -> i32
import "write" : $(i32, *u8, usize) -> isize

body "main" {

entry(argc : $i32, argv : $**u8):
    print_args = func_ref "print_args"
    apply print_args(argc, argv)
    zero = const_int 0, $i32
    return zero
}


body "print_args" {

entry(argc : $i32, argv : $**u8):
    zero = const_int 0, $i32
    check = intrinsic "gt_i32"(argc, zero)
    switch check, 0: exit, default call

call:
    arg = load argv
    puts = func_ref "puts"
    _ = apply puts(arg)
    one = const_int 1, $i32
    argc = intrinsic "sub_i32"(argc, one)
    one = const_int 1, $isize
    argv = intrinsic "ptr_offset"<$i32>(argv, one)
    br entry(argc, argv)

exit:
    return
}
