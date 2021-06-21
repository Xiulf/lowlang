module "test"

export "main" : $(i32, **u8) -> [out] i32
local "factorial" : $(i32) -> i32

body "main" {

entry(argc : $i32, argv : $**u8):
    fac = func_ref "factorial"
    five = const_int 5, $i32
    res = apply fac(five)
    return res
}

body "factorial" {

entry(num : $i32):
    zero = const_int 0, $i32
    one = const_int 1, $i32
    v3 = intrinsic "le_i32"(num, zero)
    switch v3, 0: block2, default block1

block1:
    return one

block2:
    fac = func_ref "factorial"
    v5 = intrinsic "sub_i32"(num, one)
    v6 = apply fac(v5)
    v7 = intrinsic "mul_i32"(num, v6)
    return v7
}
