module test

export main : $(i32, **u8) -> i32
export test : $<type T>() -> ()


body main {

entry(argc : $i32, argv : $**u8):
    test = func_ref test
    apply test<$i32>()
    zero = const_int 0, $i32
    return zero
}


body test <type T> {

entry:
    x = box_alloc gen $T
    a = box_addr x
    box_free x
    return
}
