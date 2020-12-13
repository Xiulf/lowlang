export @main :: (i32, **i8) -> (i32)
local @incr :: (i32) -> (i32)
local @identity :: (T) -> (T)
local @apply :: ((A) -> (B), A) -> (B)

fn @main {
    ret _0 :: i32
    arg _1 :: i32
    arg _2 :: **i8
%0:
    call @apply(@incr, 22 : i32) -> _0
    return
}

fn @incr {
    ret _0 :: i32
    arg _1 :: i32
%0:
    _0 = #add_i32(_1, 1 : i32)
    return
}

fn @identity {
    ret _0 :: T
    arg _1 :: T
%0:
    _0 = _1
    return
}

fn @apply {
    ret _0 :: B
    arg _1 :: (A) -> (B)
    arg _2 :: A
%0:
    call _1(_2) -> _0
    return
}
