use crate::sig;
use crate::ty::*;
use std::collections::HashMap;
use std::lazy::SyncLazy;

pub static INTRINSICS: SyncLazy<HashMap<&'static str, Ty>> = SyncLazy::new(|| {
    let mut map = HashMap::new();
    let int32 = Ty::int(Integer::I32, true);

    map.insert("add.i32", sig!(int32 -> int32));

    map
});
