use crate::db::IrDatabase;
use crate::ty::*;
use crate::{generic, sig};
use std::collections::HashMap;
use std::sync::Arc;

pub fn intrinsics(db: &dyn IrDatabase) -> Arc<HashMap<&'static str, Ty>> {
    let mut map = HashMap::new();
    let int32 = Ty::int(db, Integer::I32, true);
    let isize = Ty::int(db, Integer::ISize, true);
    let boolean = Ty::int(db, Integer::I8, false);

    map.insert("add_i32", sig!(db; int32, int32 => int32));
    map.insert("sub_i32", sig!(db; int32, int32 => int32));
    map.insert("mul_i32", sig!(db; int32, int32 => int32));
    map.insert("div_i32", sig!(db; int32, int32 => int32));
    map.insert("rem_i32", sig!(db; int32, int32 => int32));
    map.insert("eq_i32", sig!(db; int32, int32 => boolean));
    map.insert("ne_i32", sig!(db; int32, int32 => boolean));
    map.insert("lt_i32", sig!(db; int32, int32 => boolean));
    map.insert("le_i32", sig!(db; int32, int32 => boolean));
    map.insert("gt_i32", sig!(db; int32, int32 => boolean));
    map.insert("ge_i32", sig!(db; int32, int32 => boolean));

    map.insert("ptr_offset", generic!(db; t:Type in sig!(db; t.ptr(db), isize => t.ptr(db))));

    Arc::new(map)
}
