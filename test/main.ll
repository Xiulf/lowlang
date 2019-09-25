fn test() -> i32 {
    let $0: i32;
    
    %0: {
        $0 = const 4i32;
        return
    }
}

fn main() -> i32 {
    let $0: i32;
    let $1: i32;
    
    %0: {
        StorageLive($1);
        call($1 = const test(), goto %1)
    }
    
    %1: {
        $0 = copy $1;
        StorageDead($1);
        return
    }
}