fn test() -> i32 {
    let $0: i32;
    
    %0: {
        $0 = const 32423i32;
        
        return
    }
}

fn main() -> i32 {
    let $0: i32;
    
    %0: {
        call($0 = const test(), goto %1)
    }
    
    %1: {
        return
    }
}