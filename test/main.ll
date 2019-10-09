fn loop() -> i32 {
    let $0: i32;
    let $1: i32;
    let $2: bool;
    
    %0: {
        init $1;
        init $2;
        
        $2 = Lt($1, const 10i32);
        
        assert($2, goto %1, unwind %2)
    }
    
    %1: {
        $1 = Add($1, const 1i32);
        $0 = Add($0, const 5i32);
        $2 = Lt($1, const 10i32);
        
        assert($2, goto %1, unwind %2)
    }
    
    %2: {
        drop $1;
        drop $2;
        
        return
    }
}

fn main() -> i32 {
    let $0: i32;
    
    %0: {
        call($0 = const loop(), goto %1)
    }
    
    %1: {
        return
    }
}