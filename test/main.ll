fn main() -> i32 {
    let $0: i32;
    let $1: i32;
    
    %0: {
        init $1;
        
        call($1 = const loop(), goto %1)
    }
    
    %1: {
        $0 = $1;
        drop $1;
        
        return
    }
}

fn loop() -> i32 {
    let $0: i32;
    let $1: i32;
    let $2: bool;
    let $3: i32;
    let $4: i32;
    let $5: i32;
    
    %0: {
        init $1;
        init $2;
        init $3;
        init $4;
        init $5;
        
        $3 = const 10i32;
        $4 = const 1i32;
        $5 = const 5i32;
        $2 = Lt($1, $3);
        
        assert($2, goto %1, unwind %2)
    }
    
    %1: {
        $1 = Add($1, $4);
        $0 = Add($0, $5);
        $2 = Lt($1, $3);
        
        assert($2, goto %1, unwind %2)
    }
    
    %2: {
        drop $1;
        drop $2;
        drop $3;
        drop $4;
        drop $5;
        
        return
    }
}