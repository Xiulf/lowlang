fn main() -> i32 {
    let $0: i32;
    let $1: i32;
    
    %0: {
        init $1;
        
        call($1 = const factorial(const 6i32), goto %1)
    }
    
    %1: {
        $0 = move $1;
        
        return
    }
}

fn factorial($1: i32) -> i32 {
    let $0: i32;
    let $2: bool;
    let $3: i32;
    let $4: i32;
    
    %0: {
        init $2;
        
        $2 = Lt($1, const 1i32);
        
        assert($2, goto %1, unwind %2)
    }
    
    %1: {
        $0 = const 1i32;
        
        goto(%4)
    }
    
    %2: {
        init $3;
        init $4;
        
        $3 = Sub($1, const 1i32);
        
        call($4 = const factorial(move $3), goto %3)
    }
    
    %3: {
        $0 = Mul($1, $4);
        
        drop $4;
        
        goto(%4)
    }
    
    %4: {
        drop $2;
        
        return
    }
}

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