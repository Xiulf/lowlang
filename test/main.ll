fn main() -> i32 {
    let $0: i32;
    let $1: (i32, bool);
    
    %0: {
        StorageLive($1);
        
        $1 = Add(const 3i32, const 1i32);
        
        assert(!move($1.1)) -> %1;
    }
    
    %1: {
        $0 = move($1.0);
        return;
    }
}