fn main() -> i32 {
    let $0: i32;
    let $1: i32;
    let $2: i32;
    
    %0: {
        init $1;
        init $2;
        
        $1 = const 4i32;
        $2 = const 5i32;
        $0 = Add($1, $2);
        
        drop $1;
        drop $2;
        
        return
    }
}