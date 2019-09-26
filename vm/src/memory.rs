#[derive(Debug)]
pub struct Memory {
    pub(super) stack: Vec<u8>,
}

impl Memory {
    pub fn new() -> Memory {
        Memory {
            stack: Vec::new()
        }
    }
    
    pub fn read_u8(&self, loc: usize) -> u8 {
        self.stack[loc]
    }
    
    pub fn read_u32(&self, loc: usize) -> u32 {
        let bytes = [
            self.stack[loc + 0],
            self.stack[loc + 1],
            self.stack[loc + 2],
            self.stack[loc + 3],
        ];
        
        u32::from_le_bytes(bytes)
    }
    
    pub fn read(&self, loc: usize, size: usize) -> u64 {
        let bytes = match size {
            0 => [0, 0, 0, 0, 0, 0, 0, 0],
            1 => [self.stack[loc + 0], 0, 0, 0, 0, 0, 0, 0],
            2 => [self.stack[loc + 0], self.stack[loc + 1], 0, 0, 0, 0, 0, 0],
            4 => [self.stack[loc + 0], self.stack[loc + 1], self.stack[loc + 2], self.stack[loc + 3], 0, 0, 0, 0],
            8 => [self.stack[loc + 0], self.stack[loc + 1], self.stack[loc + 2], self.stack[loc + 3],
                self.stack[loc + 4], self.stack[loc + 5], self.stack[loc + 6], self.stack[loc + 7]],
            _ => unreachable!()
        };
        
        u64::from_le_bytes(bytes)
    }
}