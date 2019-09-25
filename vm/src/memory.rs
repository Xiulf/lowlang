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
    
    pub fn read_u32(&self, loc: usize) -> u32 {
        let bytes = [
            self.stack[loc + 0],
            self.stack[loc + 1],
            self.stack[loc + 2],
            self.stack[loc + 3],
        ];
        
        u32::from_le_bytes(bytes)
    }
}