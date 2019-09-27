#[derive(Debug)]
pub struct Memory {
    pub(super) data: Vec<u8>,
    free: Vec<(usize, usize)>,
}

impl Memory {
    pub fn new() -> Memory {
        Memory {
            data: Vec::new(),
            free: Vec::new()
        }
    }
    
    pub fn alloc(&mut self, size: usize) -> usize {
        if let Some(index) = self.free.iter().position(|f| f.1 >= size) {
            let (loc, old_size) = self.free[index];
            
            self.free.remove(index);
            
            if old_size != size { self.free.push((loc + size, old_size - size)); }
            
            loc
        } else {
            let loc = self.data.len();
            
            for _ in 0..size { self.data.push(0); }
            
            loc
        }
    }
    
    pub fn free(&mut self, loc: usize, size: usize) {
        if loc + size == self.data.len() {
            for _ in 0..size { self.data.pop().unwrap(); }
        } else {
            self.free.push((loc, size));
            
            for i in 0..size { self.data[loc + i] = 0; }
        }
    }
    
    pub fn read_u8(&self, loc: usize) -> u8 {
        self.data[loc]
    }
    
    pub fn read_u32(&self, loc: usize) -> u32 {
        let bytes = [
            self.data[loc + 0],
            self.data[loc + 1],
            self.data[loc + 2],
            self.data[loc + 3],
        ];
        
        u32::from_le_bytes(bytes)
    }
    
    pub fn read(&self, loc: usize, size: usize) -> u64 {
        let bytes = match size {
            0 => [0, 0, 0, 0, 0, 0, 0, 0],
            1 => [self.data[loc + 0], 0, 0, 0, 0, 0, 0, 0],
            2 => [self.data[loc + 0], self.data[loc + 1], 0, 0, 0, 0, 0, 0],
            4 => [self.data[loc + 0], self.data[loc + 1], self.data[loc + 2], self.data[loc + 3], 0, 0, 0, 0],
            8 => [self.data[loc + 0], self.data[loc + 1], self.data[loc + 2], self.data[loc + 3],
                self.data[loc + 4], self.data[loc + 5], self.data[loc + 6], self.data[loc + 7]],
            _ => unreachable!()
        };
        
        u64::from_le_bytes(bytes)
    }
}

impl std::fmt::Display for Memory {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[")?;
        
        for i in 0..self.data.len() {
            if i != 0 { write!(f, ", ")?; }
            
            std::fmt::Display::fmt(&self.data[i], f)?;
        }
        
        write!(f, "]")
    }
}

impl std::fmt::Binary for Memory {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for byte in &self.data {
            std::fmt::Binary::fmt(byte, f)?;
        }
        
        Ok(())
    }
}