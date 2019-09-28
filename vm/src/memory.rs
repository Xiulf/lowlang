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
    
    pub fn clear(&mut self, loc: usize, size: usize) {
        for i in 0..size { self.data[loc + i] = 0; }
    }
    
    pub fn read_u8(&self, loc: usize) -> u8 {
        self.data[loc]
    }
    
    pub fn read_u16(&self, loc: usize) -> u16 {
        let bytes = [
            self.data[loc + 0],
            self.data[loc + 1],
        ];
        
        u16::from_le_bytes(bytes)
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
    
    pub fn read_u64(&self, loc: usize) -> u64 {
        let bytes = [
            self.data[loc + 0],
            self.data[loc + 1],
            self.data[loc + 2],
            self.data[loc + 3],
            self.data[loc + 4],
            self.data[loc + 5],
            self.data[loc + 6],
            self.data[loc + 7],
        ];
        
        u64::from_le_bytes(bytes)
    }
    
    pub fn read_i8(&self, loc: usize) -> i8 {
        i8::from_le_bytes([self.data[loc]])
    }
    
    pub fn read_i16(&self, loc: usize) -> i16 {
        let bytes = [
            self.data[loc + 0],
            self.data[loc + 1],
        ];
        
        i16::from_le_bytes(bytes)
    }
    
    pub fn read_i32(&self, loc: usize) -> i32 {
        let bytes = [
            self.data[loc + 0],
            self.data[loc + 1],
            self.data[loc + 2],
            self.data[loc + 3],
        ];
        
        i32::from_le_bytes(bytes)
    }
    
    pub fn read_i64(&self, loc: usize) -> i64 {
        let bytes = [
            self.data[loc + 0],
            self.data[loc + 1],
            self.data[loc + 2],
            self.data[loc + 3],
            self.data[loc + 4],
            self.data[loc + 5],
            self.data[loc + 6],
            self.data[loc + 7],
        ];
        
        i64::from_le_bytes(bytes)
    }
    
    pub fn read_f32(&self, loc: usize) -> f32 {
        f32::from_bits(self.read_u32(loc))
    }
    
    pub fn read_f64(&self, loc: usize) -> f64 {
        f64::from_bits(self.read_u64(loc))
    }
    
    pub fn write(&mut self, mut loc: usize, value: crate::Value) -> usize {
        use crate::Value::*;
        
        match value {
            Unit => 0,
            U8(v) => { self.data[loc] = v; 1 },
            U16(v) => { let bytes = v.to_le_bytes(); for i in 0..2 { self.data[loc + i] = bytes[i]; } 2 },
            U32(v) => { let bytes = v.to_le_bytes(); for i in 0..4 { self.data[loc + i] = bytes[i]; } 4 },
            U64(v) => { let bytes = v.to_le_bytes(); for i in 0..8 { self.data[loc + i] = bytes[i]; } 8 },
            I8(v) => { self.data[loc] = v.to_le_bytes()[0]; 1 },
            I16(v) => { let bytes = v.to_le_bytes(); for i in 0..2 { self.data[loc + i] = bytes[i]; } 2 },
            I32(v) => { let bytes = v.to_le_bytes(); for i in 0..4 { self.data[loc + i] = bytes[i]; } 4 },
            I64(v) => { let bytes = v.to_le_bytes(); for i in 0..8 { self.data[loc + i] = bytes[i]; } 8 },
            F32(v) => { let bytes = v.to_bits().to_le_bytes(); for i in 0..4 { self.data[loc + i] = bytes[i]; } 4 },
            F64(v) => { let bytes = v.to_bits().to_le_bytes(); for i in 0..8 { self.data[loc + i] = bytes[i]; } 8 },
            Ptr(v, v2) => {
                let bytes = (v as u32).to_le_bytes(); for i in 0..4 { self.data[loc + i] = bytes[i]; } loc += 4;
                let bytes = (v2 as u32).to_le_bytes(); for i in 0..4 { self.data[loc + i] = bytes[i]; }
                8
            },
            Tuple(vs) => {
                let mut total = 0;
                
                for v in vs {
                    let written = self.write(loc, v);
                    
                    total += written;
                    loc += written;
                }
                
                total
            }
        }
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