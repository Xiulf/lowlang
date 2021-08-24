use bytemuck::Pod;
use std::mem::{size_of, ManuallyDrop};
use std::ptr::{copy_nonoverlapping, read_unaligned};

#[derive(Debug)]
pub struct Stack {
    inner: Vec<u8>,
    top: usize,
}

#[derive(Debug)]
pub enum StackError {
    Overflow,
    Underflow,
}

impl Default for Stack {
    fn default() -> Self {
        Self::new(4096)
    }
}

impl Stack {
    pub fn new(max_size: usize) -> Self {
        Self {
            inner: vec![0; max_size],
            top: 0,
        }
    }

    pub unsafe fn resize(&mut self, max_size: usize) {
        self.inner.resize(max_size, 0);
    }

    pub fn push<T: Pod>(&mut self, val: T) -> Result<usize, StackError> {
        let val = ManuallyDrop::new(val);
        let bytes = bytemuck::bytes_of(&val);

        if bytes.len() + self.top >= self.inner.capacity() {
            return Err(StackError::Overflow);
        }

        unsafe {
            copy_nonoverlapping(bytes.as_ptr(), &mut self.inner[self.top], bytes.len());
        }

        let ptr = self.top;

        self.top += bytes.len();

        Ok(ptr)
    }

    pub fn pop<T: Pod>(&mut self) -> Result<T, StackError> {
        let size = size_of::<T>();

        if self.top < size {
            return Err(StackError::Underflow);
        }

        let bytes = &self.inner[self.top - size..self.top];
        let val = bytemuck::from_bytes(bytes);
        let val = unsafe { read_unaligned(val) };

        self.top -= size;

        Ok(val)
    }

    pub fn alloc(&mut self, size: usize) -> Result<usize, StackError> {
        if self.top + size >= self.inner.capacity() {
            return Err(StackError::Overflow);
        }

        let ptr = self.top;

        self.top += size;

        Ok(ptr)
    }

    pub fn free(&mut self, size: usize) -> Result<(), StackError> {
        if self.top < size {
            return Err(StackError::Underflow);
        }

        self.top -= size;

        Ok(())
    }
}
