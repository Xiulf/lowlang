use std::alloc::{alloc, dealloc, Layout};
use std::mem::size_of;

#[derive(Default)]
pub struct Heap {
    free_list: FreeList,
}

impl Heap {
    pub fn alloc(&mut self, size: usize) -> GenAllocResult {
        let ptr_size = size_of::<usize>();
        let new_size = next_power_of_2(ptr_size + size);
        let free_entry = self.free_list.find(new_size);

        if let Some(free_entry) = free_entry {
            let ptr = free_entry.ptr;
            let generation = free_entry.generation;

            free_entry.ptr = Ptr::null();

            GenAllocResult { ptr, generation }
        } else {
            let ptr = Ptr::alloc(new_size);

            unsafe {
                *(ptr.start() as *mut usize) = 0usize;
            }

            GenAllocResult { ptr, generation: 0 }
        }
    }

    pub fn free(&mut self, ptr: Ptr, size: usize) {
        let ptr_size = size_of::<usize>();
        let new_size = next_power_of_2(ptr_size + size);
        let new_ptr = ptr.start() as *mut usize;

        unsafe {
            *new_ptr += 1;
        }

        let gen = unsafe { *new_ptr };

        for e in &mut self.free_list.list {
            if e.ptr.is_null() {
                e.ptr = ptr;
                e.generation = gen;
            }
        }

        self.free_list.push(FreeEntry {
            ptr,
            size: new_size,
            generation: gen,
        });
    }

    pub fn generation(&self, ptr: Ptr) -> usize {
        let new_ptr = ptr.start() as *const usize;

        unsafe { *new_ptr }
    }
}

impl Drop for Heap {
    fn drop(&mut self) {
        self.free_list.clear();
    }
}

#[derive(Clone, Copy)]
pub struct GenAllocResult {
    pub ptr: Ptr,
    pub generation: usize,
}

#[derive(Clone, Copy)]
pub struct Ptr(*mut u8);

#[derive(Clone, Copy)]
struct FreeEntry {
    ptr: Ptr,
    generation: usize,
    size: usize,
}

#[derive(Default)]
struct FreeList {
    list: Vec<FreeEntry>,
}

unsafe impl Send for Ptr {
}

unsafe impl Sync for Ptr {
}

impl FreeList {
    fn push(&mut self, entry: FreeEntry) {
        self.list.push(entry);
    }

    fn find(&mut self, size: usize) -> Option<&mut FreeEntry> {
        for e in &mut self.list {
            if !e.ptr.is_null() && e.size == size {
                return Some(e);
            }
        }

        None
    }

    fn clear(&mut self) {
        self.list.drain(..).for_each(|e| e.ptr.dealloc(e.size));
    }
}

impl Ptr {
    fn null() -> Self {
        Self(std::ptr::null_mut())
    }

    fn alloc(size: usize) -> Self {
        let align = alignment(size);

        unsafe {
            let layout = Layout::from_size_align_unchecked(size, align);

            Self(alloc(layout).add(size_of::<usize>()))
        }
    }

    fn dealloc(self, size: usize) {
        let align = alignment(size);

        unsafe {
            let layout = Layout::from_size_align_unchecked(size, align);

            dealloc(self.start(), layout);
        }
    }

    fn start(self) -> *mut u8 {
        unsafe { self.0.sub(size_of::<usize>()) }
    }

    pub fn ptr(self) -> *mut u8 {
        self.0
    }

    fn is_null(self) -> bool {
        self.0.is_null()
    }
}

fn next_power_of_2(size: usize) -> usize {
    let mut value = 1;

    while value < size {
        value = value << 1;
    }

    value
}

fn alignment(mut size: usize) -> usize {
    if size == 0 {
        return 1;
    }

    let mut pow2 = 0;

    while (size & 1) == 0 {
        pow2 += 1;
        size >>= 1;
    }

    pow2
}
