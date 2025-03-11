use std::alloc::{Allocator, Layout};
use std::cell::UnsafeCell;
use std::collections::VecDeque;
use std::marker::PhantomData;
use std::ptr::NonNull;

/// A typed bump-style allocator for objects of type `T`
pub struct Arena<T> {
    memory: UnsafeCell<Vec<T>>,             // Now supports interior mutability
    free_list: UnsafeCell<VecDeque<usize>>, // Track freed slots
    _marker: PhantomData<T>,
}

impl<T> Arena<T> {
    /// Create a new arena with a preallocated capacity.
    pub fn new(capacity: usize) -> Self {
        Self {
            memory: UnsafeCell::new(Vec::with_capacity(capacity)),
            free_list: UnsafeCell::new(VecDeque::new()),
            _marker: PhantomData,
        }
    }

    /// Allocate a new object in the arena.
    pub fn allocate(&self, value: T) -> NonNull<T> {
        let memory = unsafe { &mut *self.memory.get() };
        let free_list = unsafe { &mut *self.free_list.get() };

        if let Some(free_index) = free_list.pop_front() {
            memory[free_index] = value;
            return NonNull::new(&mut memory[free_index]).unwrap();
        }

        let index = memory.len();
        memory.push(value);
        NonNull::new(&mut memory[index]).unwrap()
    }

    /// Deallocate an object (for GC).
    pub fn deallocate(&self, ptr: NonNull<T>) {
        let memory = unsafe { &mut *self.memory.get() };
        let free_list = unsafe { &mut *self.free_list.get() };

        let index = (ptr.as_ptr() as usize - memory.as_ptr() as usize) / std::mem::size_of::<T>();
        free_list.push_back(index);
    }
}

unsafe impl<T> Allocator for Arena<T> {
    /// Allocates memory for `layout`, ensuring proper alignment.
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, std::alloc::AllocError> {
        let size = layout.size();
        let align = layout.align();

        if size != std::mem::size_of::<T>() || align != std::mem::align_of::<T>() {
            return Err(std::alloc::AllocError);
        }

        let ptr = self.allocate(unsafe { std::mem::zeroed::<T>() });

        Ok(NonNull::slice_from_raw_parts(ptr.cast(), size))
    }

    /// Deallocates memory at the given pointer.
    unsafe fn deallocate(&self, ptr: NonNull<u8>, _layout: Layout) {
        let obj_ptr = ptr.cast::<T>();
        self.deallocate(obj_ptr);
    }
}
