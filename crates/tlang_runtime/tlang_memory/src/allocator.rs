use std::alloc::{self, Layout};
use std::ptr::{self, NonNull};
use std::sync::atomic::{AtomicUsize, Ordering};

// Size of each allocation block in bytes
const BLOCK_SIZE: usize = 1024 * 1024; // 1MB blocks

// Header for each allocated object
#[repr(C)]
struct ObjectHeader {
    // Size of the object in bytes
    size: usize,
    // Type tag for the object
    tag: ObjectTag,
    // Next object in the free list (if free)
    next: Option<NonNull<ObjectHeader>>,
    // Whether this object is marked during GC
    marked: bool,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ObjectTag {
    Struct,
    Enum,
    String,
    Slice,
    Closure,
    Function,
    NativeFunction,
}

// A block of memory that can be used for allocations
struct Block {
    // Pointer to the start of the block
    ptr: NonNull<u8>,
    // Size of the block in bytes
    size: usize,
    // Current offset in the block
    offset: AtomicUsize,
}

impl Block {
    fn new(size: usize) -> Self {
        let layout = Layout::from_size_align(size, 8).unwrap();
        let ptr = unsafe { alloc::alloc(layout) };
        let ptr = NonNull::new(ptr).unwrap();

        Self {
            ptr,
            size,
            offset: AtomicUsize::new(0),
        }
    }

    unsafe fn dealloc(&self) {
        let layout = Layout::from_size_align(self.size, 8).unwrap();
        unsafe {
            alloc::dealloc(self.ptr.as_ptr(), layout);
        }
    }
}

impl Drop for Block {
    fn drop(&mut self) {
        unsafe {
            self.dealloc();
        }
    }
}

pub struct Arena {
    // List of memory blocks
    blocks: Vec<Block>,
    // Free list for each object type
    free_lists: [Option<NonNull<ObjectHeader>>; 7],
    // Statistics
    total_allocated: AtomicUsize,
    total_freed: AtomicUsize,
}

impl Arena {
    pub fn new() -> Self {
        Self {
            blocks: Vec::new(),
            free_lists: [None; 7],
            total_allocated: AtomicUsize::new(0),
            total_freed: AtomicUsize::new(0),
        }
    }

    pub fn allocate<T>(&mut self, tag: ObjectTag) -> NonNull<T> {
        let size = std::mem::size_of::<T>();
        let layout = Layout::new::<ObjectHeader>()
            .extend(Layout::new::<T>())
            .unwrap()
            .0;

        // Try to find a free object of the right size
        let tag_index = tag as usize;
        if let Some(free) = self.free_lists[tag_index] {
            unsafe {
                let header = free.as_ref();
                if header.size >= size {
                    // Remove from free list
                    self.free_lists[tag_index] = header.next;
                    
                    // Update statistics
                    self.total_freed.fetch_sub(1, Ordering::SeqCst);
                    
                    // Return pointer to the object
                    return NonNull::new_unchecked(
                        free.as_ptr().add(1) as *mut T
                    );
                }
            }
        }

        // No free object found, allocate new space
        let block = if let Some(block) = self.blocks.last_mut() {
            let offset = block.offset.load(Ordering::Acquire);
            if offset + layout.size() <= block.size {
                block
            } else {
                self.allocate_new_block()
            }
        } else {
            self.allocate_new_block()
        };

        let offset = block.offset.fetch_add(layout.size(), Ordering::AcqRel);
        let ptr = unsafe { block.ptr.as_ptr().add(offset) };

        // Initialize header
        unsafe {
            let header = ptr as *mut ObjectHeader;
            ptr::write(header, ObjectHeader {
                size,
                tag,
                next: None,
                marked: false,
            });
        }

        // Update statistics
        self.total_allocated.fetch_add(1, Ordering::SeqCst);

        // Return pointer to the object
        unsafe {
            NonNull::new_unchecked(ptr.add(std::mem::size_of::<ObjectHeader>()) as *mut T)
        }
    }

    fn allocate_new_block(&mut self) -> &mut Block {
        let block = Block::new(BLOCK_SIZE);
        self.blocks.push(block);
        self.blocks.last_mut().unwrap()
    }

    pub fn deallocate<T>(&mut self, ptr: NonNull<T>) {
        unsafe {
            let header_ptr = (ptr.as_ptr() as *mut u8).sub(std::mem::size_of::<ObjectHeader>()) as *mut ObjectHeader;
            let header = &mut *header_ptr;
            
            // Add to free list
            let tag_index = header.tag as usize;
            header.next = self.free_lists[tag_index];
            self.free_lists[tag_index] = Some(NonNull::new_unchecked(header_ptr));
            
            // Update statistics
            self.total_freed.fetch_add(1, Ordering::SeqCst);
        }
    }

    pub fn get_stats(&self) -> (usize, usize) {
        (
            self.total_allocated.load(Ordering::Acquire),
            self.total_freed.load(Ordering::Acquire)
        )
    }

    // For garbage collection
    pub fn mark_object<T>(&self, ptr: NonNull<T>) {
        unsafe {
            let header_ptr = (ptr.as_ptr() as *mut u8).sub(std::mem::size_of::<ObjectHeader>()) as *mut ObjectHeader;
            (*header_ptr).marked = true;
        }
    }

    pub fn is_marked<T>(&self, ptr: NonNull<T>) -> bool {
        unsafe {
            let header_ptr = (ptr.as_ptr() as *mut u8).sub(std::mem::size_of::<ObjectHeader>()) as *mut ObjectHeader;
            (*header_ptr).marked
        }
    }

    pub fn get_object_tag<T>(&self, ptr: NonNull<T>) -> ObjectTag {
        unsafe {
            let header_ptr = (ptr.as_ptr() as *mut u8).sub(std::mem::size_of::<ObjectHeader>()) as *mut ObjectHeader;
            (*header_ptr).tag
        }
    }

    // Methods for shape storage
    pub fn insert<T>(&mut self, value: T) -> usize {
        let ptr = self.allocate(ObjectTag::Struct);
        unsafe {
            std::ptr::write(ptr.as_ptr(), value);
        }
        ptr.as_ptr() as usize
    }

    pub fn get<T>(&self, index: usize) -> Option<&T> {
        unsafe {
            let ptr = index as *const T;
            Some(&*ptr)
        }
    }

    pub fn get_mut<T>(&mut self, index: usize) -> Option<&mut T> {
        unsafe {
            let ptr = index as *mut T;
            Some(&mut *ptr)
        }
    }
}

// Safe wrapper for arena-allocated objects
pub struct ArenaBox<T> {
    ptr: NonNull<T>,
    arena: *mut Arena,
}

impl<T> ArenaBox<T> {
    pub fn new_in(arena: &mut Arena, tag: ObjectTag) -> Self {
        Self {
            ptr: arena.allocate(tag),
            arena: arena as *mut Arena,
        }
    }

    pub fn as_ptr(&self) -> *const T {
        self.ptr.as_ptr()
    }

    pub fn as_mut_ptr(&mut self) -> *mut T {
        self.ptr.as_ptr()
    }
}

impl<T> Drop for ArenaBox<T> {
    fn drop(&mut self) {
        unsafe {
            (*self.arena).deallocate(self.ptr);
        }
    }
}

impl<T> std::ops::Deref for ArenaBox<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.ptr.as_ptr() }
    }
}

impl<T> std::ops::DerefMut for ArenaBox<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.ptr.as_ptr() }
    }
} 