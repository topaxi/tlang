use std::alloc::{self, AllocError, Allocator, GlobalAlloc, Layout};
use std::cell::UnsafeCell;
use std::marker::PhantomData;
use std::ptr::{self, NonNull};
use std::sync::OnceLock;
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

// Explicitly mark ObjectHeader as Send and Sync
unsafe impl Send for ObjectHeader {}
unsafe impl Sync for ObjectHeader {}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ObjectTag {
    Struct,
    Enum,
    String,
    Slice,
    Closure,
    Function,
    NativeFunction,
    Vec,
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

// Explicitly mark Block as Send and Sync
unsafe impl Send for Block {}
unsafe impl Sync for Block {}

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
    blocks: UnsafeCell<Vec<Block>>,
    // Free list for each object type
    free_lists: [UnsafeCell<Option<NonNull<ObjectHeader>>>; 8], // Updated size to include Vec
    // Statistics
    total_allocated: AtomicUsize,
    total_freed: AtomicUsize,
}

// Wrapper type for global allocator
struct GlobalArenaAllocator;

// Global arena allocator for static use
#[global_allocator]
static GLOBAL_ALLOCATOR: GlobalArenaAllocator = GlobalArenaAllocator;

// Global Arena instance for static use
static GLOBAL_ARENA: OnceLock<Arena> = OnceLock::new();

// Implement GlobalAlloc for the wrapper
unsafe impl GlobalAlloc for GlobalArenaAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        // Use the system allocator
        unsafe { alloc::System.alloc(layout) }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        // Use the system allocator
        unsafe { alloc::System.dealloc(ptr, layout) }
    }
}

unsafe impl GlobalAlloc for Arena {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let size = layout.size();
        let align = layout.align();

        // Find a suitable block or allocate a new one
        let blocks = unsafe { &mut *self.blocks.get() };

        let block_ptr = if let Some(block) = blocks.last() {
            let offset = block.offset.load(Ordering::Acquire);
            let aligned_offset = (offset + align - 1) & !(align - 1); // Align the offset

            if aligned_offset + size <= block.size {
                // Use existing block
                let _actual_offset = block
                    .offset
                    .fetch_add(size + (aligned_offset - offset), Ordering::AcqRel);
                unsafe { block.ptr.as_ptr().add(aligned_offset) }
            } else {
                // Need a new block
                let new_block_size = std::cmp::max(BLOCK_SIZE, size + align);
                let new_block = Block::new(new_block_size);
                let ptr = new_block.ptr.as_ptr();
                new_block.offset.store(size, Ordering::Release);
                blocks.push(new_block);
                ptr
            }
        } else {
            // First block allocation
            let new_block_size = std::cmp::max(BLOCK_SIZE, size + align);
            let new_block = Block::new(new_block_size);
            let ptr = new_block.ptr.as_ptr();
            new_block.offset.store(size, Ordering::Release);
            blocks.push(new_block);
            ptr
        };

        block_ptr
    }

    unsafe fn dealloc(&self, _ptr: *mut u8, _layout: Layout) {
        // No-op - we use garbage collection
    }
}

// Implement Allocator trait for Arena
unsafe impl Allocator for Arena {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        let ptr = unsafe { self.alloc(layout) };
        if ptr.is_null() {
            return Err(AllocError);
        }

        let ptr = NonNull::new(ptr).unwrap();
        Ok(NonNull::slice_from_raw_parts(ptr, layout.size()))
    }

    unsafe fn deallocate(&self, _ptr: NonNull<u8>, _layout: Layout) {
        // No-op - we use garbage collection
    }
}

// Mark Arena as Send and Sync
unsafe impl Send for Arena {}
unsafe impl Sync for Arena {}

impl Arena {
    pub fn new() -> Self {
        Self {
            blocks: UnsafeCell::new(Vec::new()),
            free_lists: Default::default(), // Initialize array of UnsafeCells
            total_allocated: AtomicUsize::new(0),
            total_freed: AtomicUsize::new(0),
        }
    }

    // Get the global arena instance
    pub fn global() -> &'static Arena {
        GLOBAL_ARENA.get_or_init(|| Arena::new())
    }

    pub fn allocate<T>(&self, tag: ObjectTag) -> NonNull<T> {
        let size = std::mem::size_of::<T>();
        let layout = Layout::new::<ObjectHeader>()
            .extend(Layout::new::<T>())
            .unwrap()
            .0;

        // Try to find a free object of the right size
        let tag_index = tag as usize;
        let free_list = unsafe { &mut *self.free_lists[tag_index].get() };
        if let Some(free) = *free_list {
            unsafe {
                let header = free.as_ref();
                if header.size >= size {
                    // Remove from free list
                    *free_list = header.next;

                    // Update statistics
                    self.total_freed.fetch_sub(1, Ordering::SeqCst);

                    // Return pointer to the object
                    return NonNull::new_unchecked(free.as_ptr().add(1) as *mut T);
                }
            }
        }

        // No free object found, allocate new space
        let blocks = unsafe { &mut *self.blocks.get() };
        let block = if let Some(block) = blocks.last_mut() {
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
            ptr::write(
                header,
                ObjectHeader {
                    size,
                    tag,
                    next: None,
                    marked: false,
                },
            );
        }

        // Update statistics
        self.total_allocated.fetch_add(1, Ordering::SeqCst);

        // Return pointer to the object
        unsafe { NonNull::new_unchecked(ptr.add(std::mem::size_of::<ObjectHeader>()) as *mut T) }
    }

    fn allocate_new_block(&self) -> &mut Block {
        let blocks = unsafe { &mut *self.blocks.get() };
        let block = Block::new(BLOCK_SIZE);
        blocks.push(block);
        blocks.last_mut().unwrap()
    }

    pub fn deallocate<T>(&self, ptr: NonNull<T>) {
        unsafe {
            let header_ptr = (ptr.as_ptr() as *mut u8).sub(std::mem::size_of::<ObjectHeader>())
                as *mut ObjectHeader;
            let header = &mut *header_ptr;

            // Add to free list
            let tag_index = header.tag as usize;
            let free_list = &mut *self.free_lists[tag_index].get();
            header.next = *free_list;
            *free_list = Some(NonNull::new_unchecked(header_ptr));

            // Update statistics
            self.total_freed.fetch_add(1, Ordering::SeqCst);
        }
    }

    pub fn get_stats(&self) -> (usize, usize) {
        (
            self.total_allocated.load(Ordering::Acquire),
            self.total_freed.load(Ordering::Acquire),
        )
    }

    // For garbage collection
    pub fn mark_object<T>(&self, ptr: NonNull<T>) {
        unsafe {
            let header_ptr = (ptr.as_ptr() as *mut u8).sub(std::mem::size_of::<ObjectHeader>())
                as *mut ObjectHeader;
            (*header_ptr).marked = true;
        }
    }

    pub fn is_marked<T>(&self, ptr: NonNull<T>) -> bool {
        unsafe {
            let header_ptr = (ptr.as_ptr() as *mut u8).sub(std::mem::size_of::<ObjectHeader>())
                as *mut ObjectHeader;
            (*header_ptr).marked
        }
    }

    pub fn get_object_tag<T>(&self, ptr: NonNull<T>) -> ObjectTag {
        unsafe {
            let header_ptr = (ptr.as_ptr() as *mut u8).sub(std::mem::size_of::<ObjectHeader>())
                as *mut ObjectHeader;
            (*header_ptr).tag
        }
    }

    // Methods for shape storage
    pub fn insert<T>(&self, value: T) -> usize {
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

    pub fn get_mut<T>(&self, index: usize) -> Option<&mut T> {
        unsafe {
            let ptr = index as *mut T;
            Some(&mut *ptr)
        }
    }

    // Allocate a Vec using this arena
    pub fn new_vec<T>(&self) -> Vec<T, &Arena> {
        Vec::new_in(self)
    }

    // Create a Vec with the given capacity using this arena
    pub fn new_vec_with_capacity<T>(&self, capacity: usize) -> Vec<T, &Arena> {
        Vec::with_capacity_in(capacity, self)
    }
}

// Safe wrapper for arena-allocated objects
pub struct ArenaBox<T> {
    ptr: NonNull<T>,
    arena: *const Arena,
    _phantom: PhantomData<T>, // Add PhantomData for correct variance
}

impl<T> ArenaBox<T> {
    pub fn new_in(arena: &Arena, tag: ObjectTag) -> Self {
        Self {
            ptr: arena.allocate(tag),
            arena,
            _phantom: PhantomData,
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
            // No need to convert to NonNull<u8> anymore, our deallocate takes T directly
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
