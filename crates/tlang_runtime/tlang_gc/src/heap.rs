use indexmap::IndexMap;
use log::{debug, trace};

use crate::traits::Traceable;
use crate::{GcConfig, GcStats};

/// A handle to an object in GC-managed memory
pub type GcHandle = usize;

/// An object stored in the GC heap with metadata
#[derive(Debug)]
struct GcObject<T: Traceable> {
    data: T,
    marked: bool,
    size_bytes: usize,
}

impl<T: Traceable> GcObject<T> {
    fn new(data: T) -> Self {
        let size_bytes = std::mem::size_of::<T>();
        Self {
            data,
            marked: false,
            size_bytes,
        }
    }
}

/// The GC-managed heap containing all objects
#[derive(Debug)]
pub struct GcHeap<T: Traceable> {
    objects: IndexMap<GcHandle, GcObject<T>>,
    next_handle: GcHandle,
    pub config: GcConfig,
    pub stats: GcStats,
    allocation_count_since_gc: usize,
    total_allocated_bytes: usize,
}

impl<T: Traceable> GcHeap<T> {
    pub fn new(config: GcConfig) -> Self {
        Self {
            objects: IndexMap::new(),
            next_handle: 1, // Start from 1, reserve 0 for null/invalid
            config,
            stats: GcStats::default(),
            allocation_count_since_gc: 0,
            total_allocated_bytes: 0,
        }
    }

    /// Allocate a new object in the heap, returning a handle to it
    pub fn allocate(&mut self, data: T) -> GcHandle {
        let handle = self.next_handle;
        self.next_handle += 1;

        let obj = GcObject::new(data);
        let size_bytes = obj.size_bytes;

        self.objects.insert(handle, obj);
        self.allocation_count_since_gc += 1;
        self.total_allocated_bytes += size_bytes;
        self.stats.total_allocations += 1;
        self.stats.total_bytes_allocated += size_bytes as u64;

        if self.config.debug_logging {
            debug!("GC: Allocated object {} ({} bytes)", handle, size_bytes);
        }

        handle
    }

    /// Get a reference to an object by its handle
    pub fn get(&self, handle: GcHandle) -> Option<&T> {
        self.objects.get(&handle).map(|obj| &obj.data)
    }

    /// Get a mutable reference to an object by its handle
    pub fn get_mut(&mut self, handle: GcHandle) -> Option<&mut T> {
        self.objects.get_mut(&handle).map(|obj| &mut obj.data)
    }

    /// Mark an object as reachable during garbage collection
    pub fn mark_object(&mut self, handle: GcHandle) {
        if let Some(obj) = self.objects.get_mut(&handle) {
            if !obj.marked {
                obj.marked = true;
                if self.config.debug_logging {
                    trace!("GC: Marked object {} as reachable", handle);
                }
            }
        }
    }

    /// Trace all references from a set of root handles iteratively (avoids stack overflow)
    pub fn trace_from_roots(&mut self, roots: &[GcHandle]) {
        let mut work_list: Vec<GcHandle> = Vec::new();
        
        // First mark all root objects and add them to work list
        for &handle in roots {
            if let Some(obj) = self.objects.get_mut(&handle) {
                if !obj.marked {
                    obj.marked = true;
                    work_list.push(handle);
                    if self.config.debug_logging {
                        trace!("GC: Marked root object {} as reachable", handle);
                    }
                }
            }
        }
        
        // Process work list iteratively
        while let Some(handle) = work_list.pop() {
            // Collect handles that this object references
            let mut handles_to_trace = Vec::new();
            
            if let Some(obj) = self.objects.get(&handle) {
                obj.data.trace(&mut HandleCollector {
                    handles: &mut handles_to_trace,
                });
            }
            
            // Mark referenced objects and add to work list if not already marked
            for ref_handle in handles_to_trace {
                if let Some(ref_obj) = self.objects.get_mut(&ref_handle) {
                    if !ref_obj.marked {
                        ref_obj.marked = true;
                        work_list.push(ref_handle);
                        if self.config.debug_logging {
                            trace!("GC: Marked object {} as reachable", ref_handle);
                        }
                    }
                }
            }
        }
    }

    /// Check if garbage collection should be triggered
    pub fn should_collect(&self) -> bool {
        self.allocation_count_since_gc >= self.config.allocation_threshold ||
        self.total_allocated_bytes >= self.config.heap_size_threshold
    }

    /// Sweep phase: remove all unmarked objects
    pub fn sweep(&mut self) -> (usize, usize) {
        let mut collected_objects = 0;
        let mut freed_bytes = 0;

        // Collect handles of unmarked objects
        let to_remove: Vec<GcHandle> = self.objects
            .iter()
            .filter_map(|(handle, obj)| {
                if !obj.marked {
                    Some(*handle)
                } else {
                    None
                }
            })
            .collect();

        // Remove unmarked objects
        for handle in to_remove {
            if let Some(obj) = self.objects.shift_remove(&handle) {
                collected_objects += 1;
                freed_bytes += obj.size_bytes;
                self.total_allocated_bytes -= obj.size_bytes;
                
                if self.config.debug_logging {
                    trace!("GC: Collected object {} ({} bytes)", handle, obj.size_bytes);
                }
            }
        }

        // Reset marks for next collection
        for obj in self.objects.values_mut() {
            obj.marked = false;
        }

        self.allocation_count_since_gc = 0;
        (collected_objects, freed_bytes)
    }

    /// Get current statistics
    pub fn stats(&self) -> &GcStats {
        &self.stats
    }

    /// Get number of objects currently in the heap
    pub fn object_count(&self) -> usize {
        self.objects.len()
    }

    /// Get total bytes currently allocated
    pub fn allocated_bytes(&self) -> usize {
        self.total_allocated_bytes
    }
}

/// Handle collector for avoiding borrowing conflicts during tracing
struct HandleCollector<'a> {
    handles: &'a mut Vec<GcHandle>,
}

impl<'a> crate::traits::GcTracer for HandleCollector<'a> {
    fn mark_object(&mut self, handle: usize) {
        self.handles.push(handle);
    }
}