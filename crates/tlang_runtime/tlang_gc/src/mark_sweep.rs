use std::time::Instant;
use log::{debug, info};

use crate::heap::{GcHandle, GcHeap};
use crate::traits::{GcTracer, Traceable};
use crate::{GcConfig, GcResult, GcStats};

/// Root set provider trait - implemented by the runtime to provide GC roots
pub trait GcRootProvider {
    /// Iterate over all root references and mark them
    fn trace_roots(&self, tracer: &mut dyn GcTracer);
}

/// Mark-and-sweep garbage collector
#[derive(Debug)]
pub struct MarkSweepCollector<T: Traceable> {
    heap: GcHeap<T>,
}

impl<T: Traceable> MarkSweepCollector<T> {
    pub fn new(config: GcConfig) -> Self {
        Self {
            heap: GcHeap::new(config),
        }
    }

    /// Allocate a new object in GC-managed memory
    pub fn allocate(&mut self, data: T) -> GcHandle {
        self.heap.allocate(data)
    }

    /// Get a reference to an object
    pub fn get(&self, handle: GcHandle) -> Option<&T> {
        self.heap.get(handle)
    }

    /// Get a mutable reference to an object
    pub fn get_mut(&mut self, handle: GcHandle) -> Option<&mut T> {
        self.heap.get_mut(handle)
    }

    /// Check if a garbage collection should be triggered
    pub fn should_collect(&self) -> bool {
        self.heap.should_collect()
    }

    /// Run a complete mark-and-sweep collection cycle
    pub fn collect(&mut self, root_provider: &dyn GcRootProvider) -> GcResult {
        let start_time = Instant::now();
        
        if self.heap.config.debug_logging {
            info!("GC: Starting mark-and-sweep collection");
            debug!("GC: {} objects in heap before collection", self.heap.object_count());
        }

        // Mark phase: trace from roots to mark all reachable objects
        let mut root_handles = Vec::new();
        root_provider.trace_roots(&mut RootCollector {
            handles: &mut root_handles,
        });
        
        // Mark and trace all objects reachable from roots iteratively
        self.heap.trace_from_roots(&root_handles);

        // Sweep phase: collect all unmarked objects
        let (objects_collected, bytes_freed) = self.heap.sweep();
        
        let duration = start_time.elapsed();
        let duration_micros = duration.as_micros() as u64;

        // Update statistics
        self.heap.stats.collections_run += 1;
        self.heap.stats.objects_collected += objects_collected as u64;
        self.heap.stats.bytes_collected += bytes_freed as u64;

        if self.heap.config.debug_logging {
            info!(
                "GC: Collection complete - collected {} objects ({} bytes) in {}μs",
                objects_collected, bytes_freed, duration_micros
            );
            debug!("GC: {} objects remaining in heap", self.heap.object_count());
        }

        GcResult {
            objects_collected,
            bytes_freed,
            duration_micros,
        }
    }

    /// Get current GC statistics
    pub fn stats(&self) -> &GcStats {
        self.heap.stats()
    }

    /// Get number of objects currently allocated
    pub fn object_count(&self) -> usize {
        self.heap.object_count()
    }

    /// Get total bytes currently allocated
    pub fn allocated_bytes(&self) -> usize {
        self.heap.allocated_bytes()
    }

    /// Force a garbage collection regardless of thresholds
    pub fn force_collect(&mut self, root_provider: &dyn GcRootProvider) -> GcResult {
        self.collect(root_provider)
    }
}

/// Root handle collector for the marking phase
struct RootCollector<'a> {
    handles: &'a mut Vec<GcHandle>,
}

impl<'a> GcTracer for RootCollector<'a> {
    fn mark_object(&mut self, handle: usize) {
        self.handles.push(handle);
    }
}