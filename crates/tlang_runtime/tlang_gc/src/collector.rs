use crate::heap::GcHandle;
use crate::mark_sweep::{GcRootProvider, MarkSweepCollector};
use crate::traits::Traceable;
use crate::{GcConfig, GcResult, GcStats};

/// Main garbage collector interface
#[derive(Debug)]
pub enum GcCollector<T: Traceable> {
    MarkSweep(MarkSweepCollector<T>),
    // Future: Incremental(IncrementalCollector<T>),
    // Future: Generational(GenerationalCollector<T>),
}

impl<T: Traceable> GcCollector<T> {
    /// Create a new mark-and-sweep collector
    pub fn new_mark_sweep(config: GcConfig) -> Self {
        Self::MarkSweep(MarkSweepCollector::new(config))
    }

    /// Create a new collector with default configuration
    pub fn new_default() -> Self {
        Self::new_mark_sweep(GcConfig::default())
    }

    /// Allocate a new object in GC-managed memory
    pub fn allocate(&mut self, data: T) -> GcHandle {
        match self {
            Self::MarkSweep(collector) => collector.allocate(data),
        }
    }

    /// Get a reference to an object
    pub fn get(&self, handle: GcHandle) -> Option<&T> {
        match self {
            Self::MarkSweep(collector) => collector.get(handle),
        }
    }

    /// Get a mutable reference to an object
    pub fn get_mut(&mut self, handle: GcHandle) -> Option<&mut T> {
        match self {
            Self::MarkSweep(collector) => collector.get_mut(handle),
        }
    }

    /// Check if a garbage collection should be triggered
    pub fn should_collect(&self) -> bool {
        match self {
            Self::MarkSweep(collector) => collector.should_collect(),
        }
    }

    /// Run a garbage collection cycle
    pub fn collect(&mut self, root_provider: &dyn GcRootProvider) -> GcResult {
        match self {
            Self::MarkSweep(collector) => collector.collect(root_provider),
        }
    }

    /// Force a garbage collection regardless of thresholds
    pub fn force_collect(&mut self, root_provider: &dyn GcRootProvider) -> GcResult {
        match self {
            Self::MarkSweep(collector) => collector.force_collect(root_provider),
        }
    }

    /// Get current GC statistics
    pub fn stats(&self) -> &GcStats {
        match self {
            Self::MarkSweep(collector) => collector.stats(),
        }
    }

    /// Get number of objects currently allocated
    pub fn object_count(&self) -> usize {
        match self {
            Self::MarkSweep(collector) => collector.object_count(),
        }
    }

    /// Get total bytes currently allocated
    pub fn allocated_bytes(&self) -> usize {
        match self {
            Self::MarkSweep(collector) => collector.allocated_bytes(),
        }
    }

    /// Allocate and potentially trigger garbage collection
    pub fn allocate_with_gc(&mut self, data: T, root_provider: &dyn GcRootProvider) -> GcHandle {
        // Check if we should collect before allocating
        if self.should_collect() {
            self.collect(root_provider);
        }
        
        self.allocate(data)
    }
}