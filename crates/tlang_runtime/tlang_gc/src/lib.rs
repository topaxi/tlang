mod collector;
mod heap;
mod mark_sweep;
mod traits;

pub use collector::GcCollector;
pub use heap::{GcHandle, GcHeap};
pub use mark_sweep::GcRootProvider;
pub use traits::{GcTrace, GcTracer, Traceable};

/// Statistics about garbage collection runs
#[derive(Debug, Clone, Default)]
pub struct GcStats {
    pub collections_run: u64,
    pub objects_collected: u64,
    pub bytes_collected: u64,
    pub total_allocations: u64,
    pub total_bytes_allocated: u64,
}

/// Configuration for the garbage collector
#[derive(Debug, Clone)]
pub struct GcConfig {
    /// Trigger collection when heap reaches this size in bytes
    pub heap_size_threshold: usize,
    /// Trigger collection when this many objects are allocated
    pub allocation_threshold: usize,
    /// Enable debug logging for GC operations
    pub debug_logging: bool,
}

impl Default for GcConfig {
    fn default() -> Self {
        Self {
            heap_size_threshold: 1024 * 1024, // 1MB
            allocation_threshold: 1000,
            debug_logging: false,
        }
    }
}

/// Result of a garbage collection run
#[derive(Debug)]
pub struct GcResult {
    pub objects_collected: usize,
    pub bytes_freed: usize,
    pub duration_micros: u64,
}

#[cfg(test)]
mod tests {
    use super::*;

    // Simple test data structure
    #[derive(Debug)]
    struct TestObject {
        value: i32,
        references: Vec<usize>, // Handles to other objects
    }

    impl GcTrace for TestObject {
        fn trace(&self, tracer: &mut dyn GcTracer) {
            for &handle in &self.references {
                tracer.mark_object(handle);
            }
        }
    }

    impl Traceable for TestObject {}

    // Simple root provider for testing
    struct TestRootProvider {
        roots: Vec<usize>,
    }

    impl GcRootProvider for TestRootProvider {
        fn trace_roots(&self, tracer: &mut dyn GcTracer) {
            for &root in &self.roots {
                tracer.mark_object(root);
            }
        }
    }

    #[test]
    fn test_basic_allocation() {
        let mut gc = GcCollector::new_default();
        
        let obj1 = TestObject {
            value: 42,
            references: vec![],
        };
        
        let handle = gc.allocate(obj1);
        assert_eq!(handle, 1); // First handle should be 1
        
        let retrieved = gc.get(handle).unwrap();
        assert_eq!(retrieved.value, 42);
        assert_eq!(gc.object_count(), 1);
    }

    #[test] 
    fn test_garbage_collection() {
        let mut gc = GcCollector::new_mark_sweep(GcConfig {
            allocation_threshold: 2, // Trigger GC after 2 allocations
            heap_size_threshold: 1000000,
            debug_logging: true,
        });

        // Allocate some objects
        let handle1 = gc.allocate(TestObject {
            value: 1,
            references: vec![],
        });
        
        let handle2 = gc.allocate(TestObject {
            value: 2,
            references: vec![],
        });
        
        // Only keep handle1 as a root
        let root_provider = TestRootProvider {
            roots: vec![handle1],
        };
        
        assert_eq!(gc.object_count(), 2);
        
        // Force garbage collection
        let result = gc.force_collect(&root_provider);
        
        // Should collect one object (handle2)
        assert_eq!(result.objects_collected, 1);
        assert_eq!(gc.object_count(), 1);
        
        // handle1 should still be accessible
        assert!(gc.get(handle1).is_some());
        
        // handle2 should be gone
        assert!(gc.get(handle2).is_none());
    }

    #[test]
    fn test_object_references() {
        let mut gc = GcCollector::new_default();
        
        // Create objects that reference each other
        let handle1 = gc.allocate(TestObject {
            value: 1,
            references: vec![], // Will be updated
        });
        
        let handle2 = gc.allocate(TestObject {
            value: 2,
            references: vec![handle1], // References handle1
        });
        
        // Update handle1 to reference handle2 (create a cycle)
        if let Some(obj1) = gc.get_mut(handle1) {
            obj1.references.push(handle2);
        }
        
        // Only keep handle1 as root
        let root_provider = TestRootProvider {
            roots: vec![handle1],
        };
        
        assert_eq!(gc.object_count(), 2);
        
        // Force garbage collection
        let result = gc.force_collect(&root_provider);
        
        // Both objects should survive because they reference each other
        assert_eq!(result.objects_collected, 0);
        assert_eq!(gc.object_count(), 2);
    }

    #[test]
    fn test_automatic_gc_trigger() {
        let mut gc = GcCollector::new_mark_sweep(GcConfig {
            allocation_threshold: 2, // Trigger GC after 2 allocations
            heap_size_threshold: 1000000,
            debug_logging: false,
        });

        let root_provider = TestRootProvider {
            roots: vec![], // No roots - everything should be collected
        };

        // Allocate first two objects
        let _handle1 = gc.allocate_with_gc(TestObject {
            value: 1,
            references: vec![],
        }, &root_provider);
        
        let _handle2 = gc.allocate_with_gc(TestObject {
            value: 2,
            references: vec![],
        }, &root_provider);
        
        assert_eq!(gc.object_count(), 2);
        
        // This allocation should trigger GC since allocation_count_since_gc >= 2
        let _handle3 = gc.allocate_with_gc(TestObject {
            value: 3,
            references: vec![],
        }, &root_provider);
        
        // After GC, we should only have the last object allocated
        assert_eq!(gc.object_count(), 1);
    }

    #[test]
    fn test_gc_stats() {
        let mut gc = GcCollector::new_default();
        
        let root_provider = TestRootProvider {
            roots: vec![],
        };

        // Allocate and collect some objects
        let _handle1 = gc.allocate(TestObject {
            value: 1,
            references: vec![],
        });
        
        let _handle2 = gc.allocate(TestObject {
            value: 2,
            references: vec![],
        });
        
        let stats_before = gc.stats().clone();
        assert_eq!(stats_before.collections_run, 0);
        assert_eq!(stats_before.total_allocations, 2);
        
        let _result = gc.force_collect(&root_provider);
        
        let stats_after = gc.stats();
        assert_eq!(stats_after.collections_run, 1);
        assert_eq!(stats_after.objects_collected, 2);
        assert_eq!(stats_after.total_allocations, 2);
    }
}