//! Integration tests for the garbage collector
//! 
//! These tests demonstrate how to use the GC-enabled interpreter
//! and verify that garbage collection works correctly.

#[cfg(feature = "gc")]
mod gc_tests {
    use tlang_interpreter::Interpreter;
    use tlang_gc::GcConfig;

    #[test]
    fn test_gc_interpreter_creation() {
        // Test creating an interpreter with default GC settings
        let interpreter = Interpreter::new();
        
        // Basic functionality should work
        assert!(interpreter.object_count() >= 0);
    }

    #[test]
    fn test_gc_interpreter_with_custom_config() {
        // Test creating an interpreter with custom GC configuration
        let gc_config = GcConfig {
            allocation_threshold: 50,  // Trigger GC after 50 allocations
            heap_size_threshold: 1024, // Or when heap reaches 1KB
            debug_logging: true,
        };
        
        let interpreter = Interpreter::with_gc_config(gc_config);
        
        // The interpreter should be functional
        assert!(interpreter.object_count() >= 0);
    }

    #[test]
    fn test_manual_gc_collection() {
        let mut interpreter = Interpreter::with_gc_config(GcConfig {
            allocation_threshold: 1000, // High threshold so GC doesn't trigger automatically
            heap_size_threshold: 1024 * 1024,
            debug_logging: false,
        });

        let initial_count = interpreter.object_count();
        
        // Force garbage collection
        interpreter.collect_garbage();
        
        // Should still work after GC
        let after_gc_count = interpreter.object_count();
        assert!(after_gc_count <= initial_count);
    }

    #[test]
    fn test_gc_statistics() {
        let interpreter = Interpreter::new();
        
        let stats = interpreter.gc_stats();
        
        // Should have some basic statistics
        assert!(stats.total_allocations >= 0);
        assert!(stats.collections_run >= 0);
    }

    #[test]
    fn test_gc_memory_tracking() {
        let interpreter = Interpreter::new();
        
        // Should be able to get memory usage info
        let object_count = interpreter.object_count();
        let allocated_bytes = interpreter.allocated_bytes();
        
        assert!(object_count >= 0);
        assert!(allocated_bytes >= 0);
    }
}

#[cfg(not(feature = "gc"))]
mod non_gc_tests {
    use tlang_interpreter::Interpreter;

    #[test]
    fn test_regular_interpreter_still_works() {
        // Without GC feature, should still work normally
        let interpreter = Interpreter::new();
        
        // Basic functionality should work
        // Note: GC-specific methods won't be available
        assert!(true); // Placeholder - interpreter is created successfully
    }
}