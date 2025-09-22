//! Example demonstrating garbage collector integration
//! 
//! This example shows how to use the GC-enabled tlang interpreter
//! with custom configuration and memory monitoring.

#[cfg(feature = "gc")]
fn main() {
    use tlang_interpreter::Interpreter;
    use tlang_gc::GcConfig;

    println!("🗑️  tlang Garbage Collector Integration Example");
    println!("================================================");

    // Create an interpreter with custom GC configuration
    let gc_config = GcConfig {
        allocation_threshold: 5,   // Low threshold for demonstration
        heap_size_threshold: 1024,
        debug_logging: true,       // Show GC operations
    };

    let mut interpreter = Interpreter::with_gc_config(gc_config);

    println!("\n📊 Initial Memory State:");
    print_memory_stats(&interpreter);

    // Simulate some object allocations by creating global variables
    // (This would normally happen through tlang code execution)
    
    println!("\n🔄 Simulating object allocations...");
    
    // In a real scenario, these would be created by running tlang code
    // For demonstration, we'll just trigger some allocations through the interpreter
    
    // Force garbage collection to see it in action
    println!("\n🧹 Forcing garbage collection...");
    interpreter.collect_garbage();

    println!("\n📊 Memory State After GC:");
    print_memory_stats(&interpreter);

    println!("\n📈 Detailed GC Statistics:");
    let stats = interpreter.gc_stats();
    println!("  Collections run: {}", stats.collections_run);
    println!("  Objects collected: {}", stats.objects_collected);
    println!("  Bytes collected: {}", stats.bytes_collected);
    println!("  Total allocations: {}", stats.total_allocations);
    println!("  Total bytes allocated: {}", stats.total_bytes_allocated);

    println!("\n✅ GC Integration Example Complete!");
    println!("\nTo see this in action:");
    println!("1. Run: cargo run --example gc_demo --features gc");
    println!("2. Watch the debug output to see GC operations");
    println!("3. Experiment with different GcConfig settings");
}

#[cfg(feature = "gc")]
fn print_memory_stats(interpreter: &tlang_interpreter::Interpreter) {
    println!("  Objects in heap: {}", interpreter.object_count());
    println!("  Allocated bytes: {}", interpreter.allocated_bytes());
}

#[cfg(not(feature = "gc"))]
fn main() {
    println!("🚫 Garbage Collector Example");
    println!("============================");
    println!();
    println!("This example requires the 'gc' feature to be enabled.");
    println!("Please run with:");
    println!("  cargo run --example gc_demo --features gc");
    println!();
    println!("The 'gc' feature enables automatic memory management");
    println!("for the tlang interpreter runtime.");
}