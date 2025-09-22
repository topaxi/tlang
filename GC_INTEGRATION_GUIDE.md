# Garbage Collector Integration Guide

This document explains how to use the garbage collector (GC) integration in the tlang interpreter.

## Overview

The tlang runtime now supports optional garbage collection through a modular design. When enabled, the GC automatically manages memory for objects, preventing memory leaks and handling cyclic references.

## Features

- **Automatic Memory Management**: Objects are automatically collected when no longer reachable
- **Cyclic Reference Support**: Handles complex object graphs including closures
- **Performance Monitoring**: Built-in statistics for memory usage and collection performance
- **Configurable Behavior**: Adjustable collection thresholds and debug logging
- **Optional Integration**: Can be enabled/disabled via feature flags

## Enabling the Garbage Collector

Add the `gc` feature to your `Cargo.toml`:

```toml
[dependencies]
tlang_interpreter = { path = "path/to/tlang_interpreter", features = ["gc"] }
```

## Basic Usage

### Creating a GC-Enabled Interpreter

```rust
use tlang_interpreter::Interpreter;

// Create interpreter with default GC settings
let interpreter = Interpreter::new();
```

### Custom GC Configuration

```rust
use tlang_interpreter::Interpreter;
use tlang_gc::GcConfig;

let gc_config = GcConfig {
    allocation_threshold: 1000,     // Trigger GC after 1000 allocations
    heap_size_threshold: 1024 * 1024, // Or when heap reaches 1MB
    debug_logging: true,            // Enable GC debug logging
};

let interpreter = Interpreter::with_gc_config(gc_config);
```

## Memory Management

### Manual Garbage Collection

```rust
let mut interpreter = Interpreter::new();

// Force garbage collection
interpreter.collect_garbage();
```

### Monitoring Memory Usage

```rust
let interpreter = Interpreter::new();

// Get current object count
let object_count = interpreter.object_count();
println!("Objects in heap: {}", object_count);

// Get total allocated bytes
let allocated_bytes = interpreter.allocated_bytes();
println!("Allocated bytes: {}", allocated_bytes);

// Get detailed GC statistics
let stats = interpreter.gc_stats();
println!("GC Stats: {:?}", stats);
```

## GC Statistics

The GC provides detailed statistics about its operation:

```rust
let stats = interpreter.gc_stats();

println!("Collections run: {}", stats.collections_run);
println!("Objects collected: {}", stats.objects_collected);
println!("Bytes collected: {}", stats.bytes_collected);
println!("Total allocations: {}", stats.total_allocations);
println!("Total bytes allocated: {}", stats.total_bytes_allocated);
```

## Configuration Options

### GcConfig Fields

- `allocation_threshold`: Number of allocations before triggering GC
- `heap_size_threshold`: Heap size in bytes before triggering GC
- `debug_logging`: Enable detailed GC operation logging

### Recommended Settings

**Development:**
```rust
GcConfig {
    allocation_threshold: 100,
    heap_size_threshold: 1024 * 1024, // 1MB
    debug_logging: true,
}
```

**Production:**
```rust
GcConfig {
    allocation_threshold: 10000,
    heap_size_threshold: 16 * 1024 * 1024, // 16MB
    debug_logging: false,
}
```

## Compatibility

### With GC Feature

When the `gc` feature is enabled:
- All standard interpreter functionality works
- Additional GC-specific methods are available
- Memory is automatically managed
- Performance monitoring is available

### Without GC Feature

When the `gc` feature is disabled:
- Standard interpreter functionality works normally
- Manual memory management (original behavior)
- GC-specific methods are not available
- No performance overhead from GC

## Performance Considerations

### When GC Triggers

The GC automatically triggers when:
1. Allocation count reaches the threshold, OR
2. Heap size reaches the size threshold

### Collection Performance

- **Collection time**: Generally proportional to number of live objects
- **Memory overhead**: Minimal overhead for GC metadata
- **Pause time**: Stop-the-world collection (brief pause)

### Tuning Tips

1. **High allocation rate**: Increase `allocation_threshold`
2. **Large objects**: Adjust `heap_size_threshold`
3. **Debugging**: Enable `debug_logging` temporarily
4. **Production**: Use higher thresholds for better performance

## Troubleshooting

### Common Issues

**High GC frequency**: Increase collection thresholds
```rust
GcConfig {
    allocation_threshold: 5000,  // Increase from default
    heap_size_threshold: 8 * 1024 * 1024, // 8MB
    debug_logging: false,
}
```

**Memory usage concerns**: Monitor with statistics
```rust
let stats = interpreter.gc_stats();
if stats.objects_collected < stats.total_allocations / 2 {
    // Consider manual collection or threshold adjustment
    interpreter.collect_garbage();
}
```

### Debug Logging

Enable debug logging to see GC operations:
```rust
GcConfig {
    // ... other settings
    debug_logging: true,
}
```

This will show messages like:
```
GC: Allocated object 1234 (256 bytes)
GC: Starting mark-and-sweep collection
GC: Marked object 1234 as reachable
GC: Collection complete - collected 10 objects (2048 bytes) in 150μs
```

## Migration Guide

### From Non-GC to GC

1. Add the `gc` feature to your dependencies
2. Replace `Interpreter::new()` calls (no code changes needed)
3. Optionally add GC configuration for performance tuning
4. Optionally add memory monitoring for insights

### Code Changes Required

**Before:**
```rust
let interpreter = Interpreter::new();
// No memory management needed
```

**After:**
```rust
let interpreter = Interpreter::new(); // Still works!
// OR with custom config:
let interpreter = Interpreter::with_gc_config(gc_config);

// Optional: Monitor memory usage
println!("Memory usage: {} objects", interpreter.object_count());
```

## Future Enhancements

The GC system is designed to support future improvements:
- **Incremental collection**: Reduce pause times
- **Generational collection**: Optimize for allocation patterns
- **Parallel collection**: Utilize multiple CPU cores
- **Custom allocators**: Integration with experimental allocator APIs

## Examples

See the `tests/gc_integration.rs` file for comprehensive examples of GC usage in different scenarios.