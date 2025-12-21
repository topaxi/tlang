# Garbage Collection Implementation Guide for tlang

This guide provides a comprehensive, step-by-step walkthrough for implementing garbage collection in the tlang runtime. It includes explanations of key concepts, references to learning materials, and specific implementation guidance for this codebase.

## Table of Contents

1. [Introduction to Garbage Collection](#introduction-to-garbage-collection)
2. [GC Concepts and Terminology](#gc-concepts-and-terminology)
3. [Learning Resources](#learning-resources)
4. [tlang Memory Architecture](#tlang-memory-architecture)
5. [Step-by-Step Implementation Guide](#step-by-step-implementation-guide)
6. [Testing Your GC](#testing-your-gc)
7. [Common Pitfalls](#common-pitfalls)
8. [Next Steps and Optimizations](#next-steps-and-optimizations)

---

## Introduction to Garbage Collection

### What is Garbage Collection?

Garbage collection (GC) is automatic memory management. Instead of requiring programmers to manually allocate and free memory, a garbage collector automatically identifies memory that is no longer in use and reclaims it.

### Why Do We Need GC in tlang?

Currently, tlang's interpreter has a memory leak by design:
- Objects are allocated into a `Slab` data structure but never removed
- Scope memory grows indefinitely to preserve values for closures
- Without GC, long-running programs will eventually run out of memory

### GC vs Manual Memory Management

| Approach | Pros | Cons |
|----------|------|------|
| **Manual** (like C) | Predictable performance, no pauses | Error-prone, memory leaks, use-after-free bugs |
| **Reference Counting** (like Swift) | Incremental, predictable | Can't handle cycles, overhead per operation |
| **Tracing GC** (like Java, Go) | Handles cycles, simpler programming model | Pause times, higher memory overhead |

For tlang, we recommend a **tracing mark-and-sweep GC** because:
- It's the simplest tracing GC to implement
- It handles circular references correctly
- The codebase already has infrastructure for object tracing

---

## GC Concepts and Terminology

### The Object Graph

Think of your program's memory as a graph:
- **Nodes** = Objects in memory (structs, closures, arrays, etc.)
- **Edges** = References from one object to another

```
       [Root Set]
           │
           ▼
    ┌─────────────┐
    │   Object A  │──────┐
    └─────────────┘      │
           │             ▼
           ▼      ┌─────────────┐
    ┌─────────────┐   │   Object C  │ ← Can be collected
    │   Object B  │   └─────────────┘   (unreachable)
    └─────────────┘
```

### Key Terms

1. **Roots (GC Roots)**: Starting points for finding live objects
   - Global variables
   - Local variables on the stack
   - Currently executing function arguments
   
2. **Live/Reachable Objects**: Objects that can be reached from roots by following references

3. **Dead/Unreachable Objects**: Objects with no path from any root - these are garbage

4. **Mark Phase**: Traverse the object graph from roots, marking all reachable objects

5. **Sweep Phase**: Scan all objects, freeing those not marked as reachable

6. **Safe Points**: Points in execution where it's safe to run GC (no half-constructed objects)

7. **Allocation Trigger**: Condition that starts a GC cycle (e.g., "every 1000 allocations")

### The Mark-and-Sweep Algorithm

```
function mark_and_sweep():
    // Phase 1: Mark
    worklist = get_all_roots()
    while worklist is not empty:
        object = worklist.pop()
        if object is not marked:
            mark(object)
            for each reference in object:
                worklist.push(reference)
    
    // Phase 2: Sweep
    for each object in heap:
        if object is not marked:
            free(object)
        else:
            unmark(object)  // Reset for next GC cycle
```

---

## Learning Resources

### Foundational Reading

1. **"The Garbage Collection Handbook"** by Jones, Hosking, and Moss
   - The definitive reference on GC algorithms
   - Chapters 1-3 cover mark-and-sweep in detail
   - https://gchandbook.org/

2. **"Crafting Interpreters"** by Bob Nystrom (Free online)
   - Chapter 26: "Garbage Collection" 
   - Excellent practical tutorial with code
   - https://craftinginterpreters.com/garbage-collection.html

3. **"A Unified Theory of Garbage Collection"** (Bacon, Cheng, Rajan)
   - Academic paper showing tracing and reference counting are duals
   - https://courses.cs.washington.edu/courses/cse590p/05au/p50-bacon.pdf

### Video Resources

1. **"Baby's First Garbage Collector"** by Bob Nystrom
   - Simple mark-and-sweep implementation walkthrough
   - https://journal.stuffwithstuff.com/2013/12/08/babys-first-garbage-collector/

2. **"Memory Management Reference"**
   - Comprehensive site covering many GC algorithms
   - https://www.memorymanagement.org/

### Rust-Specific Resources

1. **"rust-gc" crate** - Reference implementation of GC in Rust
   - https://github.com/Manishearth/rust-gc
   - Shows patterns for implementing GC in Rust

2. **"gc-arena" crate** - Arena-based GC for Rust
   - https://github.com/kyren/gc-arena
   - Good example of a simple, working GC

---

## tlang Memory Architecture

Before implementing GC, understand how tlang manages memory:

### Object Storage

```rust
// Location: crates/tlang_runtime/tlang_memory/src/state.rs

pub struct InterpreterState {
    // The heap - all objects live here
    objects: Slab<TlangObjectKind>,
    
    // Memory statistics (already implemented)
    memory_stats: MemoryStats,
    
    // Scope management
    scope_stack: ScopeStack,
    
    // Named globals
    globals: HashMap<String, TlangValue>,
    
    // ... other fields
}
```

### Value Representation

```rust
// Location: crates/tlang_runtime/tlang_memory/src/value.rs

pub enum TlangValue {
    Nil,
    Bool(bool),
    I64(i64),
    F64(f64),
    Object(TlangObjectId),  // ← Reference to heap object
    // ... other primitives
}
```

Key insight: **Only `TlangValue::Object(id)` values point to GC-managed objects.** Primitives don't need GC.

### Object Types

```rust
// Location: crates/tlang_runtime/tlang_memory/src/value/object.rs

pub enum TlangObjectKind {
    Fn(HirId),
    NativeFn,
    String(String),
    Struct(TlangStruct),       // Contains Vec<TlangValue>
    Enum(TlangEnum),           // Contains Vec<TlangValue>
    Slice(TlangSlice),         // Contains TlangValue reference
    Closure(TlangClosure),     // Contains captured_memory: Vec<TlangValue>
}
```

### Already Implemented Infrastructure

The following GC infrastructure is already in place:

1. **`TlangObjectKind::referenced_values()`** - Returns iterator of referenced values
2. **`MemoryStats`** - Tracks allocations/deallocations
3. **`InterpreterState::remove_object(id)`** - Removes object from heap
4. **`TlangClosure::captured_memory`** - Closures track their captured values

---

## Step-by-Step Implementation Guide

### Step 1: Implement Root Enumeration

**Goal**: Create a method that yields all GC roots.

**Location**: `crates/tlang_runtime/tlang_memory/src/state.rs`

```rust
impl InterpreterState {
    /// Returns an iterator over all GC root values.
    /// 
    /// Roots are values that are directly accessible from the running program
    /// and therefore must be considered "live" during garbage collection.
    pub fn gc_roots(&self) -> impl Iterator<Item = TlangValue> + '_ {
        // 1. Named global variables
        let globals_iter = self.globals.values().copied();
        
        // 2. Global memory (slot-based globals)
        let global_memory_iter = self.scope_stack.global_memory_iter();
        
        // 3. Local scope memory (stack variables)
        let local_memory_iter = self.scope_stack.local_memory_iter();
        
        // Chain all root sources together
        globals_iter
            .chain(global_memory_iter)
            .chain(local_memory_iter)
    }
}
```

You'll also need to add these methods to `ScopeStack`:

```rust
// Location: crates/tlang_runtime/tlang_memory/src/scope.rs

impl ScopeStack {
    pub fn global_memory_iter(&self) -> impl Iterator<Item = TlangValue> + '_ {
        self.global_memory.iter().copied()
    }
    
    pub fn local_memory_iter(&self) -> impl Iterator<Item = TlangValue> + '_ {
        self.memory.iter().copied()
    }
}
```

**Test your implementation**:
```rust
#[test]
fn test_gc_roots_includes_globals() {
    let mut state = InterpreterState::new();
    state.set_global("test".to_string(), TlangValue::I64(42));
    
    let roots: Vec<_> = state.gc_roots().collect();
    assert!(roots.contains(&TlangValue::I64(42)));
}
```

### Step 2: Implement the Mark Phase

**Goal**: Starting from roots, find all reachable objects.

**Location**: `crates/tlang_runtime/tlang_memory/src/state.rs`

```rust
use std::collections::HashSet;

impl InterpreterState {
    /// Marks all objects reachable from GC roots.
    /// Returns the set of reachable object IDs.
    pub fn mark_reachable(&self) -> HashSet<TlangObjectId> {
        let mut marked = HashSet::new();
        let mut worklist: Vec<TlangValue> = self.gc_roots().collect();
        
        while let Some(value) = worklist.pop() {
            // Only Object values can reference heap objects
            if let TlangValue::Object(id) = value {
                // Skip if already marked (handles cycles)
                if !marked.insert(id) {
                    continue;
                }
                
                // Get the object and add its references to worklist
                if let Some(obj) = self.objects.get(id) {
                    worklist.extend(obj.referenced_values());
                }
            }
        }
        
        marked
    }
}
```

**Key insight**: The `marked.insert(id)` call returns `false` if the ID was already in the set. This is how we handle circular references - we simply skip objects we've already visited.

**Test your implementation**:
```rust
#[test]
fn test_mark_phase_finds_reachable() {
    let mut state = InterpreterState::new();
    
    // Create a reachable object using new_string helper
    let obj = state.new_string("hello".to_string());
    state.set_global("my_string".to_string(), obj);
    
    let marked = state.mark_reachable();
    
    if let TlangValue::Object(id) = obj {
        assert!(marked.contains(&id));
    }
}

#[test]
fn test_mark_phase_ignores_unreachable() {
    let mut state = InterpreterState::new();
    
    // Create an unreachable object (no root reference)
    let orphan = state.new_string("orphan".to_string());
    let orphan_id = orphan.get_object_id().unwrap();
    
    let marked = state.mark_reachable();
    
    // Orphan should NOT be in marked set
    assert!(!marked.contains(&orphan_id));
}
```

### Step 3: Implement the Sweep Phase

**Goal**: Remove all objects that weren't marked.

```rust
impl InterpreterState {
    /// Removes all objects not in the marked set.
    /// Returns the number of objects collected.
    pub fn sweep_unreachable(&mut self, marked: &HashSet<TlangObjectId>) -> usize {
        // Collect IDs to remove (can't modify while iterating)
        let to_remove: Vec<TlangObjectId> = self.objects
            .iter()
            .map(|(id, _)| id)
            .filter(|id| !marked.contains(id))
            .collect();
        
        let count = to_remove.len();
        
        for id in to_remove {
            self.remove_object(id);
        }
        
        count
    }
}
```

**Test your implementation**:
```rust
#[test]
fn test_sweep_removes_unreachable() {
    let mut state = InterpreterState::new();
    
    // Create an object but don't make it a root
    let orphan = state.new_string("orphan".to_string());
    let orphan_id = orphan.get_object_id().unwrap();
    
    // Run GC phases
    let marked = state.mark_reachable();
    let collected = state.sweep_unreachable(&marked);
    
    assert_eq!(collected, 1);
    assert!(!state.contains_object(orphan_id));
}
```

### Step 4: Combine Mark and Sweep

**Goal**: Create the main GC entry point.

```rust
impl InterpreterState {
    /// Runs a complete garbage collection cycle.
    /// Returns the number of objects collected.
    pub fn collect_garbage(&mut self) -> usize {
        // Mark phase
        let marked = self.mark_reachable();
        
        // Sweep phase
        let collected = self.sweep_unreachable(&marked);
        
        // Update statistics
        self.memory_stats.gc_collections += 1;
        
        log::debug!("GC: collected {} objects, {} remain", 
            collected, self.objects.len());
        
        collected
    }
}
```

**Note**: `gc_collections` is already defined in `MemoryStats` (added in the prepare-gc branch):
```rust
pub struct MemoryStats {
    pub objects_allocated: usize,
    pub objects_deallocated: usize,
    pub gc_collections: usize,  // Already exists
}
```

### Step 5: Add GC Trigger

**Goal**: Decide when to run GC automatically.

```rust
impl InterpreterState {
    // Configuration constants
    const GC_THRESHOLD: usize = 1000;  // Run GC every N allocations
    
    /// Allocates a new object, possibly triggering GC first.
    pub fn new_object(&mut self, kind: TlangObjectKind) -> TlangValue {
        // Check if we should collect
        if self.should_collect() {
            self.collect_garbage();
        }
        
        // Normal allocation
        self.memory_stats.objects_allocated += 1;
        TlangValue::new_object(self.objects.insert(kind))
    }
    
    fn should_collect(&self) -> bool {
        // Simple strategy: collect every N allocations
        self.memory_stats.objects_allocated % Self::GC_THRESHOLD == 0
            && self.memory_stats.objects_allocated > 0
    }
}
```

**Better strategy (memory-based)**:
```rust
fn should_collect(&self) -> bool {
    // Collect when live objects exceed threshold
    let live_count = self.objects.len();
    let threshold = self.last_gc_live_count * 2;  // Double since last GC
    live_count >= threshold
}
```

### Step 6: Integration Testing

Create a test file that exercises GC in realistic scenarios:

```rust
#[test]
fn test_gc_collects_unreachable_objects() {
    let mut state = InterpreterState::new();

    // Create objects but don't make them roots
    let _orphan1 = state.new_string("orphan1".to_string());
    let _orphan2 = state.new_string("orphan2".to_string());

    assert_eq!(state.object_count(), 2);

    // Run GC - both should be collected
    let collected = state.collect_garbage();

    assert_eq!(collected, 2);
    assert_eq!(state.object_count(), 0);
    assert_eq!(state.memory_stats().gc_collections, 1);
}

#[test]
fn test_gc_preserves_reachable_objects() {
    let mut state = InterpreterState::new();

    // Create an object and make it a root via globals
    let reachable = state.new_string("keep_me".to_string());
    state.set_global("my_string".to_string(), reachable);

    // Create unreachable objects
    let _orphan = state.new_string("orphan".to_string());

    assert_eq!(state.object_count(), 2);

    // Run GC - only orphan should be collected
    let collected = state.collect_garbage();

    assert_eq!(collected, 1);
    assert_eq!(state.object_count(), 1);

    // The reachable object should still exist
    let id = reachable.get_object_id().unwrap();
    assert!(state.contains_object(id));
}

#[test]
fn test_gc_collects_temporary_objects() {
    let mut state = InterpreterState::new();
    
    // Simulate a loop that creates temporary objects
    for _ in 0..10_000 {
        let _temp = state.new_string("temp".to_string());
        // Don't store temp anywhere - it becomes garbage
    }
    
    // After GC, heap should be empty (no roots)
    state.collect_garbage();
    assert_eq!(state.object_count(), 0);
}

#[test]
fn test_gc_preserves_reachable_closures() {
    // Create a closure with captured values
    // Make the closure reachable via a global
    // Run GC
    // Verify captured values inside the closure survive GC
    // (The referenced_values() method returns captured_memory)
}

#[test]
fn test_gc_handles_circular_references() {
    // Create two objects that reference each other (e.g., structs)
    // Make them unreachable (remove from globals)
    // Verify they get collected (mark phase handles cycles via HashSet)
}
```

---

## Testing Your GC

### Unit Tests

Test each component in isolation:

1. **Root enumeration**: Test that all expected roots are found
2. **Mark phase**: Test that reachable objects are marked
3. **Sweep phase**: Test that unmarked objects are removed
4. **Edge cases**: Empty heap, single object, circular references

### Integration Tests

Run existing tlang programs and verify:

1. **No crashes**: Programs that worked before still work
2. **No premature collection**: Live objects aren't collected
3. **Memory bounded**: Long-running programs don't OOM

### Stress Tests

```tlang
// Create many temporary objects in a loop
fn stress_test() {
    for i in 0..100000 {
        let temp = [i, i+1, i+2];  // Creates temporary array
    }
}
```

---

## Common Pitfalls

### 1. Forgetting a Root Source

**Symptom**: Live objects get collected, causing crashes.

**Solution**: Audit all places values are stored:
- Global variables ✓
- Local variables (scope stack) ✓
- Function arguments (on the scope stack when called)
- Temporary values during expression evaluation

### 2. Not Handling Cycles

**Symptom**: Infinite loop during mark phase.

**Solution**: Always check if object is already marked before processing:
```rust
if !marked.insert(id) {
    continue;  // Skip already-visited objects
}
```

### 3. Collecting During Allocation

**Symptom**: Half-constructed objects cause crashes.

**Solution**: Only trigger GC at the *start* of allocation, before the new object exists:
```rust
pub fn new_object(&mut self, kind: TlangObjectKind) -> TlangValue {
    if self.should_collect() {
        self.collect_garbage();  // GC runs here
    }
    // Now safe to allocate
    self.objects.insert(kind)
}
```

### 4. Modifying Objects During GC

**Symptom**: Iterator invalidation, missing objects.

**Solution**: Collect IDs to remove first, then remove in separate pass:
```rust
let to_remove: Vec<_> = self.objects.iter()
    .filter(...)
    .collect();  // Collect first

for id in to_remove {
    self.remove_object(id);  // Then modify
}
```

---

## Next Steps and Optimizations

Once you have a working mark-and-sweep GC, consider these improvements:

### 1. Incremental/Concurrent GC

Reduce pause times by doing GC work between allocations:
- Tri-color marking (white/gray/black)
- Write barriers to track mutations

**Resources**:
- "Garbage Collection Handbook" Chapters 15-18
- Go's GC design: https://go.dev/blog/ismmkeynote

### 2. Generational GC

Collect young objects more frequently:
- "Nursery" for new objects (collected often)
- "Tenured" space for long-lived objects (collected rarely)

**Key insight**: Most objects die young ("generational hypothesis").

### 3. Moving/Compacting GC

Reduce fragmentation by moving live objects together:
- Requires updating all pointers
- More complex but better memory utilization

### 4. Scope Memory Truncation

Currently disabled in tlang. With GC properly collecting closure captures:
1. Closures use `captured_memory` for upvar access
2. `scope.pop()` can safely truncate memory
3. Memory usage stays proportional to live data

See `docs/gc-preparation-analysis.md` for detailed analysis.

---

## Quick Reference

### Files to Modify

| File | Changes Needed |
|------|----------------|
| `state.rs` | `gc_roots()`, `mark_reachable()`, `sweep_unreachable()`, `collect_garbage()` |
| `scope.rs` | `global_memory_iter()`, `local_memory_iter()` |
| `object.rs` | Already has `referenced_values()` ✓ |

### Existing Infrastructure

| Feature | Status | Location |
|---------|--------|----------|
| Object tracing | ✅ Done | `TlangObjectKind::referenced_values()` |
| Memory stats | ✅ Done | `MemoryStats`, `memory_stats()` |
| Object removal | ✅ Done | `remove_object(id)` |
| Closure captures | ✅ Done | `TlangClosure::captured_memory` |

### Implementation Checklist

- [ ] Add `gc_roots()` method
- [ ] Add `mark_reachable()` method  
- [ ] Add `sweep_unreachable()` method
- [ ] Add `collect_garbage()` method
- [ ] Add GC trigger in `new_object()`
- [ ] Add `gc_collections` counter
- [ ] Write unit tests for each component
- [ ] Write integration tests
- [ ] Run existing test suite
- [ ] Profile and tune GC threshold

---

## Summary

Implementing a garbage collector for tlang involves:

1. **Understanding the architecture**: Know where objects live and how they reference each other
2. **Finding roots**: Enumerate all directly-accessible values
3. **Tracing**: Follow references to find all live objects
4. **Sweeping**: Remove everything not found during tracing
5. **Triggering**: Decide when to run the collector
6. **Testing**: Verify correctness and performance

The tlang codebase already has the necessary infrastructure for object tracing and deallocation. The remaining work is to implement the mark-and-sweep algorithm and integrate it with the interpreter's allocation path.

Good luck with your implementation! 🎉
