# Garbage Collection Implementation Guide for tlang

This guide provides a comprehensive, step-by-step walkthrough for implementing garbage collection in the tlang runtime. It includes explanations of key concepts, references to learning materials, and specific implementation guidance for this codebase.

## Table of Contents

1. [Introduction to Garbage Collection](#introduction-to-garbage-collection)
2. [GC Concepts and Terminology](#gc-concepts-and-terminology)
3. [Learning Resources](#learning-resources)
4. [tlang Memory Architecture](#tlang-memory-architecture)
5. [Step-by-Step Implementation Guide](#step-by-step-implementation-guide)
6. [Verifying GC Correctness](#verifying-gc-correctness)
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
    │   Object A  │──────────┐
    └─────────────┘          │
           │                 ▼
           ▼          ┌─────────────┐
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

**What GC actually collects**: The GC manages *heap objects* (entries in the `Slab<TlangObjectKind>`), not individual `TlangValue`s. A `TlangValue` is just a 16-byte Copy handle: primitives are stored inline, while `Object(id)` is an index into the heap.

During the **mark phase**, only `TlangValue::Object(id)` values need to be followed -- primitives don't lead anywhere. But be careful not to confuse this with "primitives don't need GC": when a closure captures primitive values, those primitives live inside the closure's `captured_memory: Vec<TlangValue>`, which is owned by the closure *object* on the heap. When the GC collects that closure object, the captured primitives are freed along with it. The unit of collection is always a heap object, not an individual value.

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
    Cell(TlangCell),           // GC-managed mutable cell
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
        // 1. Temporary roots (intermediate values on the Rust call stack,
        //    registered by the evaluator to protect them from collection)
        let temp_roots_iter = self.temp_roots.iter().copied();

        // 2. Named global variables
        let globals_iter = self.globals.values().copied();

        // 3. Global memory (slot-based globals)
        let global_memory_iter = self.scope_stack.global_memory_iter();

        // 4. Local scope memory (only LIVE scopes, not stale memory)
        let local_memory_iter = self.scope_stack.live_local_memory_iter();

        // 5. Native function object IDs (these reference NativeFn objects in the heap)
        let native_fn_iter = self.native_fns.keys().copied().map(TlangValue::Object);

        // Chain all root sources together
        temp_roots_iter
            .chain(globals_iter)
            .chain(global_memory_iter)
            .chain(local_memory_iter)
            .chain(native_fn_iter)
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

    /// Iterates only over local memory belonging to LIVE scopes.
    ///
    /// IMPORTANT: Do NOT iterate over all of `self.memory` blindly. Because
    /// `ScopeStack::pop()` does not truncate the memory vector (to preserve
    /// closure references), there may be stale values from exited scopes
    /// lingering at the end. Iterating all of `self.memory` would treat those
    /// stale values as roots, preventing their referenced objects from ever
    /// being collected -- defeating the purpose of GC.
    ///
    /// Instead, compute the end of the last live scope and only iterate up
    /// to that point.
    pub fn live_local_memory_iter(&self) -> impl Iterator<Item = TlangValue> + '_ {
        let live_end = self.scopes.last().map_or(0, |scope| {
            // The last scope's memory extends to wherever its variables end.
            // Since push_value appends and set_local may extend, use the max
            // of scope.start() + scope.size() and actual memory length up to
            // that scope's known extent.
            (scope.start() + scope.size()).min(self.memory.len())
        });
        self.memory[..live_end].iter().copied()
    }
}
```

**Why `live_local_memory_iter` instead of iterating all of `memory`?**

The `ScopeStack` never truncates `memory` when scopes are popped (to preserve closure references). This means `self.memory` can contain stale values from exited scopes. If root enumeration scans all of `self.memory`, those stale entries act as "phantom roots" that keep objects alive indefinitely -- exactly the leak GC is supposed to fix. By only scanning memory belonging to currently live scopes, we allow objects referenced only by dead scopes to be collected.

Note: this is safe because closures store their own copy of captured values in `captured_memory`, which is traced via `referenced_values()`. Values in stale scope memory that are still needed are reachable through live closure objects.

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

#### Choosing a safe trigger point

GC can only run at **safe points** -- moments when no temporary `TlangValue::Object` references exist on the Rust call stack outside of the interpreter state. If GC runs while the evaluator holds a temporary object reference in a local Rust variable (e.g., partway through evaluating a binary expression), the referenced object might be swept even though it's still needed.

**Option A: Trigger at statement boundaries (recommended)**

Instead of triggering GC inside `new_object()`, trigger it between statements in the evaluator. At statement boundaries, all live values are stored in the scope stack or globals -- nothing is held as a Rust-local temporary.

```rust
// In the evaluator, between statements:
if state.should_collect() {
    state.collect_garbage();
}
```

**Option B: Trigger inside `new_object()` (simpler but riskier)**

This is simpler to implement but requires careful auditing. Every call site that creates objects while holding temporary object references becomes a potential bug:

```rust
impl InterpreterState {
    const GC_INITIAL_THRESHOLD: usize = 1000;

    pub fn new_object(&mut self, kind: TlangObjectKind) -> TlangValue {
        if self.should_collect() {
            self.collect_garbage();
        }

        self.memory_stats.objects_allocated += 1;
        TlangValue::new_object(self.objects.insert(kind))
    }
}
```

If you choose Option B, use GC stress testing (see [Verifying GC Correctness](#verifying-gc-correctness)) to find bugs: trigger GC on *every* allocation and run the full test suite.

#### Collection policy

Use a growth-based threshold rather than a simple modulo counter. A modulo counter (`objects_allocated % N == 0`) checks total lifetime allocations, which is incorrect -- after many allocations, the counter drifts and collections become unpredictable.

```rust
impl InterpreterState {
    const GC_INITIAL_THRESHOLD: usize = 1000;

    fn should_collect(&self) -> bool {
        // Collect when live objects have doubled since the last collection.
        // This is the strategy used by Lua and Crafting Interpreters.
        self.objects.len() >= self.next_gc_threshold
    }
}
```

After each collection, update the threshold:
```rust
pub fn collect_garbage(&mut self) -> usize {
    let marked = self.mark_reachable();
    let collected = self.sweep_unreachable(&marked);

    self.memory_stats.gc_collections += 1;
    // Next GC when live objects double (with a minimum floor)
    self.next_gc_threshold = (self.objects.len() * 2).max(Self::GC_INITIAL_THRESHOLD);

    log::debug!("GC: collected {} objects, {} remain, next at {}",
        collected, self.objects.len(), self.next_gc_threshold);

    collected
}
```

You'll need to add `next_gc_threshold: usize` to `InterpreterState`, initialized to `GC_INITIAL_THRESHOLD`.

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

## Verifying GC Correctness

A GC has two failure modes, and you need strategies to detect both:

1. **Memory leak**: Garbage is not collected (objects stay alive when they shouldn't)
2. **Premature collection**: Live objects are collected (use-after-free)

Premature collection is far more dangerous -- it causes crashes, silent data corruption, or wrong results. Leaks are merely wasteful. Invest most of your testing effort in detecting premature collection.

### Strategy 1: GC Stress Mode (Most Important)

Add a **stress mode** that triggers GC on *every* allocation. This maximizes the chance of exposing premature collection bugs by collecting as aggressively as possible.

```rust
impl InterpreterState {
    /// When true, run GC before every allocation.
    /// Use only for testing -- extremely slow.
    pub gc_stress: bool,

    fn should_collect(&self) -> bool {
        if self.gc_stress {
            return true;
        }
        self.objects.len() >= self.next_gc_threshold
    }
}
```

Then run the **entire existing test suite** with stress mode enabled:

```rust
#[test]
fn test_all_programs_survive_gc_stress() {
    // For each .tlang test file, run it with gc_stress = true.
    // If any test crashes or produces different output, you have a bug.
}
```

This is the single most effective technique for finding GC bugs. If your GC survives stress mode across the full test suite, it's likely correct.

### Strategy 2: Leak Detection via MemoryStats

After running a program that should produce no long-lived objects, verify the heap is empty:

```rust
#[test]
fn test_no_leaks_after_execution() {
    let mut state = InterpreterState::new();

    // Run a program that creates and discards many objects
    // ... evaluate program ...

    state.collect_garbage();

    // After the program completes and all scopes are exited,
    // only global-rooted objects should remain.
    let stats = state.memory_stats();
    println!(
        "allocated={}, deallocated={}, live={}",
        stats.objects_allocated, stats.objects_deallocated, stats.live_objects()
    );

    // For programs with no persistent state, live objects should be zero
    // (or equal to the number of global objects like native functions)
    assert_eq!(state.object_count(), expected_globals_count);
}
```

### Strategy 3: Object Existence Assertions

After GC, verify that every value reachable from roots still points to a valid object:

```rust
impl InterpreterState {
    /// Debug assertion: verify all root values point to valid objects.
    /// Call this after every GC cycle during development.
    #[cfg(debug_assertions)]
    pub fn assert_roots_valid(&self) {
        for value in self.gc_roots() {
            if let TlangValue::Object(id) = value {
                assert!(
                    self.contains_object(id),
                    "GC BUG: root references collected object {id}"
                );
            }
        }
    }
}
```

Call this at the end of `collect_garbage()` during development.

### Strategy 4: Unit Tests

Test each GC component in isolation:

1. **Root enumeration**: Verify all expected roots are found (globals, scope values, native fns)
2. **Mark phase**: Verify reachable objects are marked, unreachable are not
3. **Sweep phase**: Verify unmarked objects are removed
4. **Edge cases**: Empty heap, single object, circular references, deeply nested closures

### Strategy 5: Integration Tests with Output Comparison

Run existing `.tlang` test programs and compare output with GC enabled vs. disabled. Any difference indicates a GC bug:

```rust
#[test]
fn test_gc_does_not_change_program_behavior() {
    for test_file in glob("tests/**/*.tlang") {
        let output_without_gc = run_program(test_file, gc_enabled: false);
        let output_with_gc = run_program(test_file, gc_enabled: true);
        assert_eq!(output_without_gc, output_with_gc,
            "GC changed behavior of {test_file}");
    }
}
```

### Strategy 6: Memory Bounded Stress Tests

Verify that long-running programs don't run out of memory:

```tlang
// This should complete without OOM if GC is working
fn stress_test() {
    for i in 0..100000 {
        let temp = [i, i+1, i+2];  // Creates temporary array
        let s = "hello " + to_string(i);  // Creates temporary string
    }
}
```

Check that `object_count()` stays bounded during execution, not growing linearly with iterations.

### Strategy 7: Sweep Side-Effect Cleanup

When sweeping a `NativeFn` object, the corresponding entries in `native_fns` and `native_fns_meta` HashMaps must also be removed, otherwise they become dangling references. Verify this in tests:

```rust
#[test]
fn test_sweep_cleans_up_native_fn_maps() {
    // Create a native fn, don't root it
    // Run GC
    // Verify native_fns and native_fns_meta no longer contain the ID
}
```

Note: In practice, native functions are typically rooted via globals and won't be collected. But the sweep implementation should handle this case correctly for safety.

---

## Common Pitfalls

### 1. Forgetting a Root Source

**Symptom**: Live objects get collected, causing crashes.

**Solution**: Audit *all* places `TlangValue::Object` references are stored:
- `temp_roots: Vec<TlangValue>` -- intermediate expression values registered by the evaluator (see pitfall #6)
- `globals: HashMap<String, TlangValue>` -- named globals
- `scope_stack.global_memory` -- slot-based globals
- `scope_stack.memory` -- local scope variables (**only live scopes**, see pitfall #5)
- `native_fns` keys -- object IDs of native function objects

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

### 5. Scanning Stale Scope Memory as Roots

**Symptom**: Objects are never collected even though they're unreachable from the program. Memory usage grows despite GC running.

**Cause**: `ScopeStack::pop()` does not truncate the `memory` vector (to preserve values for closures). If root enumeration scans all of `self.memory`, stale values from exited scopes act as phantom roots.

**Solution**: Only scan memory belonging to currently live scopes. See the `live_local_memory_iter()` implementation in Step 1. This is safe because closures store their own copy of captured values in `captured_memory`, which is traced via `referenced_values()`.

### 6. Temporary Values on the Rust Call Stack

**Symptom**: Intermittent crashes, especially during complex expressions or when GC triggers frequently. A collected slab slot gets reused for a different object type (e.g., a list becomes a Closure), causing `todo!("eval_pat: ...")` panics or silent wrong results. Often only manifests with deeply recursive code (e.g., recursive quicksort calling `filter` with closures).

**Cause**: When the evaluator computes an expression like `f(a, b)`, it evaluates `a` (getting a `TlangValue::Object`), stores it in a Rust local (the `SmallVec` inside `eval_exprs!`), then evaluates `b`. If `b` involves a function call, that call enters `eval_stmt` which triggers GC. At this point, `a`'s value is held only on the Rust call stack -- invisible to `gc_roots()` -- and gets swept. The slab slot is reused for a new object, and when the caller later uses `a`, it finds a different object type.

This is especially dangerous with nested calls like `append(append(qs(smaller), [pivot]), qs(larger))` where the inner `append` result is held in the outer call's argument list while `qs(larger)` executes a deep recursive chain.

**Solution — Temporary roots (recommended)**:

Add a `temp_roots: Vec<TlangValue>` field to `InterpreterState` and include it in `gc_roots()`. Use a save/restore pattern to manage lifetime:

```rust
// In InterpreterState:
pub fn temp_roots_mark(&self) -> usize {
    self.temp_roots.len()
}

pub fn push_temp_root(&mut self, value: TlangValue) {
    self.temp_roots.push(value);
}

pub fn temp_roots_restore(&mut self, mark: usize) {
    self.temp_roots.truncate(mark);
}
```

The save/restore pattern handles nesting naturally — each call site saves its own mark, inner calls push/restore within that, and the outer restore cleans up everything:

```rust
// In eval_call, root the callee and each argument:
let mark = self.state.temp_roots_mark();
let callee = eval_value!(self.eval_expr(&call_expr.callee));
self.state.push_temp_root(callee);
let args = eval_exprs!(self, Self::eval_expr, call_expr.arguments);
// ↑ each arg also pushed to temp_roots inside the macro
let result = self.eval_call_object(callee, &args);
self.state.temp_roots_restore(mark);
// ↑ safe: args were pushed to scope memory inside eval_fn_call
```

The key risk points that need rooting:

| Site | What's at risk | Pop point |
|------|---------------|-----------|
| `eval_exprs!` | Earlier args while later args are evaluated | After the function call returns |
| `eval_list_expr` | Earlier list elements while later elements are evaluated | After `new_list` consumes the Vec |
| `eval_call` callee | The callee value while arguments are evaluated | After the function call returns |
| `eval_binary` | Left operand while right operand is evaluated | After the binary op completes |

**Fallback — Call-depth guard (simpler but limits GC)**:

If temporary roots aren't implemented yet, restrict GC to only run at the top-level call frame:

```rust
fn should_collect(&self) -> bool {
    self.call_stack.len() <= 1
}
```

This is correct but means GC never runs during long-running nested computations. Use this as a stopgap until temporary roots are in place.

### 7. Forgetting to Clean Up Side Tables

**Symptom**: `native_fns` or `native_fns_meta` contain dangling object IDs after GC.

**Cause**: When a `NativeFn` object is swept from the slab, the `native_fns` and `native_fns_meta` HashMaps still contain entries keyed by the old object ID.

**Solution**: In the sweep phase, also remove entries from auxiliary maps:
```rust
for id in &to_remove {
    if let Some(TlangObjectKind::NativeFn) = self.remove_object(*id) {
        self.native_fns.remove(id);
        self.native_fns_meta.remove(id);
    }
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

### 3. Heap Compaction

#### The problem

tlang's heap is a `Slab<TlangObjectKind>`, backed by a `Vec`. When objects are swept, their slots go onto an internal free list and are reused by future allocations, but the `Vec` itself never shrinks. After many GC cycles, the backing `Vec` may be much larger than the number of live objects -- the high-water mark is permanent. This wastes memory and hurts cache locality.

Scope memory (`ScopeStack::memory`) has the same issue: it is never truncated when scopes pop (to preserve closure references), so it also only grows. Heap compaction alone does not address scope memory; that requires the closure refactoring described in section 4 below.

#### Algorithm: rebuild-and-remap

Since `TlangObjectId` is just a `usize` index, compaction means assigning new contiguous IDs to all live objects and updating every reference. A practical approach:

```rust
impl InterpreterState {
    /// Compacts the heap by rebuilding the Slab with contiguous IDs.
    /// Call after sweep when fragmentation is high.
    pub fn compact_heap(&mut self) {
        // 1. Build a new Slab and an old→new ID mapping
        let mut new_objects = Slab::with_capacity(self.objects.len());
        let mut id_map: HashMap<TlangObjectId, TlangObjectId> = HashMap::new();

        for (old_id, obj) in self.objects.drain() {
            let new_id = new_objects.insert(obj);
            id_map.insert(old_id, new_id);
        }

        self.objects = new_objects;

        // 2. Update all references using the mapping
        self.remap_all_references(&id_map);
    }

    fn remap_all_references(&mut self, id_map: &HashMap<TlangObjectId, TlangObjectId>) {
        // Helper to remap a single value
        let remap = |v: &mut TlangValue| {
            if let TlangValue::Object(id) = v {
                if let Some(&new_id) = id_map.get(id) {
                    *id = new_id;
                }
            }
        };

        // Named globals
        for value in self.globals.values_mut() {
            remap(value);
        }

        // Scope memory (global + local, including stale slots)
        self.scope_stack.remap_all_values(remap);

        // Inside every heap object
        for (_, obj) in self.objects.iter_mut() {
            obj.remap_references(remap, id_map);
        }

        // native_fns and native_fns_meta keyed by old object IDs
        self.native_fns = self.native_fns.drain()
            .map(|(old_id, f)| (*id_map.get(&old_id).unwrap_or(&old_id), f))
            .collect();
        self.native_fns_meta = self.native_fns_meta.drain()
            .map(|(old_id, m)| (*id_map.get(&old_id).unwrap_or(&old_id), m))
            .collect();
    }
}
```

You'll need a `remap_references` method on `TlangObjectKind`. Note that `captured_cells` stores raw `TlangObjectId`s (not `TlangValue`), so it needs a separate ID-level remapping parameter:

```rust
impl TlangObjectKind {
    /// Update all TlangValue::Object references inside this object.
    pub fn remap_references(
        &mut self,
        mut remap_value: impl FnMut(&mut TlangValue),
        id_map: &HashMap<TlangObjectId, TlangObjectId>,
    ) {
        match self {
            TlangObjectKind::Struct(s) => {
                for v in s.values_mut() { remap_value(v); }
            }
            TlangObjectKind::Enum(e) => {
                for v in e.field_values.iter_mut() { remap_value(v); }
            }
            TlangObjectKind::Slice(s) => {
                let mut of = s.of();
                remap_value(&mut of);
                *s = TlangSlice::new(of, s.start(), s.len());
            }
            TlangObjectKind::Closure(c) => {
                for v in c.captured_memory.iter_mut() { remap_value(v); }
                // captured_cells stores raw TlangObjectId, not TlangValue
                for cell_id in c.captured_cells.values_mut() {
                    if let Some(&new_id) = id_map.get(cell_id) {
                        *cell_id = new_id;
                    }
                }
            }
            TlangObjectKind::Cell(c) => {
                let mut v = c.get();
                remap_value(&mut v);
                c.set(v);
            }
            TlangObjectKind::Fn(_)
            | TlangObjectKind::NativeFn
            | TlangObjectKind::String(_) => {}
        }
    }
}
```

And on `ScopeStack`:

```rust
impl ScopeStack {
    /// Remap all TlangValue::Object references in both global and local memory.
    /// This includes stale slots from exited scopes -- they still hold values
    /// that may be referenced by live closures' captured_memory.
    pub fn remap_all_values(&mut self, mut remap: impl FnMut(&mut TlangValue)) {
        for v in self.global_memory.iter_mut() { remap(v); }
        for v in self.memory.iter_mut() { remap(v); }
    }
}
```

**Note on stale scope memory during compaction**: Even though `live_local_memory_iter()` skips stale slots during root enumeration (correctly -- those values are not roots), compaction must still remap *all* slots in `self.memory`, including stale ones. If a stale slot contains `Object(5)` and we don't remap it, then later code that reads that slot (e.g., a closure's scope-swapping logic) would find a dangling or wrong ID. Either remap all slots, or zero out stale slots during sweep so they can't cause confusion.

#### When to compact

Compaction is expensive (touches every reference), so don't do it on every GC cycle. Run it when fragmentation is high:

```rust
fn should_compact(&self) -> bool {
    // Compact when the Slab's capacity is much larger than live objects.
    // Slab doesn't expose capacity directly, but you can track the
    // high-water mark or use objects.len() vs. last known pre-sweep count.
    let live = self.objects.len();
    let capacity_estimate = self.memory_stats.objects_allocated
        - self.memory_stats.objects_deallocated;
    // If less than 25% of slots are used, compact
    live > 0 && live < capacity_estimate / 4
}
```

Or simply compact after every N GC cycles, or on explicit request.

#### Scope memory compaction

Scope memory (`ScopeStack::memory`) cannot be compacted independently of heap compaction because scopes reference memory by absolute index (`Scope::start`). However, once the closure refactoring (section 4) is complete and `scope.pop()` truncates memory, this problem goes away -- scope memory naturally stays compact.

Until then, stale scope memory is a source of waste that heap compaction does not address. If scope memory becomes a concern before the closure refactoring, you could add a `compact_scope_memory()` that shifts live scope data down and updates `Scope::start` offsets, but this is complex and best deferred.

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
| `state.rs` | `gc_roots()`, `mark_reachable()`, `sweep_unreachable()`, `collect_garbage()`, `next_gc_threshold` field, `gc_stress` flag, `temp_roots` field + `temp_roots_mark()`/`push_temp_root()`/`temp_roots_restore()` |
| `scope.rs` | `global_memory_iter()`, `live_local_memory_iter()` |
| `object.rs` | Already has `referenced_values()` ✓ |
| evaluator | GC trigger at statement boundaries; temp root registration in `eval_exprs!`, `eval_list_expr`, `eval_call`, `eval_binary` |

### Existing Infrastructure

| Feature | Status | Location |
|---------|--------|----------|
| Object tracing | ✅ Done | `TlangObjectKind::referenced_values()` |
| Memory stats | ✅ Done | `MemoryStats`, `memory_stats()` |
| Object removal | ✅ Done | `remove_object(id)` |
| Closure captures | ✅ Done | `TlangClosure::captured_memory` |

### Implementation Checklist

- [ ] Add `gc_roots()` method (including temp_roots and native_fns as roots)
- [ ] Add `live_local_memory_iter()` to ScopeStack (not all of memory!)
- [ ] Add `mark_reachable()` method
- [ ] Add `sweep_unreachable()` method (with side-table cleanup for native_fns)
- [ ] Add `collect_garbage()` method with growth-based threshold
- [ ] Choose GC trigger strategy (statement boundaries vs. inside new_object)
- [ ] Add temporary roots (`temp_roots` + save/restore) to protect Rust-stack intermediates
- [ ] Root values in `eval_exprs!`, `eval_list_expr`, `eval_call` callee, `eval_binary` operands
- [ ] Add `gc_stress` mode for testing
- [ ] Add `assert_roots_valid()` debug assertion
- [ ] Write unit tests for each component
- [ ] Run full test suite with `gc_stress = true`
- [ ] Run full test suite comparing output with/without GC
- [ ] Profile and tune GC threshold

---

## Summary

Implementing a garbage collector for tlang involves:

1. **Understanding the architecture**: GC collects *heap objects* (`TlangObjectKind` entries in the Slab). `TlangValue` is just a handle -- primitives are inline, `Object(id)` is an index.
2. **Finding roots**: Enumerate all directly-accessible values -- temporary roots, globals, live scope memory, native function IDs. Be careful to only scan *live* scope memory, not stale leftovers.
3. **Protecting temporaries**: Intermediate expression values on the Rust call stack (function arguments being evaluated, list elements, binary operands) are invisible to GC. Use a `temp_roots` save/restore mechanism to register them before any code path that could trigger GC.
4. **Tracing**: Follow `Object(id)` references to find all reachable heap objects. Primitives in the worklist can be skipped (they don't lead anywhere).
5. **Sweeping**: Remove unreachable objects and clean up side tables (`native_fns`, `native_fns_meta`).
6. **Triggering**: Choose safe trigger points (statement boundaries recommended) and a growth-based threshold policy. Without temporary roots, restrict GC to the top-level call frame as a stopgap.
7. **Verifying correctness**: Use GC stress mode (collect on every allocation) across the full test suite. This is the most effective way to find premature collection bugs.

The tlang codebase already has the necessary infrastructure for object tracing and deallocation. The remaining work is to implement the mark-and-sweep algorithm, integrate it with the interpreter, protect temporary values with root registration, and verify correctness with stress testing.
