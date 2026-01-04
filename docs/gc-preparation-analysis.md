# Garbage Collector Preparation Analysis

This document analyzes the tlang runtime and identifies refactors and cleanups needed before implementing a garbage collector for the language runtime.

## Current Memory Management Architecture

### Overview

The tlang interpreter uses a relatively simple memory management scheme:

1. **Value Representation**: `TlangValue` is a Copy type that represents either primitives (integers, floats, bools) or references to heap-allocated objects via indices.

2. **Object Storage**: Heap objects are stored in a `Slab<TlangObjectKind>` in `InterpreterState`.

3. **Scope Management**: Variables are stored in contiguous memory vectors (`global_memory` and `memory`) indexed by scope position.

### Key Components

```
TlangValue (Copy, 16 bytes)
├── Primitives (Nil, Bool, I8..I64, U8..U64, F32, F64)
└── Object(TlangObjectId) → index into objects Slab

InterpreterState
├── objects: Slab<TlangObjectKind>  ← heap storage
├── scope_stack: ScopeStack         ← local variables
├── globals: HashMap<String, TlangValue>
├── closures: HashMap<HirId, Rc<FunctionDeclaration>>
├── fn_decls: HashMap<HirId, Rc<FunctionDeclaration>>
├── struct_decls: HashMap<String, Rc<StructDeclaration>>
└── enum_decls: HashMap<String, Rc<EnumDeclaration>>
```

## Identified Issues for GC Implementation

### 1. No Object Deallocation Mechanism

**Location**: `crates/tlang_runtime/tlang_memory/src/state.rs:400-402`

```rust
pub fn new_object(&mut self, kind: TlangObjectKind) -> TlangValue {
    TlangValue::new_object(self.objects.insert(kind))
}
```

**Problem**: Objects are only ever inserted into the Slab, never removed. There is no API to deallocate objects.

**Impact**: Memory usage grows unboundedly. The `Slab` data structure does support removal via `slab.remove(key)`, but this is never called.

**Recommendation**: Before implementing GC, add infrastructure for:
- Tracking which objects are reachable
- Removing unreachable objects from the Slab
- Consider using `Slab::retain()` for batch collection

### 2. Acknowledged Memory Leak in Scope Management

**Location**: `crates/tlang_runtime/tlang_memory/src/scope.rs:60-76`

```rust
pub fn pop(&mut self) {
    if let Some(_scope) = self.scopes.pop() {
        // ...
        // Note: We don't call self.memory.truncate(scope.start()) here to preserve closure memory
        // The downside is that this can lead to memory leaks, but it's necessary for closures
    }
}
```

**Problem**: When a scope is popped, the underlying memory is not reclaimed because closures may reference those values via indices into the memory vector. If we truncated the memory, those indices would become invalid.

**Impact**: The memory vector (`ScopeStack.memory`) grows indefinitely during execution, never shrinking.

**Root Cause Analysis**:
The current design stores local variables in a contiguous `memory: Vec<TlangValue>` where each scope tracks its `start` position. Closures store copies of `Scope` metadata (which is `Copy` - just `start`, `size`, `next_var_index` fields). When a closure accesses an upvar, it uses the stored scope metadata to compute the absolute index into the memory vector.

The problem is that the memory vector must preserve values at those indices for as long as any closure might reference them. Since we don't track which closures are still alive or which specific indices they reference, we cannot safely reclaim any memory.

**Detailed Solution - GC-Managed Captured Values**:

The recommended solution is to move captured values from the scope memory vector into GC-managed objects when creating closures:

1. **Capture Analysis (already partially exists in HIR)**: During compilation, analyze which variables each closure captures. This information is available via `upvars()` in `HirScope`.

2. **Closure Capture Objects**: When creating a closure, allocate a new `TlangObjectKind::CapturedVars` object containing only the specific `TlangValue`s the closure needs:

```rust
pub struct TlangClosure {
    pub id: HirId,
    // Instead of scope_stack metadata, store captured values directly
    pub captures: TlangValue,  // Object ID pointing to CapturedVars
}

pub struct TlangCapturedVars {
    pub values: Vec<TlangValue>,
}
```

3. **Modified Closure Creation**:
```rust
pub fn new_closure(&mut self, decl: &hir::FunctionDeclaration) -> TlangValue {
    // Collect captured values from current scope chain
    let captured_values = decl.upvar_indices()
        .map(|(scope_idx, var_idx)| self.get_upvar(scope_idx, var_idx))
        .collect();
    
    let captures = self.new_object(TlangObjectKind::CapturedVars(
        TlangCapturedVars { values: captured_values }
    ));
    
    self.new_object(TlangObjectKind::Closure(TlangClosure {
        id: decl.hir_id,
        captures,
    }))
}
```

4. **Safe Scope Memory Truncation**: With captured values stored in GC-managed objects, `scope.pop()` can now safely call `self.memory.truncate(scope.start())` since no indices into the truncated region will be accessed.

5. **GC Tracing**: The `TlangCapturedVars` object participates in normal GC tracing via `referenced_values()`, ensuring captured values remain alive as long as the closure is alive.

**Benefits**:
- Scope memory can be reclaimed immediately when scopes pop
- Captured values are GC-managed and collected when closures die
- Clear ownership semantics - captured values are owned by their closure
- Memory usage is proportional to live closures, not historical scope depth

### 3. Closure Scope Metadata Storage

**Location**: `crates/tlang_runtime/tlang_memory/src/state.rs:415-424`

```rust
pub fn new_closure(&mut self, decl: &hir::FunctionDeclaration) -> TlangValue {
    self.closures
        .entry(decl.hir_id)
        .or_insert_with(|| decl.clone().into());

    self.new_object(TlangObjectKind::Closure(TlangClosure {
        id: decl.hir_id,
        scope_stack: self.scope_stack.scopes.clone(),  // ← clones scope metadata
    }))
}
```

**Clarification**: Each closure clones `Vec<Scope>` where `Scope` is a small `Copy` struct containing just three `usize` fields (`start`, `size`, `next_var_index`). This is **not** expensive - it's O(scope_depth) copies of 24 bytes each, typically a few hundred bytes total.

The actual issue is that these scope metadata structs contain indices into the global `memory` vector, which must remain valid. This is addressed in Issue #2 above (scope memory leak).

**Current Behavior**: Acceptable for now - the cloned scope metadata is lightweight and necessary for closure variable resolution.

**Future Optimization**: When implementing GC-managed captured values (see Issue #2), closures won't need scope metadata at all, reducing `TlangClosure` to just `id` and `captures` fields.

### 4. Unsafe Code in Native Function Calls

**Location**: `crates/tlang_runtime/tlang_memory/src/state.rs:454-466`

```rust
pub fn call_native_fn(
    &mut self,
    fn_id: TlangObjectId,
    args: &[TlangValue],
) -> Option<NativeFnReturn> {
    let native_fn_ptr = self.native_fns.get_mut(&fn_id)? as *mut TlangNativeFn;

    // SAFETY: This works around borrow checker limitations - see notes below
    unsafe {
        let native_fn = &mut *native_fn_ptr;
        Some(native_fn(self, args))
    }
}
```

**Problem**: Uses raw pointer to work around borrow checker limitations. The native function has mutable access to `InterpreterState` while the function object is borrowed.

**Impact for GC**: For a single-threaded GC implementation, this pattern is acceptable. The key concern (GC running during native calls) is not an issue because:
- GC will only be triggered at safe points (e.g., allocation)
- Native functions complete synchronously
- No concurrent access to `InterpreterState`

**Priority**: Low - can be ignored for initial single-threaded GC implementation.

**Future Consideration**: If implementing concurrent/parallel GC or multi-threaded execution, this pattern would need restructuring.

### 5. Extensive Use of Rc for Declarations

**Location**: Multiple files use `Rc<hir::FunctionDeclaration>`, `Rc<hir::StructDeclaration>`, `Rc<hir::EnumDeclaration>`

**Problem**: Reference counting can create cycles if:
- A closure references a function that references the closure
- Functions are stored both in the global table and in closures

**Impact**: While the current structure may not create cycles, a GC implementation needs to handle this carefully.

**Recommendation**:
- Declarations are mostly immutable and could be arena-allocated
- Consider separating "static" declarations from "dynamic" runtime values
- For GC: treat declarations as GC roots rather than GC-managed objects

### 6. No Reachability Tracing Infrastructure

**Problem**: There is no mechanism to determine which objects are reachable from:
- The scope stack
- Global variables  
- The call stack
- Native function arguments

**Recommendation**: Implement a `Trace` trait:
```rust
pub trait Trace {
    fn trace(&self, tracer: &mut dyn Tracer);
}

pub trait Tracer {
    fn mark_value(&mut self, value: TlangValue);
}
```

### 7. Object Kind Lacks Unified Reference Access

**Location**: `crates/tlang_runtime/tlang_memory/src/value/object.rs:141-150`

```rust
pub enum TlangObjectKind {
    Fn(HirId),
    NativeFn,
    String(String),
    Struct(TlangStruct),
    Enum(TlangEnum),
    Slice(TlangSlice),
    Closure(TlangClosure),
}
```

**Problem**: Each variant has different ways of referencing other objects:
- `TlangStruct` has `Vec<TlangValue>` (can reference objects)
- `TlangEnum` has `field_values: Vec<TlangValue>` (can reference objects)
- `TlangSlice` has `of: TlangValue` (references one object)
- `TlangClosure` has `scope_stack: Vec<Scope>` (indirect references)

**Recommendation**: Add a method to enumerate all referenced values:
```rust
impl TlangObjectKind {
    pub fn referenced_values(&self) -> impl Iterator<Item = TlangValue> + '_ {
        // ...
    }
}
```

### 8. Missing Memory Statistics and Debugging

**Problem**: No way to monitor memory usage, object counts, or allocation patterns.

**Recommendation**: Add basic statistics:
```rust
pub struct MemoryStats {
    pub objects_allocated: usize,
    pub objects_deallocated: usize,
    pub memory_bytes: usize,
    pub gc_collections: usize,
}
```

## Recommended Refactor Order

1. **Add object tracing infrastructure** (Issue #7)
   - Add `referenced_values()` method to `TlangObjectKind`
   - No behavioral changes, just new API

2. **Add memory statistics** (Issue #8)
   - Add counters for allocations
   - Useful for testing GC effectiveness

3. **Refactor closure capture** (Issue #3)
   - Most impactful change
   - Enables both scope memory cleanup (Issue #2) and proper GC

4. **Remove unsafe code** (Issue #4)
   - Restructure native function storage
   - Important for GC safety

5. **Add object deallocation** (Issue #1)
   - Once tracing is in place, add `deallocate_object()` method
   - Add `gc_collect()` method that uses tracing + deallocation

6. **Review Rc usage** (Issue #5)
   - Lower priority, current design may be acceptable
   - Consider if declarations should be GC roots

## GC Algorithm Considerations

Given the current architecture, a **mark-and-sweep** GC would be most appropriate:

1. **Roots**: globals, scope stack values, call stack, native function arguments
2. **Mark**: Traverse from roots, marking reachable objects
3. **Sweep**: Remove unmarked objects from the Slab

Alternative: **Reference counting with cycle detection**
- More incremental, less pause time
- More complex to implement correctly
- Cycle detection adds overhead

## Test Considerations

The file `tests/known_failures/large_data_processing.tlang` appears to be designed for testing memory-intensive operations. After GC implementation:

1. This test should complete without running out of memory
2. Add explicit tests for:
   - Object collection after scope exit
   - Closure-captured value lifetime
   - Circular reference handling (if applicable)
   - Large allocation/deallocation cycles

## Implementation Attempt Notes

An attempt was made to implement the GC-managed captured values approach. Key findings:

### Complexity of Closure Refactoring

The closure capture refactor is more complex than initially assessed due to:

1. **Multiple variable access patterns**:
   - `Local` slots for current scope variables
   - `Upvar` slots for parent scope variables
   - Global scope variables accessed via both `Local` and `Upvar` depending on context

2. **Global variable mutations**: When a closure modifies a global variable, the change must be visible outside the closure. This requires:
   - Not capturing globals by value (or mutations are lost)
   - Detecting which upvars refer to global scope at runtime
   - Using live `global_memory` access for globals while using captured values for non-globals

3. **Scope stack restoration**: Closures need both:
   - Captured values for upvar accesses (GC-managed)
   - Scope metadata for local slot accesses (like globals accessed via `Local(0)`)

4. **Function resolution**: When closures call other functions that recursively call themselves, the function resolution must work correctly regardless of scope stack state.

### Recommended Approach

Given the complexity, the recommended approach is:

1. **Phase 1**: Add GC infrastructure without changing closure behavior ✅ **COMPLETED**
   - Object tracing (`referenced_values()`) ✅
   - Memory statistics (`MemoryStats`) ✅
   - Object deallocation capability (`remove_object()`) ✅
   - Comprehensive analysis documentation ✅
   - Closure captured memory storage (`captured_memory` field) ✅
   - Criterion benchmarks for closure performance (optional, not yet implemented)

2. **Phase 2**: Closure capture refactoring
   - Should be a focused, dedicated effort
   - Requires extensive testing with all closure patterns
   - Should include criterion benchmarks before and after

3. **Phase 3**: Full GC implementation
   - Mark-and-sweep collection
   - Safe point identification
   - Collection triggering policy

## Current Implementation Status

### Completed Items

The following GC preparation infrastructure has been implemented:

1. **`TlangObjectKind::referenced_values()`** - Returns an iterator over all `TlangValue` references contained in an object, enabling GC tracing. All object types now properly report their references:
   - `Struct` → yields all field values
   - `Enum` → yields all field values
   - `Slice` → yields the underlying array reference
   - `Closure` → yields all captured memory values
   - `Cell` → yields the wrapped value
   - `Fn`, `NativeFn`, `String` → no references (empty iterator)

2. **`MemoryStats`** - Tracks allocation statistics:
   - `objects_allocated: usize` - Total objects created
   - `objects_deallocated: usize` - Total objects removed
   - Accessible via `InterpreterState::memory_stats()`

3. **`InterpreterState::remove_object()`** - Enables object deallocation:
   - Removes object from the Slab
   - Updates `objects_deallocated` counter
   - Returns the removed object kind

4. **`TlangClosure::captured_memory`** - Closures now capture their memory context:
   - Stores all values from global and local memory at closure creation time
   - `global_memory_len` field tracks the split point
   - Currently stored but not used during execution (execution still uses scope-swapping)
   - Enables GC tracing via `referenced_values()`

5. **`TlangClosure::captured_cells`** - Prepared for mutable capture support:
   - Maps (scope_index, var_index) to Cell object IDs
   - Enables closures to share mutable state with parent scopes via cells
   - Not currently populated during closure creation (prepared for future use)

6. **`TlangObjectKind::Cell`** - New object type for mutable captures:
   - Wraps a `TlangValue` in a mutable cell
   - Enables shared mutable state between closures and parent scopes
   - Includes `get()` and `set()` methods for value access/mutation
   - Helper methods: `new_cell()`, `get_cell_value()`, `set_cell_value()` on `InterpreterState`

7. **`ScopeStack::capture_all_memory()`** - Helper to capture memory state:
   - Returns combined global + local memory as a single vector
   - `global_memory_len()` returns the current global memory size

### Remaining Work Before GC Implementation

The following items remain before a full GC can be implemented:

#### High Priority (Required for GC)

1. **Root Enumeration** - Need a method to enumerate all GC roots:
   - Global variables (`globals` HashMap)
   - Scope stack values (`global_memory` and `memory` vectors)
   - Call stack frames (if any values stored there)
   
   ```rust
   impl InterpreterState {
       pub fn gc_roots(&self) -> impl Iterator<Item = TlangValue> + '_ {
           // Yield all values that are roots
       }
   }
   ```

2. **Mark Phase Implementation** - Traverse from roots, marking reachable objects:
   ```rust
   pub fn mark_reachable(&mut self) -> HashSet<TlangObjectId> {
       let mut marked = HashSet::new();
       let mut worklist: Vec<TlangValue> = self.gc_roots().collect();
       
       while let Some(value) = worklist.pop() {
           if let Some(id) = value.get_object_id() {
               if marked.insert(id) {
                   if let Some(obj) = self.get_object_by_id(id) {
                       worklist.extend(obj.referenced_values());
                   }
               }
           }
       }
       marked
   }
   ```

3. **Sweep Phase Implementation** - Remove unreachable objects:
   ```rust
   pub fn sweep_unreachable(&mut self, marked: &HashSet<TlangObjectId>) {
       let all_ids: Vec<_> = self.objects.iter().map(|(id, _)| id).collect();
       for id in all_ids {
           if !marked.contains(&id) {
               self.remove_object(id);
           }
       }
   }
   ```

4. **GC Trigger Policy** - Decide when to run collection:
   - After N allocations
   - When memory usage exceeds threshold
   - On explicit request

#### Medium Priority (Improve GC Effectiveness)

5. **Scope Memory Truncation** - Currently disabled to preserve closure references:
   - Once closures use `captured_memory` for execution, scope memory can be truncated
   - Requires Phase 2 closure refactoring

6. **Selective Capture** - Currently captures all memory:
   - Optimize to only capture values the closure actually uses
   - Requires upvar analysis from HIR

#### Lower Priority (Optimizations)

7. **Criterion Benchmarks** - Performance testing:
   - Closure creation overhead
   - Memory capture overhead
   - GC pause times

8. **Generational GC** - Future optimization:
   - Track object age
   - Collect young objects more frequently

## Conclusion

The tlang runtime has a clean, simple architecture that is amenable to GC implementation. The main challenges are:

1. **Closure capture design** - needs redesign to capture specific values
2. **Scope memory management** - currently leaks by design
3. **Tracing infrastructure** - ✅ Now implemented

**Phase 1 is complete.** The remaining work for a basic mark-and-sweep GC is:
1. Implement root enumeration
2. Implement mark phase
3. Implement sweep phase  
4. Add GC trigger policy

The recommended approach is to implement the refactors incrementally, with tests at each step, before adding the actual GC collection logic.
