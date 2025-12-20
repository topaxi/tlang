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

**Problem**: When a scope is popped, the underlying memory is not reclaimed because closures may reference those values.

**Impact**: The memory vector grows indefinitely during execution.

**Recommendation**: 
- Closures should capture only the values they reference, not raw memory indices
- Implement proper closure capture analysis during HIR optimization
- Alternative: Use reference counting or GC for closure-captured values specifically

### 3. Closure Design Clones Entire Scope Stack

**Location**: `crates/tlang_runtime/tlang_memory/src/state.rs:415-424`

```rust
pub fn new_closure(&mut self, decl: &hir::FunctionDeclaration) -> TlangValue {
    self.closures
        .entry(decl.hir_id)
        .or_insert_with(|| decl.clone().into());

    self.new_object(TlangObjectKind::Closure(TlangClosure {
        id: decl.hir_id,
        scope_stack: self.scope_stack.scopes.clone(),  // ← clones entire scope stack
    }))
}
```

**Problem**: Each closure copies the entire scope stack metadata. This:
- Prevents GC from collecting parent scope values
- Creates unnecessary memory overhead
- Makes it impossible to track what values a closure actually uses

**Recommendation**:
- Implement upvar analysis in the semantic/HIR phase (partially exists)
- Closures should only capture the specific `TlangValue`s they reference
- Change `TlangClosure` to store captured values directly

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

**Impact**: While currently safe, this pattern:
- Is fragile and could break with future changes
- Makes GC implementation more complex (need to ensure GC doesn't run during native calls)
- The existing comment indicates the author is aware this is a workaround

**Recommendation**:
- Restructure to avoid the need for unsafe code
- Consider storing native functions outside of `InterpreterState`
- Alternative: Use `RefCell` with clear borrowing semantics

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

## Conclusion

The tlang runtime has a clean, simple architecture that is amenable to GC implementation. The main challenges are:

1. **Closure capture design** - needs redesign to capture specific values
2. **Scope memory management** - currently leaks by design
3. **Tracing infrastructure** - needs to be added

The recommended approach is to implement the refactors incrementally, with tests at each step, before adding the actual GC collection logic.
