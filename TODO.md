# Closure Memory Management TODO

## Current Status

- **JavaScript tests**: Unrelated and can be ignored, we are only testing the interpreter

### Remaining Issues

- **Integration test**: `tests/functions/quicksort/quicksort.tlang`

### Fixed Issues

- **Rust test**: `tlang_interpreter tests::test_simple_closure`
- **Integration test**: `tests/functions/closures/mutation.tlang`

## Root Cause Analysis

### 1. Simple Closures

```rust
fn make_adder(a: Int) {
    return fn adder(b: Int) -> Int {
        a + b
    };
}
let add_5 = make_adder(5);
add_5(10); // Should return 15
```

**Status**: FIXED

### 2. Global Variable Mutation

```rust
let counter = 0;
let increment = fn () {
    counter = counter + 1;
};
increment(); // Should return 1
```

**Status**: FIXED

### 3. Local Variable Mutation

```rust
fn counter_builder(init: usize) {
   let count = init;
   return fn () -> usize {
       count = count + 1;
       count
   };
}

let counter = counter_builder(0);
counter() |> log(); // Should log 1
counter() |> log(); // Should log 2
```

**Status**: FIXED

## Technical Details

### Current Memory "Layout"

```rust
#[derive(Debug, Default, Clone, Copy)]
pub struct Scope {
    // Starting position of this scope in the memory vector
    start: usize,
}

#[derive(Debug)]
pub struct ScopeStack {
    pub scopes: Vec<Scope>,
    // Global scope memory - can grow independently without affecting other scopes
    global_memory: Vec<TlangValue>,
    // Central continuous memory for local scopes only (non-global)
    memory: Vec<TlangValue>,
}
```

### Current Closure Structure

```rust
pub struct TlangClosure {
    pub id: HirId,
    // Scope metadata, stores start vector of each scope which existed during
    // creation of closure. When calling a closure, the scope stack is restored
    // and should point to the existing memory locations, given that scopes and
    // memory is not cleaned up.
    pub scope_stack: Vec<Scope>,
}
```
