# Closure Memory Management TODO

## Current Status

- **JavaScript tests**: Unrelated and can be ignored, we are only testing the interpreter

### ❌ Remaining Issues

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

**Status**: TO BE TESTED

### 2. Global Variable Mutation

```rust
let counter = 0;
let increment = fn () {
    counter = counter + 1;
};
increment(); // Should return 1
```

**Status**: TO BE TESTED

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

**Status**: TO BE TESTED

## Technical Details

### Current Closure Structure

```rust
pub struct TlangClosure {
    pub id: HirId,
    // Scope metadata, stores start vector of each scope which existed during
    // creation of closure.
    pub scope_stack: Vec<Scope>,
}
```

### The Fundamental Problem

- **Global variables**: Stored in `ScopeStack.global_memory`
- **Local variables**: Stored in `ScopeStack.memory`
- **Mutation requirement**: Local variables need shared mutable access like globals

## Proposed Solutions

TODO: Evaluate possible solutions
