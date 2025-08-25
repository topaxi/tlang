# Closure Memory Management TODO

## Current Status

- **JavaScript tests**: Unrelated and can be ignored, we are only testing the interpreter

### Remaining Issues

- **Integration test**: `tests/functions/quicksort/quicksort.tlang`

### Fixed Issues

- **Rust test**: `tlang_interpreter tests::test_simple_closure`
- **Integration test**: `tests/functions/closures/mutation.tlang`

## Root Cause Analysis

### Found the Root Cause!

The issue is NOT with closure memory management, but with **variable assignment of function call results within function scope**.

**Reproduction case:**
```tlang
fn random_int(max) { 42 }

fn test() {
  // This works fine:
  random_int(5) |> log();  // → 42
  
  // This is broken:
  let result = random_int(5);  // Stores fn random_int#4(max) instead of 42
  result |> log();
}
```

**Root cause**: When assigning the result of a function call to a variable within a function scope, the function object is being stored instead of the computed return value.

**Impact**: This affects any code that assigns function call results to variables within functions, which is exactly what quicksort does with `let pivotIndex = random_int(len(list));`.

**Global scope works fine**: The issue only affects assignments within function scope, not global scope.

### Fix needed

The variable assignment mechanism within function scopes needs to be fixed to properly store function call return values instead of function objects.

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
