# Closure Memory Management TODO

## Current Status

- **JavaScript tests**: Unrelated and can be ignored, we are only testing the interpreter

### Remaining Issues

- **Integration test**: `tests/functions/quicksort/quicksort.tlang`

### Fixed Issues

- **Rust test**: `tlang_interpreter tests::test_simple_closure`
- **Integration test**: `tests/functions/closures/mutation.tlang`

## Root Cause Analysis - COMPLETE

### Variable Assignment Bug in Function Scopes

The issue is NOT with closure memory management, but with **variable assignment of function call results within function scope** when memory truncation is disabled to preserve closures.

**Root cause**: The memory management system has two incompatible requirements:
1. **Closure preservation**: Memory cannot be truncated because closures need access to captured variables
2. **Variable assignment**: The current system assumes memory is truncated and uses sequential assignment

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

**Memory layout issue**: 
- Variables should be stored at `scope.start() + variable_index`
- But `push_value` stores at end of memory vector
- When memory isn't truncated, these positions don't align

**Impact**: This affects any code that assigns function call results to variables within functions, which is exactly what quicksort does with `let pivotIndex = random_int(len(list));`.

**Global scope works fine**: The issue only affects assignments within function scope, not global scope.

## Solution Plan

The fix requires implementing **slot-based variable assignment** for let bindings using the existing HIR resolution system:

1. **Modify `eval_let_stmt`** to use slot-based assignment instead of `push_value`
2. **Get slot information** from pattern `HirId` via HIR optimization metadata
3. **Use `set_local`** with correct variable index instead of appending to memory
4. **Keep `push_value`** for function parameters where sequential assignment is correct

### Technical Implementation

Pattern identifiers have `HirId` that should map to slot information after HIR optimization. Need to:
- Find how to resolve `HirId` → `Slot` mapping in interpreter
- Implement targeted fix in `eval_let_stmt` for identifier patterns
- Use existing `scope_stack.set_local(index, value)` mechanism

### Alternative Approach

If slot resolution proves complex, implement **selective memory preservation**:
- Re-enable truncation by default
- Mark closure memory regions as "preserved" 
- Only truncate non-preserved regions

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
