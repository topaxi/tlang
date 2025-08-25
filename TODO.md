# Closure Memory Management TODO

## Current Status

### ✅ Fixed Issues
- **Rust test**: `tlang_interpreter tests::test_simple_closure` - now passes with `add_5(10) = 15` ✓
- **Simple closure captures**: Non-mutating closures that capture outer scope variables now work

### ❌ Remaining Issues  
- **Integration test**: `tests/functions/closures/mutation.tlang` - still fails with upvar resolution panic for local variable mutation
- **JavaScript tests**: Node.js version mismatch causing stack trace differences

### ⚠️ Current Implementation Limitations
Implemented basic closure memory capture in commit that:
- ✅ Captures local memory state when creating closures (`captured_memory` field)
- ✅ Restores captured memory during closure execution
- ✅ Allows access to upvars from outer scopes
- ❌ Does NOT support mutation of captured local variables (requires shared memory)
- ✅ Supports mutation of global variables (they use separate `global_memory`)

## Root Cause Analysis

### 1. Simple Closures (✅ Fixed)
```rust
fn make_adder(a: Int) {
    return fn adder(b: Int) -> Int { a + b };
}
let add_5 = make_adder(5);
add_5(10); // Now correctly returns 15
```
**Status**: FIXED - upvar `a` is properly captured and accessible

### 2. Global Variable Mutation (✅ Works)  
```rust
let counter = 0;
let increment = fn () { counter = counter + 1; };
increment(); // Works - counter is in global_memory
```
**Status**: WORKS - global variables use shared `global_memory`

### 3. Local Variable Mutation (❌ Still Broken)
```rust
fn counter_builder(init: usize) {
   let count = init;
   return fn () -> usize { count = count + 1; count };
}
```
**Status**: FAILS - `count` is captured as immutable copy, mutations don't persist

## Technical Details

### Current Closure Structure
```rust
pub struct TlangClosure {
    pub id: HirId,
    pub scope_stack: Vec<Scope>,        // Scope metadata
    pub captured_memory: Vec<TlangValue>, // Immutable memory snapshot
}
```

### The Fundamental Problem
- **Global variables**: Stored in `ScopeStack.global_memory` - shared between all executions ✅
- **Local variables**: Stored in `ScopeStack.memory` - captured as immutable snapshot ❌
- **Mutation requirement**: Local variables need shared mutable access like globals

## Proposed Solutions

### Option 1: Reference Counting for Local Variables (Recommended for Full Fix)
Implement `Rc<RefCell<TlangValue>>` for captured local variables to enable shared mutation:

```rust
pub struct TlangClosure {
    pub id: HirId,
    pub scope_stack: Vec<Scope>,
    pub captured_locals: HashMap<(ScopeIndex, SlotIndex), Rc<RefCell<TlangValue>>>,
}
```

**Benefits**: 
- True closure mutation semantics
- Multiple closures can share same variables
- Automatic cleanup when closures are dropped

**Complexity**: Requires significant changes to memory system

### Option 2: Lexical Environment Stack (Alternative)
Create separate environment stack for closure captures:

```rust
pub struct ClosureEnvironment {
    bindings: HashMap<HirId, TlangValue>,
    parent: Option<Rc<ClosureEnvironment>>,
}
```

**Benefits**:
- Clear separation of closure vs normal scopes  
- Proper lexical scoping semantics
- Can implement mutation through environment chains

### Option 3: Current Implementation + TODO (Minimal for Now)
Keep current fix for simple closures, document limitation:

```rust
// TODO: Local variable mutation in closures not yet supported
// Currently only supports immutable captures and global variable mutation
```

## Implementation Recommendation

Given the complexity and @topaxi's preference to avoid big changes for now:

1. **Keep current fix** - it solves simple closure cases
2. **Document limitation** in tests and code
3. **Plan proper solution** for future milestone

The current implementation handles most common closure use cases (accessing outer scope values). The mutation of local variables is a more advanced feature that requires architectural changes.

## Testing Strategy

### ✅ Currently Passing
- Simple closure value capture
- Global variable mutation through closures
- Basic upvar resolution

### ❌ Known Limitations  
- Local variable mutation in closures (documented in TODO)
- Complex nested closure mutations

### 🔄 Node.js Version Fix (Separate Issue)
The JavaScript test failures are unrelated to closure memory - they're due to Node.js version differences in stack trace formatting.