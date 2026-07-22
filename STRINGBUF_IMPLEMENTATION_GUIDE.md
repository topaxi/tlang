# StringBuf Implementation Guide

This guide describes how to add a new built-in type `StringBuf` to tlang, based on the existing `String` type patterns.

## 1. Register StringBuf as a Built-in Type

### Step 1.1: Add Type Constant
**File**: `crates/tlang_semantics/src/builtin_types.rs`

```rust
pub const STRINGBUF: &str = "StringBuf";

pub const ALL: &[&str] = &[
    BOOL, I8, I16, I32, I64, ISIZE, U8, U16, U32, U64, USIZE, F32, F64, 
    CHAR, STRING, STRINGBUF,  // Add StringBuf
    SLICE,
];
```

### Step 1.2: Register in Symbol Resolution
**File**: `crates/tlang_hir_opt/src/symbol_resolution/identifier_resolver.rs` (line 12-17)

```rust
const PRIM_TY_NAMES: &[&str] = &[
    "bool", "i8", "i16", "i32", "i64", "isize", "u8", "u16", "u32", "u64", 
    "usize", "f32", "f64", "char", "String", "StringBuf",  // Add StringBuf
    "Slice", "unknown",
];
```

---

## 2. Create StringBuf Runtime Value Type

### Option A: Simple Approach (Using Rust String/Vec)
**File**: Create `crates/tlang_runtime/tlang_stdlib/src/stringbuf.rs`

```rust
use tlang_macros::native_fn;
use tlang_memory::{VMState, prelude::*};

#[native_fn]
pub fn new(state: &mut VMState) -> TlangValue {
    // Create a new StringBuf (could use Vec<u8> or custom type)
    state.new_string(String::new())  // Temporary: reuse String
}

#[native_fn]
pub fn push(state: &mut VMState, stringbuf: TlangValue, ch: TlangValue) -> TlangValue {
    if let Some(TlangObjectKind::String(s)) = state.get_object_mut(stringbuf) {
        if let TlangValue::I64(code) = ch {
            if let Some(c) = char::from_u32(code as u32) {
                s.push(c);
            }
        }
    }
    TlangValue::Nil  // or return modified StringBuf
}

#[native_fn]
pub fn pop(state: &mut VMState, stringbuf: TlangValue) -> TlangValue {
    if let Some(TlangObjectKind::String(s)) = state.get_object_mut(stringbuf) {
        if let Some(c) = s.pop() {
            return state.new_string(c.to_string());
        }
    }
    TlangValue::Nil
}

#[native_fn]
pub fn len(state: &mut VMState, stringbuf: TlangValue) -> TlangValue {
    if let Some(obj) = state.get_object(stringbuf) {
        if let Some(s) = obj.as_str() {
            return TlangValue::I64(s.len() as i64);
        }
    }
    TlangValue::I64(0)
}
```

### Option B: Advanced Approach (Custom Struct)
**File**: Create same as Option A but use `define_struct!` macro

```rust
use tlang_macros::define_struct;

define_struct! {
    struct StringBuf { data, capacity }
    
    impl StringBuf {
        fn push(this, ch) {
            // Custom implementation
        }
        
        fn pop(this) {
            // Custom implementation
        }
    }
}
```

---

## 3. Register StringBuf Functions

### Step 3.1: Export from stdlib
**File**: `crates/tlang_runtime/tlang_stdlib/src/lib.rs`

```rust
pub mod stringbuf;  // Add this

pub fn init() {}  // Already has empty init to anchor symbols
```

### Step 3.2: Add Protocol Implementations (Methods)
**File**: `crates/tlang_runtime/tlang_stdlib/src/stringbuf.rs`

```rust
#[protocol_impl("Functor", "StringBuf", method = "map")]
fn stringbuf_functor_map(vm: &mut VMState, this: TlangValue, func: TlangValue) -> TlangValue {
    // Similar to String.map implementation
    let chars: Vec<String> = vm
        .get_object(this)
        .unwrap()
        .as_str()
        .unwrap()
        .chars()
        .map(|c| c.to_string())
        .collect();
    let mut result = String::with_capacity(chars.len());
    for ch in chars {
        let ch_val = vm.new_string(ch);
        let mapped = vm.call(func, &[ch_val]);
        result.push_str(&vm.stringify(mapped));
    }
    vm.new_string(result)
}
```

---

## 4. Add Integration Tests

### Step 4.1: Create Test File
**File**: `tests/stringbuf/basic_operations.tlang`

```tlang
// Test StringBuf creation and operations
fn test_stringbuf_creation() {
    let buf = stringbuf::new();
    log(buf);
}

fn test_stringbuf_push() {
    let buf = stringbuf::new();
    stringbuf::push(buf, 65);  // Push 'A'
    stringbuf::push(buf, 66);  // Push 'B'
    let result = stringbuf::as_string(buf);
    log(result);
}

// Run tests
test_stringbuf_creation();
test_stringbuf_push();
```

### Step 4.2: Create Rust Test
**File**: `crates/tlang_runtime/tlang_stdlib/src/stringbuf.rs` (add test module)

```rust
#[cfg(test)]
mod tests {
    use tlang_memory::VMState;
    use super::*;

    #[test]
    fn test_stringbuf_new() {
        let mut state = VMState::new();
        let buf = new(&mut state);
        assert!(buf.is_object());
    }

    #[test]
    fn test_stringbuf_push() {
        let mut state = VMState::new();
        let buf = new(&mut state);
        push(&mut state, buf, TlangValue::I64(65));
        let len_result = len(&mut state, buf);
        assert_eq!(len_result.as_usize(), 1);
    }
}
```

---

## 5. Compiler Integration Checklist

- [ ] Add type constant to `builtin_types.rs`
- [ ] Add to `PRIM_TY_NAMES` in `identifier_resolver.rs`
- [ ] Create `stringbuf.rs` module in `tlang_stdlib`
- [ ] Export module from `tlang_stdlib/lib.rs`
- [ ] Implement at least 3 native functions:
  - [ ] `new()` - constructor
  - [ ] `push(buf, ch)` - append
  - [ ] `len(buf)` - length
- [ ] Add protocol implementations if needed:
  - [ ] `Functor` (for `.map()`)
  - [ ] `Iterable` (for `.iter()`)
- [ ] Write integration tests in `tests/stringbuf/`
- [ ] Write unit tests in module
- [ ] Test with existing string tests to ensure no regression

---

## 6. Usage Examples (After Implementation)

```tlang
// Basic usage
let buf = stringbuf::new();
stringbuf::push(buf, 72);   // 'H'
stringbuf::push(buf, 105);  // 'i'
let s = stringbuf::as_string(buf);
log(s);  // "Hi"

// With methods (if implemented)
buf |> map(|ch| stringbuf::char_code_at(ch, 0) + 1)

// Pattern matching (if iterator implemented)
let [first, ...rest] = buf;
```

---

## 7. Key Design Decisions

### Immutability vs Mutability
- **Current approach**: StringBuf operations return modified value
- **Alternative**: Use mutable references (requires runtime support)
- **Recommendation**: Start with immutable approach, mirror String pattern

### Value Representation
- **Simplest**: Reuse existing `String` object kind
- **Better**: Create dedicated `StringBuf` object kind
- **Flexible**: Use shape system with custom struct

### Method Call Syntax
- **Option 1**: `stringbuf::push(buf, ch)` (module function)
- **Option 2**: `buf.push(ch)` (method via protocol impl)
- **Both are possible**: Start with module functions, add methods later

---

## 8. Related Files & Patterns

### To Understand String Implementation
- `crates/tlang_runtime/tlang_stdlib/src/string.rs` - String native functions
- `crates/tlang_runtime/tlang_stdlib/src/collections.rs` - Protocol implementations
- `tests/strings/string_processing.tlang` - String usage examples

### Native Function Macro Patterns
- `crates/tlang_macros/src/native_fn.rs` - How `#[native_fn]` works
- `crates/tlang_macros/src/protocol_impl.rs` - How `#[protocol_impl]` works

### Symbol Resolution Flow
- `crates/tlang_hir_opt/src/symbol_resolution/identifier_resolver.rs` - Type lookup
- `crates/tlang_symbols/src/lib.rs` - Symbol table structure

### Runtime Execution
- `crates/tlang_runtime/tlang_interpreter/src/lib.rs` - Field access evaluation
- `crates/tlang_runtime/tlang_memory/src/state.rs` - VMState native fn registration

---

## 9. Testing the Implementation

```bash
# Run all tests
cargo test

# Run only StringBuf tests
cargo test stringbuf

# Run compiler on StringBuf code
cargo run -- compile tests/stringbuf/basic_operations.tlang

# Run with debug output
RUST_LOG=debug cargo run -- compile tests/stringbuf/basic_operations.tlang
```

---

## 10. Performance Considerations

- **Allocations**: Each `push()` creates new value in current design - could optimize with interior mutability
- **String conversion**: Use `as_string()` sparingly if performance critical
- **Pattern matching**: Leverage existing Slice/String pattern mechanisms

---

## Implementation Priority

**Phase 1 (MVP):**
- ✅ Register as built-in type
- ✅ Implement `new()`, `push()`, `len()`, `as_string()`
- ✅ Basic tests

**Phase 2 (Enhancement):**
- ✅ Protocol implementations (Functor, Iterable)
- ✅ Method syntax support
- ✅ More operations (pop, clear, etc.)

**Phase 3 (Optimization):**
- ✅ Optimize allocations
- ✅ Interior mutability patterns
- ✅ Performance benchmarks

---

