# tlang Built-in Types Exploration - Executive Summary

## Document Map

This exploration includes **3 comprehensive documents**:

1. **BUILTIN_TYPES_EXPLORATION.md** (503 lines)
   - Complete technical reference for the type system
   - How built-in types work at each compiler stage
   - All relevant code locations and patterns
   - Suitable for: deep understanding, reference

2. **STRINGBUF_IMPLEMENTATION_GUIDE.md** (270+ lines)
   - Step-by-step guide to add StringBuf
   - Copy-paste code examples
   - Testing strategies
   - Suitable for: implementation, actionable tasks

3. **EXPLORATION_SUMMARY.md** (this file)
   - Quick reference and overview
   - Key findings and patterns
   - Decision framework

---

## Quick Reference: Built-in Type Flow

```
┌─────────────────────────────────────────────────────────────┐
│ 1. PARSER: User writes code                                 │
│    "String", "i32", "StringBuf" → all as identifiers        │
└────────────────┬────────────────────────────────────────────┘
                 │
┌────────────────▼────────────────────────────────────────────┐
│ 2. AST: Represented as Path nodes                           │
│    TyKind::Path(Path { segments: ["String"] })              │
│    No special node types for built-in types                 │
└────────────────┬────────────────────────────────────────────┘
                 │
┌────────────────▼────────────────────────────────────────────┐
│ 3. SEMANTIC ANALYSIS: Register symbols                      │
│    SemanticAnalysisContext::add_builtin_symbols()           │
│    Create SymbolInfo with builtin=true                      │
└────────────────┬────────────────────────────────────────────┘
                 │
┌────────────────▼────────────────────────────────────────────┐
│ 4. HIR LOWERING: 1:1 mapping (no transformation)            │
│    TyKind::Path → TyKind::Path (same representation)        │
│    Field access: FieldExpression → FieldAccess              │
└────────────────┬────────────────────────────────────────────┘
                 │
┌────────────────▼────────────────────────────────────────────┐
│ 5. SYMBOL RESOLUTION (HIR optimization pass):               │
│    Check if path is in PRIM_TY_NAMES list                   │
│    Mark as primitive type: Res::new_prim_ty()               │
│    → No symbol table lookup needed                          │
└────────────────┬────────────────────────────────────────────┘
                 │
┌────────────────▼────────────────────────────────────────────┐
│ 6. CODE GENERATION: Generate JavaScript                     │
│    Field access → static member access (obj.field)          │
│    No code for type itself (types are compile-time only)    │
└────────────────┬────────────────────────────────────────────┘
                 │
┌────────────────▼────────────────────────────────────────────┐
│ 7. RUNTIME (Interpreter/JS Execution):                      │
│    - Native functions via #[native_fn] macro                │
│    - Protocol implementations via #[protocol_impl]          │
│    - Method resolution in shapes/heap                       │
│    - Actual values are TlangValue enums                     │
└─────────────────────────────────────────────────────────────┘
```

---

## Key Findings

### Finding #1: Built-in Types Are NOT Special at Compile Time
- No unique AST nodes - just `Path` like user types
- Same type annotation structure as structs/enums
- Differentiation happens at symbol resolution phase
- `PRIM_TY_NAMES` is the single source of truth

### Finding #2: String Is a Regular Type (Currently)
- Not treated as primitive like i32/bool
- Has native functions defined via `#[native_fn]` macro
- Has protocol implementations via `#[protocol_impl]` macro
- Pattern matching via Slice type (both Slice and String work with `[x, ...rest]`)
- Operations: `map()`, `iter()` via protocols + `char_code_at()`, `from_char_code()`

### Finding #3: Method Calls Are Field Access
- `obj.method(arg)` → AST FieldExpression
- Lowered to HIR FieldAccess (no transformation)
- At runtime, interpreted as property access
- For built-in types: currently unimplemented in interpreter (line 374: `todo!()`)
- For structs: looks up field index in shape

### Finding #4: Symbol Tables Are Built Early
- Created during semantic analysis phase
- Mapped from NodeId → SymbolTable during AST analysis
- Remapped to HirId → SymbolTable during HIR lowering
- Built-in symbols get `global_slot` for runtime reference
- Symbol tables are hierarchical (parent-child scopes)

### Finding #5: Native Function Registration Uses Inventory Crate
- `#[native_fn]` macro generates `inventory::submit!()` call
- `#[protocol_impl]` macro generates protocol impl submissions
- All submissions collected at startup via `inventory::collect!()`
- No manual registration needed - just define and derive

---

## Critical Code Locations

| Task | File | Lines | Key Code |
|------|------|-------|----------|
| Type constants | builtin_types.rs | 1-36 | `pub const STRING: &str = "String"` |
| Type resolution | identifier_resolver.rs | 10-17 | `const PRIM_TY_NAMES` |
| Symbol info | lib.rs (symbols) | 61-174 | `pub struct SymbolInfo` |
| Symbol creation | analyzer.rs | 49-83 | `add_builtin_symbols()` |
| AST types | node.rs (ast) | 560-610 | `pub enum TyKind` |
| HIR types | hir.rs | 724-736 | `pub enum TyKind` |
| Field access lowering | expr.rs (lowering) | 60-66 | `FieldExpression` → `FieldAccess` |
| String operations | string.rs (stdlib) | full | `#[native_fn]` examples |
| Protocol methods | collections.rs | 73-120 | `#[protocol_impl]` examples |
| Runtime execution | lib.rs (interpreter) | 344-375 | `eval_field_access()` |
| Native fn defs | lib.rs (memory) | 32-150 | `pub struct NativeFnDef` |

---

## Pattern: How to Add a Built-in Type (StringBuf)

### Minimum Changes (3 files)

**1. Register the type** (2 lines added each)
```rust
// builtin_types.rs
pub const STRINGBUF: &str = "StringBuf";
// Add to ALL list

// identifier_resolver.rs
const PRIM_TY_NAMES: &[&str] = &[..., "StringBuf", ...];
```

**2. Create native functions** (1 new file)
```rust
// stringbuf.rs
#[native_fn]
pub fn new(state: &mut VMState) -> TlangValue { ... }

#[native_fn]
pub fn push(state: &mut VMState, buf: TlangValue, ch: TlangValue) -> TlangValue { ... }
```

**3. Export from stdlib** (1 line added)
```rust
// stdlib/lib.rs
pub mod stringbuf;
```

### Extended Changes (for methods like `.push()`)

**Add protocol implementations**
```rust
#[protocol_impl("SomeTrait", "StringBuf", method = "method_name")]
fn stringbuf_method_impl(vm: &mut VMState, this: TlangValue, args: TlangValue) -> TlangValue { ... }
```

**Register as builtin symbol** (optional, if you want `let x: StringBuf = ...` syntax)
```rust
// In semantic analysis setup
ctx.add_builtin_symbols(&[
    ("StringBuf", SymbolType::Struct),
]);
```

---

## Compilation Pipeline Phases & Symbol Handling

```
Phase 1: SEMANTIC ANALYSIS (Reads AST, Creates Symbols)
├─ DeclarationAnalyzer: Collects all declarations
├─ Creates NodeId → SymbolTable mapping  
├─ SymbolInfo records name, type, span
└─ Built-in symbols added with builtin=true flag

Phase 2: HIR LOWERING (Translates AST to HIR)
├─ Maps NodeId → HirId
├─ Translates SymbolTable to HirId-based
├─ Symbol.node_id → Symbol.hir_id
└─ No transformation of type nodes (1:1 mapping)

Phase 3: SYMBOL RESOLUTION (HIR Optimization)
├─ Identifier Resolver walks HIR
├─ For each Path, checks if it's in PRIM_TY_NAMES
├─ If yes: mark Res::Prim (no further lookup)
├─ If no: lookup in symbol table
├─ Sets Res::Slot(Global(n)) for builtins
└─ Sets Res::HirId(id) for user-defined

Phase 4: CODE GENERATION (HIR → JS)
├─ Field access → static member expr
├─ Path → identifier lookup or qualified name
└─ Types don't generate code (compile-time only)

Phase 5: RUNTIME (Execution)
├─ inventory::collect! gathers NativeFnDef
├─ VMState registers all native fns at startup
├─ Heap shapes store method implementations
└─ Field access evaluated at runtime
```

---

## Symbol Resolution Resolution vs. Type Resolution

### Type Resolution (`resolve_ty_path`)
- **Input**: A type annotation like `String` or `MyStruct`
- **Process**: Check PRIM_TY_NAMES first, then symbol table
- **Output**: `Res::Prim` or `Res::Binding(Struct/Enum)`
- **Used for**: Type annotations, type checking

### Path Resolution (`resolve_path`)  
- **Input**: An identifier like `foo` or `String::new`
- **Process**: Look up symbol table hierarchically
- **Output**: `Res::Binding(Function/Variable)` + slot
- **Used for**: Function calls, variable references

### Both use `Res` enum:
```rust
pub enum Res {
    Unresolved,
    Binding(BindingKind),  // Function, Variable, Enum, Struct, etc.
    Prim,                  // Built-in type
    ...
}
```

---

## Implementation Roadmap for StringBuf

### Phase 1: Type Registration (30 min)
- [ ] Add `STRINGBUF` constant
- [ ] Add to `PRIM_TY_NAMES`
- [ ] Compile check: `tlang::StringBuf`

### Phase 2: Basic Operations (1-2 hours)
- [ ] Create `stringbuf.rs` with `#[native_fn]` decorated functions
- [ ] Implement: `new()`, `push()`, `len()`, `as_string()`
- [ ] Export from lib.rs
- [ ] Unit tests

### Phase 3: Protocol Methods (1-2 hours)
- [ ] Add `#[protocol_impl]` for `Functor` and `Iterable`
- [ ] Enable `.map()` and `.iter()` syntax
- [ ] Integration tests

### Phase 4: Optimization (2-4 hours)
- [ ] Consider interior mutability
- [ ] Benchmark allocation patterns
- [ ] Performance tests

---

## Testing Strategy

**Rust Unit Tests** (in stringbuf.rs)
- Test native functions directly
- Mock VMState
- Quick feedback

**Integration Tests** (in tests/stringbuf/*.tlang)
- Test from tlang code
- Verify compilation and execution
- Test error cases

**Regression Tests**
- Run existing string tests to ensure no breakage
- Run full test suite

---

## Key Insights for Implementation

### 1. Symbol Table Hierarchy
- Root table created at semantic analysis start
- New tables created for each scope (blocks, match arms, etc.)
- Tables linked with parent pointers
- Lookup walks up parent chain

### 2. Built-in Symbol Slots
- Not allocated in local scope slots
- Have `global_slot` pointing to heap slot
- Used for fast runtime lookup
- Important for performance

### 3. Macro-Based Function Registration
- No manual registry needed
- `#[native_fn]` and `#[protocol_impl]` handle it
- Uses `inventory` crate for zero-cost abstraction
- All collected at startup

### 4. Field Access Is Currently a Stub
```rust
// In interpreter, line 374:
todo!("eval_field_access: {}.{}", state.stringify(value), ident);
```
This means method calls on non-struct objects aren't implemented yet. This would be a good place to add proper method resolution for built-in types.

### 5. String Isn't Actually Built-in at Runtime
- It's a regular heap object (`TlangObjectKind::String`)
- Operations via native functions and protocol impls
- This is the RIGHT pattern for StringBuf too

---

## Related Issues & Improvements

### Potential Future Enhancements
1. **Implement `eval_field_access` for method calls** - Currently unimplemented
2. **Add interior mutability** - Some objects need mutable operations
3. **Method syntax sugar** - `obj.method()` → `Type::method(obj)`
4. **Generic type parameters** - `Option<T>`, `Result<T, E>` fully generic
5. **Custom object kinds** - Create `StringBuf` as dedicated object type

### Known Patterns to Avoid
- ❌ Creating special-case type nodes - use Path always
- ❌ Manual symbol registration - use add_builtin_symbols()
- ❌ Hard-coding type names - use constants
- ❌ Type transformation during lowering - keep 1:1
- ❌ Manual inventory registration - use macros

---

## Quick Decisions Framework

**Q: How should StringBuf values be represented?**
- Option A: Reuse `TlangObjectKind::String`  
  - ✅ Quickest implementation
  - ❌ Can't differentiate at runtime
- Option B: Create `TlangObjectKind::StringBuf(Vec<u8>)`
  - ✅ Type-safe representation
  - ✅ Can differentiate at runtime
  - ❌ Need heap changes
- **Recommendation**: Start with A, move to B if needed

**Q: Should push() modify in-place or return new value?**
- Option A: Return unit, modify in-place
  - ✅ Matches mutable semantics
  - ❌ Breaks immutability model
- Option B: Return modified value (copy/Rc)
  - ✅ Maintains immutability
  - ❌ Extra allocations
- **Recommendation**: Match current String/collection pattern, use Option B

**Q: Module functions or method syntax?**
- Option A: `stringbuf::push(buf, ch)` (module functions)
  - ✅ Works immediately
  - ✅ No protocol needed
- Option B: `buf.push(ch)` (via protocol)
  - ✅ More ergonomic
  - ❌ Requires protocol impl
- **Recommendation**: Implement A first, add B in Phase 2

---

## File Checklist for Implementation

### To Modify
- [ ] `crates/tlang_semantics/src/builtin_types.rs` - Add constant
- [ ] `crates/tlang_hir_opt/src/symbol_resolution/identifier_resolver.rs` - Add to PRIM_TY_NAMES
- [ ] `crates/tlang_runtime/tlang_stdlib/src/lib.rs` - Export module

### To Create
- [ ] `crates/tlang_runtime/tlang_stdlib/src/stringbuf.rs` - Main implementation
- [ ] `tests/stringbuf/basic_operations.tlang` - Integration tests
- [ ] `tests/stringbuf/protocol_methods.tlang` - Method tests (Phase 2)

### Reference Only (for patterns)
- [ ] `crates/tlang_runtime/tlang_stdlib/src/string.rs` - String implementation
- [ ] `crates/tlang_runtime/tlang_stdlib/src/collections.rs` - Protocol impl examples
- [ ] `crates/tlang_macros/src/native_fn.rs` - Macro patterns

---

## Conclusion

The tlang type system is well-designed for adding new built-in types:

1. **Minimal changes needed** - Just register in two places
2. **Pattern already exists** - String provides the perfect template  
3. **Macro-based registration** - No boilerplate needed
4. **Clear separation of concerns** - Type system vs. runtime operations
5. **Extensible architecture** - Easy to add new operations later

StringBuf is a perfect test case for the pattern and should be straightforward to implement following the guide in `STRINGBUF_IMPLEMENTATION_GUIDE.md`.

---

**Generated**: Comprehensive exploration of tlang built-in type system
**Reference Documents**: 
- BUILTIN_TYPES_EXPLORATION.md (technical deep-dive)
- STRINGBUF_IMPLEMENTATION_GUIDE.md (implementation steps)
- EXPLORATION_SUMMARY.md (this file)

