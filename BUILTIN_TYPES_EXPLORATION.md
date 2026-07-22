# tlang Built-in Types & AST System - Comprehensive Exploration

## Overview
The tlang compiler uses a multi-stage pipeline for handling built-in types: AST → HIR (High-level IR) → Symbol Resolution → Code Generation. Built-in types like `String` are treated as regular types but with special handling at runtime through native function definitions.

---

## 1. HOW BUILT-IN TYPES ARE REFERENCED IN THE AST

### Type Definition Structure
**Location**: `crates/tlang_ast/src/node.rs` (lines 560-610)

```rust
// AST Type Definition
#[derive(Debug, Clone)]
pub struct Ty {
    pub id: NodeId,
    pub kind: TyKind,
    pub parameters: Vec<Ty>,  // For generic types
    pub span: Span,
}

#[derive(Debug, Default, Clone)]
pub enum TyKind {
    #[default]
    Unknown,
    Path(Path),              // e.g., "String", "i32", "MyStruct"
    Union(Vec<Path>),        // Type unions
}
```

### How Built-in Types Look in AST
Built-in types are **referenced as `Path` nodes** - no special node type. Examples:
- `String` → `TyKind::Path(Path { segments: [Segment { ident: "String" }] })`
- `i32` → `TyKind::Path(Path { segments: [Segment { ident: "i32" }] })`

### Built-in Type Constants
**Location**: `crates/tlang_semantics/src/builtin_types.rs`

```rust
pub const BOOL: &str = "bool";
pub const I8: &str = "i8";
pub const I16: &str = "i16";
pub const I32: &str = "i32";
pub const I64: &str = "i64";
pub const ISIZE: &str = "isize";
pub const U8: &str = "u8";
pub const U16: &str = "u16";
pub const U32: &str = "u32";
pub const U64: &str = "u64";
pub const USIZE: &str = "usize";
pub const F32: &str = "f32";
pub const F64: &str = "f64";
pub const CHAR: &str = "char";
pub const STRING: &str = "String";      // Heap-allocated string
pub const SLICE: &str = "Slice";        // Sliceable sequence type
```

**All built-in types**: `&[BOOL, I8, I16, I32, I64, ISIZE, U8, U16, U32, U64, USIZE, F32, F64, CHAR, STRING, SLICE]`

---

## 2. SYMBOL RESOLUTION SYSTEM FOR BUILT-IN TYPES

### Identifier Resolver
**Location**: `crates/tlang_hir_opt/src/symbol_resolution/identifier_resolver.rs` (lines 1-78)

```rust
// Primitive type names that are builtin to the language
const PRIM_TY_NAMES: &[&str] = &[
    "bool", "i8", "i16", "i32", "i64", "isize", "u8", "u16", "u32", "u64", 
    "usize", "f32", "f64", "char", "String", "Slice", "unknown",
];

fn resolve_ty_path(&mut self, path: &mut hir::Path, ctx: &mut HirOptContext) {
    let name = path.to_string();
    
    if PRIM_TY_NAMES.contains(&name.as_str()) {
        debug!("Type path '{}' resolved as primitive type", name);
        path.res = hir::Res::new_prim_ty();  // Mark as primitive type
        return;
    }
    
    // Otherwise, look up in symbol table for user-defined types
    // ...
}
```

### How Built-in Types Are Resolved
1. **Check if name is in `PRIM_TY_NAMES`**
2. **If yes**: Mark with `Res::new_prim_ty()` (no symbol table lookup needed)
3. **If no**: Look up in symbol table for enums/structs

### Symbol Table Structure
**Location**: `crates/tlang_symbols/src/lib.rs` (lines 61-174)

```rust
pub struct SymbolInfo {
    pub id: SymbolId,
    pub name: Box<str>,
    pub symbol_type: SymbolType,  // Function, Enum, Struct, Variable, etc.
    pub defined_at: Span,
    pub scope_start: u32,
    pub node_id: Option<NodeId>,
    pub hir_id: Option<HirId>,
    pub builtin: bool,            // Flag for built-in symbols
    pub global_slot: Option<usize>,  // Global slot for built-ins
    // ...
}

// Creating built-in symbols:
pub fn new_builtin(
    id: SymbolId,
    name: &str,
    symbol_type: SymbolType,
    global_slot: Option<usize>,
) -> Self {
    let mut symbol_info = SymbolInfo::new(id, name, symbol_type, Span::default(), 0);
    symbol_info.builtin = true;
    symbol_info.global_slot = global_slot;
    symbol_info
}
```

### Builtin Symbol Slots
- Built-in symbols get `global_slot` indices for runtime access
- Used to store references to native functions and types
- Symbol tables don't allocate local slots for builtins

---

## 3. HIR LOWERING FOR BUILT-IN TYPES

### AST to HIR Type Lowering
**Location**: `crates/tlang_ast_lowering/src/expr.rs` (lines 50-113)

```rust
// Simple 1:1 mapping from AST TyKind to HIR TyKind
ast::node::TyKind::Path(path) → hir::TyKind::Path(path)
ast::node::TyKind::Union(paths) → hir::TyKind::Union(paths)
```

### HIR Type Definition
**Location**: `crates/tlang_hir/src/hir.rs` (lines 724-736)

```rust
#[derive(Debug, Default, Clone)]
pub struct Ty {
    pub kind: TyKind,
    pub span: Span,
}

#[derive(Debug, Default, Clone)]
pub enum TyKind {
    #[default]
    Unknown,
    Path(Path),          // Built-in types use Path
    Union(Vec<Path>),    // Union types
}
```

### Field Access/Method Call Lowering
**Location**: `crates/tlang_ast_lowering/src/expr.rs` (lines 60-66)

```rust
// AST: FieldExpression
ast::node::ExprKind::FieldExpression(box ast::node::FieldAccessExpression {
    base,
    field,
}) => {
    let expr = self.lower_expr(base);
    hir::ExprKind::FieldAccess(Box::new(expr), field.clone())
}
```

**Pattern**: Field access is kept as-is during lowering (no expansion at this stage)

---

## 4. HOW STRING TYPE IS CURRENTLY HANDLED

### String Is NOT a Special Case
Unlike some languages, `String` in tlang:
- ✅ Uses same `Path`-based type representation as other types
- ✅ Resolved through the same `PRIM_TY_NAMES` list
- ✅ No special AST node types

### String Literal Handling
**Location**: `crates/tlang_ast_lowering/src/expr.rs` (line 84)

```rust
ast::node::ExprKind::Literal(box literal) => {
    hir::ExprKind::Literal(Box::new(literal.clone()))
}
```

String literals are preserved as-is through lowering.

### String Value Representation at Runtime
**Location**: `crates/tlang_runtime/tlang_memory/src/value/object.rs` (implied)

```rust
// String is stored as a heap object
TlangObjectKind::String(String)  // Rust String type
```

### String Operations via Native Functions
**Location**: `crates/tlang_runtime/tlang_stdlib/src/string.rs`

```rust
#[native_fn]  // Macro generates wrapper + inventory registration
pub fn from_char_code(state: &mut VMState, code: TlangValue) -> TlangValue {
    let char_code = code.as_usize() as u32;
    if let Some(ch) = std::char::from_u32(char_code) {
        state.new_string(ch.to_string())
    } else {
        state.panic(format!("Invalid char code: {}", char_code))
    }
}

#[native_fn]
pub fn char_code_at(state: &mut VMState, string: TlangValue, index: TlangValue) -> TlangValue {
    if let Some(ch) = state
        .get_object(string)
        .and_then(|o| o.as_str())
        .and_then(|string| string.chars().nth(index.as_usize()))
    {
        TlangValue::U32(ch as u64)
    } else {
        state.panic(format!("Index out of bounds: {}", index))
    }
}
```

### String Pattern Matching
Strings work with list patterns (`[c, ...rest]`) via the `Slice` type:
- `[c, ...rest]` matches both `Slice` and `String`
- Characters are extracted as single-char strings

---

## 5. TYPE INFERENCE & METHOD CALLS ON KNOWN TYPES

### Type Inference Passes
**Location**: `crates/tlang_semantics/src/passes/fn_param_type_inference.rs`

The semantic analysis pipeline:
1. **DeclarationAnalyzer**: Collects declarations
2. **FnParamTypeInference**: Infers parameter types from call sites
3. **StringLiteralValidator**: Validates string literals
4. **VariableUsageValidator**: Checks variable usage

### Field Access Expression (Method Calls)
**Location**: `crates/tlang_codegen_js/src/expr_generator.rs` (lines 179-186)

```rust
fn generate_field_access_expression(
    &mut self,
    base: &hir::Expr,
    field: &Ident,
) -> Expression<'a> {
    let obj = self.generate_expr(base);
    self.static_member_expr(obj, field.as_str())
}
```

**Pattern**: Method calls are compiled to static member access (`obj.field`)

### Method Resolution at Runtime
**Location**: `crates/tlang_runtime/tlang_interpreter/src/lib.rs` (line 344-375)

```rust
fn eval_field_access(&self, state: &mut VMState, lhs: &hir::Expr, ident: &Ident) -> EvalResult {
    let value = eval_value!(state, self.eval_expr(state, lhs));

    // For structs: look up field by name
    if let Some(TlangObjectKind::Struct(obj)) = state.get_object(value) {
        if let Some(index) = state.get_struct_field_index(obj.shape(), ident.as_str()) {
            return EvalResult::Value(obj[index]);
        }
        // Error: field not found
    }

    // For other types: currently unimplemented
    // This is where built-in type methods would be called
    todo!("eval_field_access: {}.{}", state.stringify(value), ident);
}
```

---

## 6. EXISTING STRING TESTS

**Location**: `tests/strings/` directory

Test files show current string capabilities:
1. **unicode_strings.tlang** - Unicode support
2. **escape_sequences.tlang** - String escapes
3. **string_processing.tlang** - Pattern matching, recursion
4. **is_palindrome.tlang** - Example functionality
5. **regex_literals.tlang** - Regex support
6. **invalid_escape_sequences.tlang** - Error handling

### Example: String Processing
```tlang
fn reverse_string(str) { reverse_string(str, "") }
fn reverse_string("", acc) { acc }
fn reverse_string([c, ...rest], acc) {
    rec reverse_string(rest, c + acc)
}

let result = "hello" |> reverse_string();  // "olleh"
```

---

## 7. CURRENT STRING OPERATIONS & IMPLEMENTATION

### Native Functions in `tlang_stdlib::string`
**Location**: `crates/tlang_runtime/tlang_stdlib/src/string.rs`

```rust
#[native_fn]
pub fn from_char_code(state: &mut VMState, code: TlangValue) -> TlangValue
    // Module: "string"
    // Binding: "string::from_char_code" or "from_char_code"
    // Arity: 1

#[native_fn]
pub fn char_code_at(state: &mut VMState, string: TlangValue, index: TlangValue) -> TlangValue
    // Module: "string"
    // Binding: "string::char_code_at"
    // Arity: 2
```

### Protocol Implementations for String
**Location**: `crates/tlang_runtime/tlang_stdlib/src/collections.rs` (lines 84-101)

```rust
#[protocol_impl("Functor", "String", method = "map")]
fn string_functor_map(vm: &mut VMState, this: TlangValue, func: TlangValue) -> TlangValue {
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

#[protocol_impl("Iterable", "String", method = "iter")]
fn string_iter(vm: &mut VMState, this: TlangValue) -> TlangValue {
    // Creates iterator over string characters
}
```

### String Operations Summary
| Operation | Type | Implementation |
|-----------|------|-----------------|
| `from_char_code(code)` | Native Fn | Module function |
| `char_code_at(str, idx)` | Native Fn | Module function |
| `String.map(fn)` | Protocol Method | Functor protocol |
| `String.iter()` | Protocol Method | Iterable protocol |
| Pattern match `[c, ...rest]` | Language Feature | Built-in pattern |
| Concatenation `a + b` | Operator | Binary op |

### Native Function Registration
**Location**: `crates/tlang_runtime/tlang_memory/src/lib.rs` (lines 32-84)

```rust
pub struct NativeFnDef {
    name: &'static str,
    binding_name: &'static str,
    arity: usize,
    function: fn(&mut VMState, &[TlangValue]) -> NativeFnReturn,
    module_path: &'static str,
}

inventory::collect!(NativeFnDef);  // Collects all #[native_fn] at startup
```

Functions decorated with `#[native_fn]` are:
1. Wrapped with argument extraction
2. Added to `inventory::submit!()` macro
3. Collected at startup and registered with `VMState`

---

## KEY PATTERNS FOR IMPLEMENTING StringBuf

### 1. **Type Definition**
Add to `PRIM_TY_NAMES` in `identifier_resolver.rs`:
```rust
const PRIM_TY_NAMES: &[&str] = &[
    // ... existing types ...
    "StringBuf",  // Add this
];
```

Add constant to `builtin_types.rs`:
```rust
pub const STRINGBUF: &str = "StringBuf";
```

### 2. **Native Function Implementation Pattern**
```rust
#[native_fn]
pub fn stringbuf_push(state: &mut VMState, buf: TlangValue, ch: TlangValue) -> TlangValue {
    // Implementation
}

#[protocol_impl("SomeTrait", "StringBuf", method = "some_method")]
fn stringbuf_method(vm: &mut VMState, this: TlangValue, arg: TlangValue) -> TlangValue {
    // Implementation
}
```

### 3. **Runtime Value Representation**
Create a heap object kind or struct:
```rust
// In tlang_memory value/object.rs
TlangObjectKind::StringBuf(Vec<u8>)  // or custom struct

// Or use shape system:
define_struct! {
    struct StringBuf { data, capacity }
}
```

### 4. **Method Call Flow**
1. **Parser**: `buf.push('x')` → AST FieldExpression
2. **HIR Lowering**: FieldExpression → HIR FieldAccess
3. **Symbol Resolution**: Resolve "push" method
4. **Code Gen**: Generate `obj.push(arg)` access
5. **Runtime Execution**: Look up method in shape, call native function

### 5. **Builtin Symbol Registration**
```rust
// In SemanticAnalysisContext setup
ctx.add_builtin_symbols(&[
    ("StringBuf", SymbolType::Struct),
]);
```

---

## SYMBOL TABLE STRUCTURE IN COMPILER PIPELINE

```
AST Phase:
  ├─ DeclarationAnalyzer: Collects type/function declarations
  └─ Creates NodeId → SymbolTable mapping

HIR Lowering Phase:
  ├─ Maps NodeId to HirId
  └─ Translates SymbolTable to HirId-based lookup

Symbol Resolution Phase:
  ├─ Identifier Resolver: Resolves paths to symbols
  ├─ Checks PRIM_TY_NAMES for built-in types
  ├─ Looks up symbol table for user-defined types
  └─ Sets Res::Slot(Global(n)) for builtin functions

Runtime Phase:
  ├─ inventory::collect! gathers all NativeFnDef
  ├─ VMState registers native functions at startup
  └─ Heap shapes store method implementations
```

---

## RELEVANT FILE LOCATIONS

### Core Type System
- **AST Types**: `crates/tlang_ast/src/node.rs` (lines 560-610)
- **Built-in Type Constants**: `crates/tlang_semantics/src/builtin_types.rs`
- **HIR Types**: `crates/tlang_hir/src/hir.rs` (lines 724-736)

### Symbol Management
- **SymbolInfo/SymbolTable**: `crates/tlang_symbols/src/lib.rs` (lines 61-174)
- **Symbol Resolution**: `crates/tlang_hir_opt/src/symbol_resolution/identifier_resolver.rs` (lines 10-78)

### Type Lowering
- **AST to HIR**: `crates/tlang_ast_lowering/src/expr.rs` (lines 50-113)

### Runtime/Native Functions
- **Native Function Defs**: `crates/tlang_runtime/tlang_memory/src/lib.rs` (lines 32-150)
- **String Operations**: `crates/tlang_runtime/tlang_stdlib/src/string.rs`
- **Protocol Implementations**: `crates/tlang_runtime/tlang_stdlib/src/collections.rs`
- **Native Macros**: `crates/tlang_macros/src/native_fn.rs`, `protocol_impl.rs`
- **Interpreter Execution**: `crates/tlang_runtime/tlang_interpreter/src/lib.rs` (lines 344-375)

### Code Generation
- **Field Access Gen**: `crates/tlang_codegen_js/src/expr_generator.rs` (lines 179-186)

