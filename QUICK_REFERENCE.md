# Tlang Codebase Quick Reference

## File Locations & Purposes

```
crates/
├── tlang_hir/src/hir.rs              ← All HIR node definitions (24 ExprKind variants)
├── tlang_hir_opt/src/
│   ├── hir_opt.rs                    ← HirPass trait & optimizer framework
│   ├── constant_folding/mod.rs       ← ConstantFolding (2 sub-passes)
│   ├── symbol_resolution/mod.rs      ← Symbol resolution pass
│   └── slot_allocation/mod.rs        ← Slot allocation + scope metadata
├── tlang_codegen_js/src/
│   ├── generator.rs                  ← CodegenJS + BlockContext + completion vars
│   ├── expr_generator.rs             ← Expression code generation (ternary opt)
│   ├── pattern_match_generator.rs    ← Match expression 4-stage generation
│   └── scope.rs                      ← Variable scope + temp var naming
└── tlang_ast_lowering/src/
    ├── lib.rs                        ← lower_to_hir() entry point
    ├── expr.rs                       ← lower_expr() for all expression kinds
    ├── loop.rs                       ← For-loop desugaring
    └── tests/common/mod.rs           ← hir_from_str & hir_from_str_analyzed
```

## Core Data Structures at a Glance

| Struct | File | Purpose |
|--------|------|---------|
| `Module` | hir.rs:385 | Root HIR node containing a Block |
| `Block` | hir.rs:427 | Scoped sequence of Stmts + optional completion Expr |
| `Stmt` | hir.rs:489 | Statement with hir_id, kind, span |
| `Expr` | hir.rs:646 | Expression with hir_id, kind (24 variants), span |
| `StmtKind` | hir.rs:513 | Enum: Expr, Let, FnDecl, Return, Enum/Struct/Protocol decls |
| `ExprKind` | hir.rs:679 | Enum: 24 variants - Block, Loop, If/Match, Call, Literal, etc |
| `PatKind` | hir.rs:562 | Enum: Wildcard, Identifier, Literal, List, Rest, Enum |
| `MatchArm` | hir.rs:573 | Pattern + optional guard + Block |
| `Path` | hir.rs:296 | Segments + Res (resolution) + span |
| `Res` | hir.rs:143 | hir_id + binding_kind + slot (for resolution tracking) |
| `BindingKind` | hir.rs:58 | 11 kinds: Local, Upvar, Temp, Struct, Enum, Variant, Fn, etc |
| `Slot` | hir.rs:96 | Local(idx) \| Upvar(idx, scope) \| Global(idx) \| None |

## ExprKind Variants (Complete List)

1. **Block** - Standalone block as expression
2. **Loop** - Infinite loop
3. **Break** - Break with optional value
4. **Continue** - Continue iteration
5. **Call** - Function/method call
6. **TailCall** - Tail-recursive call (optimization)
7. **Cast** - Type cast
8. **Binary** - Binary operation (17 ops)
9. **Unary** - Unary operation (!x, -x, ...)
10. **Let** - Let binding (if conditions/guards only)
11. **IfElse** - If-else chain
12. **Path** - Variable/function reference
13. **List** - List literal [...]
14. **Dict** - Object literal {...}
15. **FunctionExpression** - Anonymous function
16. **FieldAccess** - obj.field
17. **IndexAccess** - arr[idx]
18. **Literal** - 42, "str", true
19. **Match** - Pattern matching
20. **Range** - 1..10, 1..=10
21. **Wildcard** - _ (not commonly used)

Plus 3 more = 24 total

## StmtKind Variants (Complete List)

1. **Expr** - Expression statement
2. **Let** - Variable declaration
3. **FunctionDeclaration** - Function definition
4. **DynFunctionDeclaration** - Multi-arity function
5. **Return** - Return statement
6. **EnumDeclaration** - Enum type definition
7. **StructDeclaration** - Struct type definition
8. **ProtocolDeclaration** - Protocol definition
9. **ImplBlock** - Implementation block

## BinaryOpKind Variants (17 total)

Arithmetic: Assign, Add, Sub, Mul, Div, Mod, Exp
Comparison: Eq, NotEq, Less, LessEq, Greater, GreaterEq
Logical: And, Or
Bitwise: BitwiseAnd, BitwiseOr, BitwiseXor

## Optimizer Passes (in order)

```
HirOptimizer default
├── SymbolResolution
│   └── IdentifierResolver  [init_context: builds symbol tables]
├── ConstantFolding
│   ├── ConstantFolder
│   └── ConstantPropagator
└── SlotAllocation
    ├── SlotAllocator       [assigns slots to variables]
    └── ScopeDataUpdater    [updates locals/upvars counts]

Runs up to 10 iterations (prevents infinite loops)
```

## JS Codegen Key Mechanisms

### 1. Completion Variables (Stack-based)
```
Stores temp variable names for expression-as-statement conversion
completion_variables: Vec<Option<Box<str>>>

When entering expression context, push name (e.g., "__tmp_0")
When entering statement context, push None
When exiting, pop
```

### 2. BlockContext (Control flow)
```
enum BlockContext {
    Program,      // Top-level module
    Statement,    // In statement position
    Expression,   // In expression position
}
```

### 3. Statement Buffer (Deferred generation)
```
statement_buffer: Vec<String>  // Stack of strings being built

Used to "save" current output, generate nested code,
then restore and inject results
```

### 4. Scope Tracking (Variable naming)
```
Current scope manages:
- Variable name mappings (original → mangled)
- Temp variable counter (__tmp_0, __tmp_1, ...)
- Variable shadowing/aliases
```

## JS Codegen Generation Strategies

### If-Else Expressions

**Strategy 1: Ternary (if possible)**
```
If all branches are pure expressions (no statements):
  cond ? then_expr : else_expr

Checks via expr_can_render_as_js_expr()
Only single else branch (no nested ternaries yet)
```

**Strategy 2: IIFE + Completion Variable (fallback)**
```
if (cond) {
  __tmp_0 = ...;
} else {
  __tmp_0 = ...;
}
// later: use __tmp_0
```

### Match Expressions

**4-Stage Generation:**
1. **setup_match_completion_variables()** - Create temp or reuse "return"
2. **setup_match_context()** - Analyze expr type (Identifier/List/Dynamic)
3. **setup_pattern_identifiers()** - Pre-declare all pattern bindings
4. **generate_match_arms()** - Generate if-else-if chain

**Optimization:** If match expr is already a simple identifier, don't create temp

### Block Expressions

**In Expression Context:**
```
Wraps in IIFE or uses outer temp variable:
let a = (() => {
  // statements
  return expr;
})();
```

**In Statement Context:**
```
Direct statement generation with potential temp var
for completion expression
```

## Test Helpers

### Parse & Lower to HIR (no analysis)
```rust
hir_from_str(input: &str) -> Module
// Uses default SymbolIdAllocator, symbol tables
```

### Parse & Analyze & Lower to HIR
```rust
hir_from_str_analyzed(input: &str) -> Module
// Runs SemanticAnalyzer first, then lowering
// Needed for type inference (enum variant types, etc)
```

### Pretty-Print HIR
```rust
pretty_print(node: &Module) -> String
// Uses tlang_hir_pretty crate with mark_unresolved: false
```

## Known Brittleness/TODOs

1. **Pattern match generator (line 259):** "ohoh..." comment - unhandled ListOfIdentifiers case
2. **Expr generator (line 366-372):** Multiple TODOs about generic statement lifting
3. **Block expression (line 37):** "Halp I made a mess" comment for completion variable handling
4. **Pattern identifiers (line 326):** "Ugly workaround" for comma generation

## No ANF Phase

Tlang does NOT have a separate ANF (Administrative Normal Form) normalization pass.
Instead, it uses:
- Completion variables + temp naming during code generation
- Statement buffers for deferred code
- Context tracking for expression vs statement position

This is more "post-hoc" (during codegen) rather than "pre-hoc" (during HIR optimization).

## How to Find Things

| Looking for | Try |
|-------------|-----|
| All ExprKind variants | `grep "pub enum ExprKind" crates/tlang_hir/src/hir.rs -A 30` |
| All StmtKind variants | `grep "pub enum StmtKind" crates/tlang_hir/src/hir.rs -A 15` |
| Optimizer passes | `crates/tlang_hir_opt/src/hir_opt.rs` line 99-103 |
| If-else generation | `crates/tlang_codegen_js/src/expr_generator.rs` line 348-445 |
| Match generation | `crates/tlang_codegen_js/src/pattern_match_generator.rs` line 415-437 |
| Test helpers | `crates/tlang_ast_lowering/tests/common/mod.rs` |
| Lowering entry | `crates/tlang_ast_lowering/src/lib.rs` line 445 |
| AST→HIR expr | `crates/tlang_ast_lowering/src/expr.rs` line 15 |
| For-loop desugar | `crates/tlang_ast_lowering/src/loop.rs` |
