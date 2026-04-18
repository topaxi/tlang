# tlang Architecture

## Compiler Pipeline

The tlang compiler follows a traditional multi-pass architecture:

1.  **Source Code** (`.tlang` text)
2.  **Lexer** (`crates/tlang_lexer`): Converts source text into a stream of tokens.
3.  **Parser** (`crates/tlang_parser`): Consumes tokens and builds the Abstract Syntax Tree (AST).
4.  **AST** (`crates/tlang_ast`): The initial tree representation of the code.
5.  **Semantics** (`crates/tlang_semantics`): Performs semantic analysis, including symbol resolution and type checking.
6.  **Lowering** (`crates/tlang_ast_lowering`): Transforms AST into High-Level Intermediate Representation (HIR).
7.  **HIR** (`crates/tlang_hir`): A simplified representation optimized for analysis and transformations.
8.  **Optimization** (`crates/tlang_hir_opt`): Performs optimizations on the HIR (e.g., dead code elimination, constant folding).
9.  **Code Generation**:
    *   **JavaScript** (`crates/tlang_codegen_js`): Generates JavaScript code from the optimized HIR.
    *   **Interpreter** (`crates/tlang_core/tlang_interpreter`): Directly executes the code (via `tlangdi`).

## Core

The core (`crates/tlang_core`) provides the necessary environment for execution:

*   **Memory** (`crates/tlang_core/tlang_memory`): Handles memory management.
*   **Standard Library** (`crates/tlang_core/tlang_stdlib`): Built-in functions available to tlang programs.
*   **Interpreter** (`crates/tlang_core/tlang_interpreter`): The direct execution engine.

## Type System

The type checker (`crates/tlang_typeck`) operates on the HIR and assigns types to all expressions and bindings.

### Collection Types: List vs Slice

The type system distinguishes two collection kinds via dedicated `TyKind` variants:

| | `TyKind::List(T)` | `TyKind::Slice(T)` |
|---|---|---|
| Display | `List<T>` | `Slice<T>` |
| Created by | List literals `[1, 2, 3]`, `map`/`filter` results | Rest patterns `[x, ...xs]` (the `xs` binding), `.slice()` method |
| Runtime | `TlangObjectKind::Struct` with list shape (owned array) | `TlangObjectKind::Slice` (view into an existing list) |
| Compatibility | Mutually compatible with `Slice<T>` | Mutually compatible with `List<T>` |

Both types share the same builtin methods (e.g. `map`, `filter`, `foldl`) and are iterable.
The key semantic difference is ownership: a `List` is an independent value while a `Slice`
is a lightweight view referencing a portion of an existing `List`.

**Type annotations**: Writing `List<T>` in user code lowers to `TyKind::List(T)` and `Slice<T>` lowers
to `TyKind::Slice(T)`. The bare unparameterised `List` path remains compatible with both.
