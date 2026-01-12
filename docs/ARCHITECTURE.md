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
    *   **Interpreter** (`crates/tlang_runtime/tlang_interpreter`): Directly executes the code (via `tlangdi`).

## Runtime

The runtime (`crates/tlang_runtime`) provides the necessary environment for execution:

*   **Memory** (`crates/tlang_runtime/tlang_memory`): Handles memory management.
*   **Standard Library** (`crates/tlang_runtime/tlang_stdlib`): Built-in functions available to tlang programs.
*   **Interpreter** (`crates/tlang_runtime/tlang_interpreter`): The direct execution engine.
