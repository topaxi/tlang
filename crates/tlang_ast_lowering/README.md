# tlang_ast_lowering

Converts the Abstract Syntax Tree (AST) into the High-Level Intermediate Representation (HIR).

## Responsibilities

*   **Desugaring**: Transforms complex AST constructs into simpler HIR equivalents.
*   **Simplification**: Removes syntactic sugar (e.g., `let` bindings might be normalized).
*   **Preparation**: Prepares the code for optimization and code generation.

Input: `tlang_ast`
Output: `tlang_hir`
