# tlang_ast

Defines the Abstract Syntax Tree (AST) for the tlang programming language.

## Structure

*   `ast.rs`: Core AST node definitions (expressions, statements, declarations).
*   `node.rs`: Wrapper `Node` struct that includes `Span` and `NodeId`.
*   Also includes utilities for AST traversal or pretty printing if applicable.

This crate contains purely data structures and does not implement parsing logic.
