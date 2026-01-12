# tlang_hir

Defines the High-Level Intermediate Representation (HIR) for the tlang programming language.

## Purpose

HIR is a simplified and normalized version of the AST, suitable for:
*   Optimization passes.
*   Analysis (flow analysis, dead code detection).
*   Code generation.

It is generally less "syntactic" than the AST and more "semantic".
