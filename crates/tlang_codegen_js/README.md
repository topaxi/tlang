# tlang_codegen_js

JavaScript code generator for tlang.

## Responsibilities

*   Takes optimized HIR as input.
*   Generates equivalent JavaScript code.
*   Handles:
    *   Function definitions.
    *   Pattern matching (compilation to if/else or switch).
    *   Tail call optimization (trampolines or loops).
    *   Runtime library calls (from `tlang_runtime`).
