# tlang_runtime

The runtime environment for tlang.

## Purpose

This crate serves as a unified entry point for runtime components required by the interpreter and compiled code (wasm/js).

## Components

It re-exports:
*   `tlang_memory`: Memory management (values, scopes).
*   `tlang_stdlib`: Standard library functions.
*   `tlang_interpreter`: The direct execution engine.

This crate ensures that consumers (like `tlang_cli` or `tlang_bindings_js`) have a consistent view of the runtime types.
