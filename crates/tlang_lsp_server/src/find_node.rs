//! Re-export the shared find-node utilities from [`tlang_analysis::find_node`].
//!
//! The implementation lives in `tlang_analysis` so that both the LSP server
//! and the WASM playground bindings share the same code.

pub use tlang_analysis::find_node::*;
