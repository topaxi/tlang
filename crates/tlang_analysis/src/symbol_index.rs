//! Lightweight symbol index extracted from the semantic analyzer.
//!
//! Instead of caching the full [`SemanticAnalyzer`], this module extracts only
//! the symbol entries and parent links needed for hover/goto lookups.  Both the
//! LSP server and the WASM playground bindings share this implementation to
//! ensure consistent symbol resolution behaviour.

use std::collections::HashMap;
use std::sync::Arc;

use tlang_defs::{DefKind, DefScope};
use tlang_semantics::SemanticAnalyzer;
use tlang_span::{NodeId, Span};

/// A single symbol entry extracted from the semantic analyzer.
#[derive(Debug, Clone)]
pub struct SymbolEntry {
    pub name: Box<str>,
    pub kind: DefKind,
    pub defined_at: Span,
    pub scope_start: u32,
    pub builtin: bool,
}

/// Lightweight symbol index extracted from the semantic analyzer.
///
/// Instead of caching the full [`SemanticAnalyzer`], we extract only the
/// symbol entries and parent links we need for hover/goto lookups.
#[derive(Debug, Default)]
pub struct SymbolIndex {
    /// Maps each scope [`NodeId`] to the symbols declared directly in that
    /// scope (not inherited from parent scopes).
    scopes: HashMap<NodeId, Vec<SymbolEntry>>,
    /// Maps a child scope [`NodeId`] to its parent scope [`NodeId`], enabling
    /// upward scope-chain walks during symbol lookup.
    parents: HashMap<NodeId, NodeId>,
}

impl SymbolIndex {
    /// Build a `SymbolIndex` from the semantic analyzer's symbol tables.
    ///
    /// # Panics
    ///
    /// Panics if any scope `RwLock` is poisoned.
    pub fn from_analyzer(analyzer: &SemanticAnalyzer) -> Self {
        let mut index = SymbolIndex::default();
        let tables = analyzer.symbol_tables();

        // Pre-compute a reverse map from Arc pointer identity to NodeId so that
        // parent lookups are O(1) instead of a linear scan per scope.
        let ptr_to_node: HashMap<*const std::sync::RwLock<DefScope>, NodeId> = tables
            .iter()
            .map(|(&node_id, arc)| (Arc::as_ptr(arc), node_id))
            .collect();

        for (&node_id, scope_rc) in tables {
            let scope = scope_rc.read().unwrap();
            let entries: Vec<SymbolEntry> = scope
                .get_all_local_symbols()
                .iter()
                .filter(|d| d.declared)
                .map(|d| SymbolEntry {
                    name: d.name.clone(),
                    kind: d.kind,
                    defined_at: d.defined_at,
                    scope_start: d.scope_start,
                    builtin: d.builtin,
                })
                .collect();
            index.scopes.insert(node_id, entries);

            // Record parent link via the precomputed pointer→NodeId map.
            if let Some(parent_arc) = scope.parent() {
                let parent_ptr = Arc::as_ptr(&parent_arc);
                if let Some(&parent_id) = ptr_to_node.get(&parent_ptr) {
                    index.parents.insert(node_id, parent_id);
                }
            }
        }

        index
    }

    /// Look up the closest matching symbol by name, mimicking
    /// `DefScope::get_closest_by_name` by walking up the parent chain.
    pub fn get_closest_by_name(
        &self,
        scope_id: NodeId,
        name: &str,
        span: Span,
    ) -> Option<SymbolEntry> {
        let mut current = Some(scope_id);

        while let Some(sid) = current {
            if let Some(entries) = self.scopes.get(&sid) {
                let by_name: Vec<&SymbolEntry> =
                    entries.iter().filter(|e| *e.name == *name).collect();

                if !by_name.is_empty() {
                    // Try scope_start < line first.
                    let closest = by_name
                        .iter()
                        .rev()
                        .find(|e| e.scope_start < span.start_lc.line)
                        .cloned();
                    if let Some(entry) = closest {
                        return Some(entry.clone());
                    }

                    // Fallback: defined_at < cursor, or is a function / builtin.
                    let fallback = by_name
                        .iter()
                        .rev()
                        .find(|e| {
                            e.defined_at.start_lc < span.start_lc
                                || e.kind.arity().is_some()
                                || e.builtin
                        })
                        .cloned();
                    if let Some(entry) = fallback {
                        return Some(entry.clone());
                    }
                }
            }

            // Walk up to the parent scope.
            current = self.parents.get(&sid).copied();
        }

        None
    }
}
