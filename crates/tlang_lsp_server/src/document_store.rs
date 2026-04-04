use std::collections::HashMap;
use std::hash::{DefaultHasher, Hash, Hasher};

use lsp_types::Url;
use tlang_ast::node as ast;
use tlang_defs::{DefKind, DefScope};
use tlang_semantics::SemanticAnalyzer;
use tlang_span::{NodeId, Span};

/// Cached parse result for a document.
pub struct ParseCache {
    /// Hash of the source text that produced this cache.
    pub source_hash: u64,
    /// The parsed AST module.
    pub module: ast::Module,
    /// Parse diagnostics converted to LSP format.
    pub diagnostics: Vec<lsp_types::Diagnostic>,
}

/// A single symbol entry extracted from the semantic analyzer.
#[derive(Debug, Clone)]
pub struct SymbolEntry {
    pub name: Box<str>,
    pub kind: DefKind,
    pub defined_at: Span,
    pub scope_start: u32,
    pub builtin: bool,
}

/// Send-safe symbol index extracted from the semantic analyzer.
///
/// Replaces caching the full [`SemanticAnalyzer`] (which contains
/// `Rc<RefCell<DefScope>>` and is not `Send`).
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
    pub fn from_analyzer(analyzer: &SemanticAnalyzer) -> Self {
        let mut index = SymbolIndex::default();
        let tables = analyzer.symbol_tables();

        for (&node_id, scope_rc) in tables {
            let scope = scope_rc.borrow();
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

            // Record parent link by checking if this scope's parent matches
            // another known scope.
            Self::record_parent(&mut index.parents, node_id, &scope, tables);
        }

        index
    }

    fn record_parent(
        parents: &mut HashMap<NodeId, NodeId>,
        child_id: NodeId,
        scope: &DefScope,
        tables: &HashMap<NodeId, std::rc::Rc<std::cell::RefCell<DefScope>>>,
    ) {
        if let Some(parent_rc) = scope.parent() {
            // Find which NodeId corresponds to this parent Rc.
            let parent_ptr = std::rc::Rc::as_ptr(&parent_rc) as usize;
            for (&nid, rc) in tables {
                if std::rc::Rc::as_ptr(rc) as usize == parent_ptr {
                    parents.insert(child_id, nid);
                    return;
                }
            }
        }
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

/// Per-document state tracked by the LSP server.
pub struct DocumentState {
    /// Current source text.
    pub source: String,
    /// Hash of source for quick change detection.
    pub source_hash: u64,
    /// Version from LSP client.
    pub version: i32,
    /// Cached parse result (AST + parse diagnostics).
    pub parse_cache: Option<ParseCache>,
    /// Cached semantic diagnostics.
    pub semantic_cache: Option<Vec<lsp_types::Diagnostic>>,
    /// Cached symbol index for hover/goto (extracted from analyzer).
    pub symbol_index: Option<SymbolIndex>,
}

/// Simple HashMap-based document cache.
pub struct DocumentStore {
    documents: HashMap<Url, DocumentState>,
}

fn hash_source(source: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    source.hash(&mut hasher);
    hasher.finish()
}

impl DocumentStore {
    pub fn new() -> Self {
        Self {
            documents: HashMap::new(),
        }
    }

    /// Open a new document, returning a mutable reference to its state.
    pub fn open(&mut self, uri: Url, version: i32, source: String) -> &mut DocumentState {
        let source_hash = hash_source(&source);
        self.documents
            .entry(uri)
            .insert_entry(DocumentState {
                source,
                source_hash,
                version,
                parse_cache: None,
                semantic_cache: None,
                symbol_index: None,
            })
            .into_mut()
    }

    /// Update an existing document. Returns `None` if the document is not open.
    /// Returns `Some((state, changed))` where `changed` indicates whether the
    /// source text actually changed (based on hash comparison).
    pub fn change(
        &mut self,
        uri: &Url,
        version: i32,
        source: String,
    ) -> Option<(&mut DocumentState, bool)> {
        let state = self.documents.get_mut(uri)?;
        let new_hash = hash_source(&source);
        let changed = new_hash != state.source_hash;

        state.version = version;
        state.source = source;
        state.source_hash = new_hash;

        if changed {
            state.parse_cache = None;
            state.semantic_cache = None;
            state.symbol_index = None;
        }

        Some((state, changed))
    }

    /// Close a document, removing it from the store.
    pub fn close(&mut self, uri: &Url) {
        self.documents.remove(uri);
    }

    /// Get a reference to a document's state.
    pub fn get(&self, uri: &Url) -> Option<&DocumentState> {
        self.documents.get(uri)
    }

    /// Get mutable access to the underlying document map.
    pub fn documents_mut(&mut self) -> &mut HashMap<Url, DocumentState> {
        &mut self.documents
    }
}

impl Default for DocumentStore {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_uri(name: &str) -> Url {
        Url::parse(&format!("file:///test/{name}")).unwrap()
    }

    #[test]
    fn open_and_get() {
        let mut store = DocumentStore::new();
        let uri = test_uri("hello.tlang");
        store.open(uri.clone(), 1, "let x = 1;".into());

        let state = store.get(&uri).unwrap();
        assert_eq!(state.source, "let x = 1;");
        assert_eq!(state.version, 1);
        assert!(state.parse_cache.is_none());
        assert!(state.semantic_cache.is_none());
    }

    #[test]
    fn change_with_different_source_invalidates_cache() {
        let mut store = DocumentStore::new();
        let uri = test_uri("hello.tlang");
        store.open(uri.clone(), 1, "let x = 1;".into());

        let (state, changed) = store.change(&uri, 2, "let x = 2;".into()).unwrap();
        assert!(changed);
        assert_eq!(state.version, 2);
        assert_eq!(state.source, "let x = 2;");
    }

    #[test]
    fn change_with_same_source_preserves_cache() {
        let mut store = DocumentStore::new();
        let uri = test_uri("hello.tlang");
        store.open(uri.clone(), 1, "let x = 1;".into());

        let (state, changed) = store.change(&uri, 2, "let x = 1;".into()).unwrap();
        assert!(!changed);
        assert_eq!(state.version, 2);
    }

    #[test]
    fn change_unknown_document_returns_none() {
        let mut store = DocumentStore::new();
        let uri = test_uri("unknown.tlang");
        assert!(store.change(&uri, 1, "text".into()).is_none());
    }

    #[test]
    fn close_removes_document() {
        let mut store = DocumentStore::new();
        let uri = test_uri("hello.tlang");
        store.open(uri.clone(), 1, "let x = 1;".into());

        store.close(&uri);
        assert!(store.get(&uri).is_none());
    }
}
