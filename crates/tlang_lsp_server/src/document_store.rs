use std::collections::HashMap;
use std::hash::{DefaultHasher, Hash, Hasher};

use lsp_types::Url;
use tlang_ast::node as ast;

/// Cached parse result for a document.
pub struct ParseCache {
    /// Hash of the source text that produced this cache.
    pub source_hash: u64,
    /// The parsed AST module.
    pub module: ast::Module,
    /// Parse diagnostics converted to LSP format.
    pub diagnostics: Vec<lsp_types::Diagnostic>,
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
