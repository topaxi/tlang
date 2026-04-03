use std::ops::ControlFlow;
use std::panic::AssertUnwindSafe;

use async_lsp::ClientSocket;
use async_lsp::LanguageClient;
use async_lsp::router::Router;
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    InitializeParams, InitializeResult, PublishDiagnosticsParams, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};
use tracing::error;

use crate::diagnostics;
use crate::document_store::{DocumentStore, ParseCache};

/// Create an empty AST module (used as a placeholder when parsing fails).
fn empty_module() -> tlang_ast::node::Module {
    tlang_ast::node::Module::new(
        tlang_span::NodeId::new(1),
        vec![],
        tlang_span::Span::default(),
    )
}

/// The main state of the tlang language server.
pub struct ServerState {
    client: ClientSocket,
    store: DocumentStore,
}

impl ServerState {
    /// Create a new `Router` wired up with all LSP handlers.
    pub fn new_router(client: ClientSocket) -> Router<Self> {
        let mut router = Router::new(Self {
            client,
            store: DocumentStore::new(),
        });

        router
            .request::<lsp_types::request::Initialize, _>(Self::on_initialize)
            .request::<lsp_types::request::Shutdown, _>(|_, _| Box::pin(async { Ok(()) }))
            .notification::<lsp_types::notification::Initialized>(|_, _| ControlFlow::Continue(()))
            .notification::<lsp_types::notification::Exit>(|_, _| ControlFlow::Break(Ok(())))
            .notification::<lsp_types::notification::DidOpenTextDocument>(Self::on_did_open)
            .notification::<lsp_types::notification::DidChangeTextDocument>(Self::on_did_change)
            .notification::<lsp_types::notification::DidCloseTextDocument>(Self::on_did_close);

        router
    }

    fn on_initialize(
        _state: &mut Self,
        _params: InitializeParams,
    ) -> futures::future::BoxFuture<'static, Result<InitializeResult, async_lsp::ResponseError>>
    {
        Box::pin(async move {
            Ok(InitializeResult {
                capabilities: ServerCapabilities {
                    text_document_sync: Some(TextDocumentSyncCapability::Kind(
                        TextDocumentSyncKind::FULL,
                    )),
                    ..ServerCapabilities::default()
                },
                server_info: Some(lsp_types::ServerInfo {
                    name: "tlang-lsp-server".into(),
                    version: Some(env!("CARGO_PKG_VERSION").into()),
                }),
            })
        })
    }

    fn on_did_open(
        state: &mut Self,
        params: DidOpenTextDocumentParams,
    ) -> ControlFlow<async_lsp::Result<()>> {
        let uri = params.text_document.uri;
        let version = params.text_document.version;
        let source = params.text_document.text;

        state.store.open(uri.clone(), version, source.clone());
        state.run_diagnostics(&uri, &source);
        ControlFlow::Continue(())
    }

    fn on_did_change(
        state: &mut Self,
        params: DidChangeTextDocumentParams,
    ) -> ControlFlow<async_lsp::Result<()>> {
        let uri = params.text_document.uri;
        let version = params.text_document.version;

        // Full document sync: the last content change contains the entire document.
        let source = match params.content_changes.into_iter().last() {
            Some(change) => change.text,
            None => return ControlFlow::Continue(()),
        };

        match state.store.change(&uri, version, source.clone()) {
            Some((_state, true)) => {
                // Source changed — reparse and reanalyze.
                state.run_diagnostics(&uri, &source);
            }
            Some((doc_state, false)) => {
                // Source hash unchanged — republish cached diagnostics.
                let cached = Self::collect_cached_diagnostics(doc_state);
                state.publish_diagnostics(uri, cached, Some(version));
            }
            None => {
                // Unknown document — ignore.
            }
        }
        ControlFlow::Continue(())
    }

    fn on_did_close(
        state: &mut Self,
        params: DidCloseTextDocumentParams,
    ) -> ControlFlow<async_lsp::Result<()>> {
        let uri = params.text_document.uri;

        // Clear diagnostics for the closed document.
        state.publish_diagnostics(uri.clone(), vec![], None);
        state.store.close(&uri);
        ControlFlow::Continue(())
    }

    /// Collect cached diagnostics from a document state (parse + semantic).
    fn collect_cached_diagnostics(
        state: &crate::document_store::DocumentState,
    ) -> Vec<lsp_types::Diagnostic> {
        let mut all = Vec::new();
        if let Some(ref cache) = state.parse_cache {
            all.extend(cache.diagnostics.iter().cloned());
        }
        if let Some(ref semantic) = state.semantic_cache {
            all.extend(semantic.iter().cloned());
        }
        all
    }

    /// Run the full diagnostic pipeline using `tlang_analysis`, then publish.
    ///
    /// This replaces the previous manual parse → `SemanticAnalyzer` setup with
    /// a single call to the shared `tlang_analysis` pipeline, which is also
    /// used (via `configure_js_analyzer`) by the WASM playground bindings.
    fn run_diagnostics(&mut self, uri: &Url, source: &str) {
        // Run the full analysis pipeline (parse + semantic) with panic protection.
        let analysis = std::panic::catch_unwind(AssertUnwindSafe(|| {
            tlang_analysis::analyze_with_js_symbols(source)
        }));

        let all_diagnostics = match analysis {
            Ok(result) => {
                // Convert parse issues → LSP diagnostics.
                let parse_diags: Vec<lsp_types::Diagnostic> = result
                    .parse_issues
                    .iter()
                    .map(diagnostics::from_parse_issue)
                    .collect();

                // Convert semantic diagnostics → LSP diagnostics.
                let semantic_diags: Vec<lsp_types::Diagnostic> = result
                    .analyzer
                    .get_diagnostics()
                    .iter()
                    .map(|d| diagnostics::from_tlang_diagnostic(d, uri))
                    .collect();

                // Update caches so unchanged-source republishing works.
                if let Some(doc) = self.store.documents_mut().get_mut(uri) {
                    doc.parse_cache = Some(ParseCache {
                        source_hash: doc.source_hash,
                        module: result.module.unwrap_or_else(empty_module),
                        diagnostics: parse_diags.clone(),
                    });
                    doc.semantic_cache = Some(semantic_diags.clone());
                }

                let mut all = parse_diags;
                all.extend(semantic_diags);
                all
            }
            Err(_panic) => {
                error!("analysis panicked for {uri}");
                let diag = lsp_types::Diagnostic {
                    range: lsp_types::Range::default(),
                    severity: Some(lsp_types::DiagnosticSeverity::ERROR),
                    source: Some("tlang".into()),
                    message: "Internal error: analysis panicked".into(),
                    ..Default::default()
                };

                // Cache the panic diagnostic so it is republished on unchanged
                // `didChange` requests rather than silently clearing.
                if let Some(doc) = self.store.documents_mut().get_mut(uri) {
                    doc.parse_cache = Some(ParseCache {
                        source_hash: doc.source_hash,
                        module: empty_module(),
                        diagnostics: vec![diag.clone()],
                    });
                    doc.semantic_cache = Some(vec![]);
                }

                vec![diag]
            }
        };

        // Publish all collected diagnostics.
        let version = self.store.get(uri).map(|d| d.version);
        self.publish_diagnostics(uri.clone(), all_diagnostics, version);
    }

    /// Send diagnostics to the client.
    fn publish_diagnostics(
        &mut self,
        uri: Url,
        diagnostics: Vec<lsp_types::Diagnostic>,
        version: Option<i32>,
    ) {
        let _ = self.client.publish_diagnostics(PublishDiagnosticsParams {
            uri,
            diagnostics,
            version,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_uri() -> Url {
        Url::parse("file:///test/example.tlang").unwrap()
    }

    #[test]
    fn run_diagnostics_on_valid_source() {
        let result = tlang_analysis::analyze_with_js_symbols("fn add(a, b) { a + b }");
        // Valid code should produce no error diagnostics (may have warnings).
        let errors: Vec<_> = result
            .analyzer
            .get_diagnostics()
            .into_iter()
            .filter(|d| d.is_error())
            .collect();
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    }

    #[test]
    fn run_diagnostics_on_source_with_errors() {
        // Using an undeclared variable should produce a diagnostic.
        let result = tlang_analysis::analyze_with_js_symbols("fn f() { x }");
        assert!(
            !result.analyzer.get_diagnostics().is_empty(),
            "expected diagnostics for undeclared variable"
        );
    }

    #[test]
    fn lsp_diagnostic_conversion_from_analysis_result() {
        let uri = test_uri();
        let result = tlang_analysis::analyze_with_js_symbols("fn f() { x }");
        let lsp_diags: Vec<_> = result
            .analyzer
            .get_diagnostics()
            .iter()
            .map(|d| diagnostics::from_tlang_diagnostic(d, &uri))
            .collect();
        assert!(!lsp_diags.is_empty());
        assert_eq!(lsp_diags[0].source.as_deref(), Some("tlang"));
    }
}
