use std::ops::ControlFlow;
use std::panic::AssertUnwindSafe;

use async_lsp::router::Router;
use async_lsp::ClientSocket;
use async_lsp::LanguageClient;
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    InitializeParams, InitializeResult, PublishDiagnosticsParams, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};
use tracing::error;

use crate::diagnostics;
use crate::document_store::{DocumentStore, ParseCache};

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
            .notification::<lsp_types::notification::Initialized>(|_, _| ControlFlow::Continue(()))
            .notification::<lsp_types::notification::DidOpenTextDocument>(Self::on_did_open)
            .notification::<lsp_types::notification::DidChangeTextDocument>(Self::on_did_change)
            .notification::<lsp_types::notification::DidCloseTextDocument>(Self::on_did_close);

        router
    }

    fn on_initialize(
        _state: &mut Self,
        _params: InitializeParams,
    ) -> futures::future::BoxFuture<
        'static,
        Result<InitializeResult, async_lsp::ResponseError>,
    > {
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

    /// Run the full diagnostic pipeline: parse → semantic analysis → publish.
    fn run_diagnostics(&mut self, uri: &Url, source: &str) {
        let mut all_diagnostics = Vec::new();

        // Phase 1: Parse (with panic catch for parser robustness).
        let parse_result = std::panic::catch_unwind(AssertUnwindSafe(|| {
            let mut parser = tlang_parser::Parser::from_source(source);
            parser.parse()
        }));

        let parsed_module = match parse_result {
            Ok(Ok((module, _parse_meta))) => {
                // Cache successful parse with empty diagnostics.
                if let Some(doc) = self.store.documents_mut().get_mut(uri) {
                    doc.parse_cache = Some(ParseCache {
                        source_hash: doc.source_hash,
                        module: module.clone(),
                        diagnostics: vec![],
                    });
                }
                Some(module)
            }
            Ok(Err(parse_error)) => {
                let parse_diags: Vec<lsp_types::Diagnostic> = parse_error
                    .issues()
                    .iter()
                    .map(diagnostics::from_parse_issue)
                    .collect();
                all_diagnostics.extend(parse_diags.clone());

                // Cache the parse diagnostics even though parsing failed.
                if let Some(doc) = self.store.documents_mut().get_mut(uri) {
                    doc.parse_cache = Some(ParseCache {
                        source_hash: doc.source_hash,
                        module: tlang_ast::node::Module::default(),
                        diagnostics: parse_diags,
                    });
                }
                None
            }
            Err(_panic) => {
                error!("parser panicked for {uri}");
                let diag = lsp_types::Diagnostic {
                    range: lsp_types::Range::default(),
                    severity: Some(lsp_types::DiagnosticSeverity::ERROR),
                    source: Some("tlang".into()),
                    message: "Internal error: parser panicked".into(),
                    ..Default::default()
                };
                all_diagnostics.push(diag);
                None
            }
        };

        // Phase 2: Semantic analysis (only if parsing succeeded).
        if let Some(mut module) = parsed_module {
            let semantic_diags = Self::run_semantic_analysis(&mut module);
            all_diagnostics.extend(semantic_diags.clone());

            // Cache semantic diagnostics.
            if let Some(doc) = self.store.documents_mut().get_mut(uri) {
                doc.semantic_cache = Some(semantic_diags);
                // Update the cached module with the analyzed version.
                if let Some(ref mut cache) = doc.parse_cache {
                    cache.module = module;
                }
            }
        }

        // Phase 3: Publish all collected diagnostics.
        let version = self.store.get(uri).map(|d| d.version);
        self.publish_diagnostics(uri.clone(), all_diagnostics, version);
    }

    /// Run semantic analysis on a parsed module, returning LSP diagnostics.
    fn run_semantic_analysis(module: &mut tlang_ast::node::Module) -> Vec<lsp_types::Diagnostic> {
        let mut analyzer = tlang_semantics::SemanticAnalyzer::default();
        analyzer.add_builtin_symbols(
            tlang_codegen_js::generator::CodegenJS::get_standard_library_symbols(),
        );

        let analysis_result = analyzer.analyze(module);

        // Collect all diagnostics (errors + warnings).
        let all_diagnostics = analyzer.get_diagnostics();
        let mut lsp_diags: Vec<lsp_types::Diagnostic> = all_diagnostics
            .iter()
            .map(diagnostics::from_tlang_diagnostic)
            .collect();

        // If analyze() returned Err, those error diagnostics are already included
        // in get_diagnostics(). But if it returned Ok, we still want warnings.
        if let Err(errors) = analysis_result {
            // Errors from the result that might not be in get_diagnostics().
            for err in &errors {
                if !all_diagnostics.iter().any(|d| d.message() == err.message()) {
                    lsp_diags.push(diagnostics::from_tlang_diagnostic(err));
                }
            }
        }

        lsp_diags
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

    #[test]
    fn run_semantic_analysis_on_valid_source() {
        let source = "fn add(a, b) { a + b }";
        let mut parser = tlang_parser::Parser::from_source(source);
        let (mut module, _) = parser.parse().unwrap();

        let diags = ServerState::run_semantic_analysis(&mut module);
        // Valid code should produce no error diagnostics (may have warnings).
        let errors: Vec<_> = diags
            .iter()
            .filter(|d| d.severity == Some(lsp_types::DiagnosticSeverity::ERROR))
            .collect();
        assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    }

    #[test]
    fn run_semantic_analysis_on_source_with_errors() {
        // Using an undeclared variable should produce a diagnostic.
        let source = "fn f() { x }";
        let mut parser = tlang_parser::Parser::from_source(source);
        let (mut module, _) = parser.parse().unwrap();

        let diags = ServerState::run_semantic_analysis(&mut module);
        assert!(
            !diags.is_empty(),
            "expected diagnostics for undeclared variable"
        );
    }
}
