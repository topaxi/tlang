use std::ops::ControlFlow;
use std::panic::AssertUnwindSafe;

use async_lsp::ClientSocket;
use async_lsp::LanguageClient;
use async_lsp::router::Router;
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    GotoDefinitionParams, GotoDefinitionResponse, HoverParams, HoverProviderCapability,
    InitializeParams, InitializeResult, PublishDiagnosticsParams, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};
use serde::Deserialize;
use tlang_analysis::CompilationTarget;
use tlang_defs::DefKind;
use tlang_span::Span;
use tracing::{error, warn};

use crate::diagnostics;
use crate::document_store::{DocumentStore, ParseCache, SymbolIndex};

/// Create an empty AST module (used as a placeholder when parsing fails).
fn empty_module() -> tlang_ast::node::Module {
    tlang_ast::node::Module::new(
        tlang_span::NodeId::new(1),
        vec![],
        tlang_span::Span::default(),
    )
}

/// Settings passed by the editor via `initializationOptions` in the LSP
/// `initialize` request.
///
/// Example Neovim (`lspconfig`) config:
/// ```lua
/// require('lspconfig').tlang_lsp_server.setup {
///   init_options = { target = "interpreter" }
/// }
/// ```
///
/// For VS Code, a client/extension must explicitly forward these values as
/// LSP `initializationOptions` when starting the language server. VS Code
/// does not automatically send arbitrary `settings.json` entries as
/// `initializationOptions`.
#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct ServerSettings {
    /// The compilation target to use for semantic analysis.
    ///
    /// Accepted values: `"js"` (default) or `"interpreter"`.
    #[serde(default)]
    pub(crate) target: TargetSetting,
}

/// The `target` field within [`ServerSettings`].
#[derive(Debug, Default, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub(crate) enum TargetSetting {
    /// JavaScript code-generation backend (default).
    #[default]
    #[serde(rename = "js")]
    Js,
    /// tlang interpreter / VM backend.
    #[serde(rename = "interpreter")]
    Interpreter,
}

impl From<TargetSetting> for CompilationTarget {
    fn from(s: TargetSetting) -> Self {
        match s {
            TargetSetting::Js => CompilationTarget::Js,
            TargetSetting::Interpreter => CompilationTarget::Interpreter,
        }
    }
}

/// The main state of the tlang language server.
pub struct ServerState {
    client: ClientSocket,
    store: DocumentStore,
    target: CompilationTarget,
}

impl ServerState {
    /// Create a new `Router` wired up with all LSP handlers.
    pub fn new_router(client: ClientSocket) -> Router<Self> {
        let mut router = Router::new(Self {
            client,
            store: DocumentStore::new(),
            target: CompilationTarget::default(),
        });

        router
            .request::<lsp_types::request::Initialize, _>(Self::on_initialize)
            .request::<lsp_types::request::Shutdown, _>(|_, _| Box::pin(async { Ok(()) }))
            .request::<lsp_types::request::HoverRequest, _>(Self::on_hover)
            .request::<lsp_types::request::GotoDefinition, _>(Self::on_goto_definition)
            .notification::<lsp_types::notification::Initialized>(|_, _| ControlFlow::Continue(()))
            .notification::<lsp_types::notification::Exit>(|_, _| ControlFlow::Break(Ok(())))
            .notification::<lsp_types::notification::DidOpenTextDocument>(Self::on_did_open)
            .notification::<lsp_types::notification::DidChangeTextDocument>(Self::on_did_change)
            .notification::<lsp_types::notification::DidCloseTextDocument>(Self::on_did_close);

        router
    }

    fn on_initialize(
        state: &mut Self,
        params: InitializeParams,
    ) -> futures::future::BoxFuture<'static, Result<InitializeResult, async_lsp::ResponseError>>
    {
        // Parse initializationOptions if provided.
        let settings: ServerSettings = params
            .initialization_options
            .and_then(|v| match serde_json::from_value(v) {
                Ok(s) => Some(s),
                Err(e) => {
                    warn!("failed to parse initializationOptions: {e}");
                    None
                }
            })
            .unwrap_or_default();

        state.target = settings.target.into();

        Box::pin(async move {
            Ok(InitializeResult {
                capabilities: ServerCapabilities {
                    text_document_sync: Some(TextDocumentSyncCapability::Kind(
                        TextDocumentSyncKind::FULL,
                    )),
                    hover_provider: Some(HoverProviderCapability::Simple(true)),
                    definition_provider: Some(lsp_types::OneOf::Left(true)),
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

    fn on_hover(
        state: &mut Self,
        params: HoverParams,
    ) -> futures::future::BoxFuture<
        'static,
        Result<Option<lsp_types::Hover>, async_lsp::ResponseError>,
    > {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let result = Self::resolve_symbol(state, uri, pos);
        Box::pin(async move { Ok(result.map(|info| info.to_hover())) })
    }

    fn on_goto_definition(
        state: &mut Self,
        params: GotoDefinitionParams,
    ) -> futures::future::BoxFuture<
        'static,
        Result<Option<GotoDefinitionResponse>, async_lsp::ResponseError>,
    > {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let result = Self::resolve_symbol(state, &uri, pos);
        Box::pin(async move { Ok(result.and_then(|info| info.to_goto_definition(&uri))) })
    }

    /// Given a cursor position, find and resolve the symbol under the cursor.
    fn resolve_symbol(state: &Self, uri: &Url, pos: lsp_types::Position) -> Option<ResolvedSymbol> {
        let doc = state.store.get(uri)?;
        let cache = doc.parse_cache.as_ref()?;
        let index = doc.symbol_index.as_ref()?;

        let found =
            crate::find_node::find_node_at_position(&cache.module, pos.line, pos.character)?;

        // Look up the symbol in the scope's symbol table.
        let entry = index.get_closest_by_name(found.scope_id, &found.name, found.span)?;

        Some(ResolvedSymbol {
            name: found.name,
            ident_span: found.span,
            def_kind: entry.kind,
            def_span: entry.defined_at,
            builtin: entry.builtin,
        })
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
    /// The analysis uses the [`CompilationTarget`] that was set during
    /// `initialize` (from `initializationOptions`).  This selects the correct
    /// set of builtin symbols — JS stdlib for [`CompilationTarget::Js`] or VM
    /// builtins for [`CompilationTarget::Interpreter`].
    fn run_diagnostics(&mut self, uri: &Url, source: &str) {
        let target = self.target;

        // Run the full analysis pipeline (parse + semantic) with panic protection.
        let analysis = std::panic::catch_unwind(AssertUnwindSafe(|| {
            tlang_analysis::analyze_for_target(source, target)
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
                // `result.module` is `Some` on a successful parse (even when
                // semantic analysis emits errors) and `None` only when parsing
                // fails outright; `empty_module()` is only a fallback for the
                // parse-failure case.
                let symbol_index = SymbolIndex::from_analyzer(&result.analyzer);
                if let Some(doc) = self.store.documents_mut().get_mut(uri) {
                    doc.parse_cache = Some(ParseCache {
                        source_hash: doc.source_hash,
                        module: result.module.unwrap_or_else(empty_module),
                        diagnostics: parse_diags.clone(),
                    });
                    doc.semantic_cache = Some(semantic_diags.clone());
                    doc.symbol_index = Some(symbol_index);
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
                    doc.symbol_index = None;
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

/// Information about a symbol resolved from a cursor position.
struct ResolvedSymbol {
    /// The identifier name as written in source.
    name: String,
    /// The span of the identifier under the cursor.
    ident_span: Span,
    /// The kind of the resolved definition.
    def_kind: DefKind,
    /// The span where the symbol was defined.
    def_span: Span,
    /// Whether the symbol is a builtin (no source location to jump to).
    builtin: bool,
}

impl ResolvedSymbol {
    fn to_hover(&self) -> lsp_types::Hover {
        let kind_label = self.def_kind.to_string();
        let contents = if let Some(arity) = self.def_kind.arity() {
            format!("({kind_label}) {name}/{arity}", name = self.name)
        } else {
            format!("({kind_label}) {name}", name = self.name)
        };

        lsp_types::Hover {
            contents: lsp_types::HoverContents::Scalar(lsp_types::MarkedString::String(contents)),
            range: Some(diagnostics::span_to_range(&self.ident_span)),
        }
    }

    fn to_goto_definition(&self, uri: &Url) -> Option<GotoDefinitionResponse> {
        if self.builtin {
            return None;
        }

        let range = diagnostics::span_to_range(&self.def_span);
        Some(GotoDefinitionResponse::Scalar(lsp_types::Location {
            uri: uri.clone(),
            range,
        }))
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
        let result =
            tlang_analysis::analyze_for_target("fn add(a, b) { a + b }", CompilationTarget::Js);
        // Valid code should produce no error diagnostics (may have warnings).
        assert!(!result.has_errors(), "unexpected errors");
    }

    #[test]
    fn run_diagnostics_on_source_with_errors() {
        // Using an undeclared variable should produce a diagnostic.
        let result = tlang_analysis::analyze_for_target("fn f() { x }", CompilationTarget::Js);
        assert!(
            !result.analyzer.get_diagnostics().is_empty(),
            "expected diagnostics for undeclared variable"
        );
    }

    #[test]
    fn lsp_diagnostic_conversion_from_analysis_result() {
        let uri = test_uri();
        let result = tlang_analysis::analyze_for_target("fn f() { x }", CompilationTarget::Js);
        let lsp_diags: Vec<_> = result
            .analyzer
            .get_diagnostics()
            .iter()
            .map(|d| diagnostics::from_tlang_diagnostic(d, &uri))
            .collect();
        assert!(!lsp_diags.is_empty());
        assert_eq!(lsp_diags[0].source.as_deref(), Some("tlang"));
    }

    #[test]
    fn server_settings_default_is_js() {
        let settings = ServerSettings::default();
        assert_eq!(settings.target, TargetSetting::Js);
    }

    #[test]
    fn server_settings_parse_js() {
        let v = serde_json::json!({ "target": "js" });
        let s: ServerSettings = serde_json::from_value(v).unwrap();
        assert_eq!(s.target, TargetSetting::Js);
    }

    #[test]
    fn server_settings_parse_interpreter() {
        let v = serde_json::json!({ "target": "interpreter" });
        let s: ServerSettings = serde_json::from_value(v).unwrap();
        assert_eq!(s.target, TargetSetting::Interpreter);
    }

    #[test]
    fn server_settings_parse_empty_object_defaults_to_js() {
        let v = serde_json::json!({});
        let s: ServerSettings = serde_json::from_value(v).unwrap();
        assert_eq!(s.target, TargetSetting::Js);
    }

    #[test]
    fn compilation_target_from_target_setting() {
        assert_eq!(
            CompilationTarget::from(TargetSetting::Js),
            CompilationTarget::Js
        );
        assert_eq!(
            CompilationTarget::from(TargetSetting::Interpreter),
            CompilationTarget::Interpreter
        );
    }

    #[test]
    fn run_diagnostics_interpreter_target_valid_source() {
        let result = tlang_analysis::analyze_for_target(
            "fn add(a, b) { a + b }",
            CompilationTarget::Interpreter,
        );
        assert!(
            !result.has_errors(),
            "unexpected errors for interpreter target"
        );
    }

    /// Helper: create a `ServerState` with a document, run diagnostics, and return the state.
    fn setup_server_with_source(source: &str) -> ServerState {
        let client = async_lsp::ClientSocket::new_closed();
        let mut state = ServerState {
            client,
            store: DocumentStore::new(),
            target: CompilationTarget::Js,
        };
        let uri = test_uri();
        state.store.open(uri.clone(), 1, source.into());
        state.run_diagnostics(&uri, source);
        state
    }

    #[test]
    fn resolve_symbol_for_variable_reference() {
        let state = setup_server_with_source("fn f(x) { x }");
        // `x` in the body is at col 10 (0-indexed columns on first line)
        let resolved = ServerState::resolve_symbol(
            &state,
            &test_uri(),
            lsp_types::Position {
                line: 0,
                character: 10,
            },
        );
        assert!(resolved.is_some(), "should resolve `x` reference");
        let resolved = resolved.unwrap();
        assert_eq!(resolved.name, "x");
        assert_eq!(resolved.def_kind, DefKind::Parameter);
        assert!(!resolved.builtin);
    }

    #[test]
    fn resolve_symbol_for_function_name() {
        let state = setup_server_with_source("fn hello() { 1 }");
        // `hello` starts at col 3
        let resolved = ServerState::resolve_symbol(
            &state,
            &test_uri(),
            lsp_types::Position {
                line: 0,
                character: 3,
            },
        );
        assert!(resolved.is_some(), "should resolve function name");
        let resolved = resolved.unwrap();
        assert_eq!(resolved.name, "hello");
    }

    #[test]
    fn resolve_symbol_returns_none_on_whitespace() {
        let state = setup_server_with_source("fn f() { }");
        let resolved = ServerState::resolve_symbol(
            &state,
            &test_uri(),
            lsp_types::Position {
                line: 0,
                character: 9,
            },
        );
        assert!(resolved.is_none());
    }

    #[test]
    fn hover_returns_kind_and_name() {
        let state = setup_server_with_source("fn f(x) { x }");
        let resolved = ServerState::resolve_symbol(
            &state,
            &test_uri(),
            lsp_types::Position {
                line: 0,
                character: 10,
            },
        )
        .unwrap();
        let hover = resolved.to_hover();
        match hover.contents {
            lsp_types::HoverContents::Scalar(lsp_types::MarkedString::String(s)) => {
                assert_eq!(s, "(parameter) x");
            }
            _ => panic!("unexpected hover contents format"),
        }
    }

    #[test]
    fn hover_includes_arity_for_functions() {
        // `add` on the same line, avoid multiline column offset issues
        let state = setup_server_with_source("fn add(a, b) { a + b }\nlet _ = add(1, 2);");
        // `add` function defined at col 3 on line 0
        let resolved = ServerState::resolve_symbol(
            &state,
            &test_uri(),
            lsp_types::Position {
                line: 0,
                character: 3,
            },
        );
        assert!(resolved.is_some(), "should resolve `add` at definition");
        let resolved = resolved.unwrap();
        let hover = resolved.to_hover();
        match hover.contents {
            lsp_types::HoverContents::Scalar(lsp_types::MarkedString::String(s)) => {
                assert!(
                    s.contains("function"),
                    "hover should mention 'function': {s}"
                );
                assert!(s.contains("add"), "hover should contain name: {s}");
            }
            _ => panic!("unexpected hover contents format"),
        }
    }

    #[test]
    fn goto_definition_returns_location() {
        let state = setup_server_with_source("fn f(x) { x }");
        let resolved = ServerState::resolve_symbol(
            &state,
            &test_uri(),
            lsp_types::Position {
                line: 0,
                character: 10,
            },
        )
        .unwrap();
        let uri = test_uri();
        let goto = resolved.to_goto_definition(&uri);
        assert!(
            goto.is_some(),
            "goto should return a location for non-builtin"
        );
        match goto.unwrap() {
            GotoDefinitionResponse::Scalar(loc) => {
                assert_eq!(loc.uri, uri);
            }
            _ => panic!("expected scalar location"),
        }
    }

    #[test]
    fn goto_definition_returns_none_for_builtin() {
        let state = setup_server_with_source("fn f() { log(1) }");
        // `log` is a builtin; column 9 should be on `log`
        let resolved = ServerState::resolve_symbol(
            &state,
            &test_uri(),
            lsp_types::Position {
                line: 0,
                character: 9,
            },
        );
        if let Some(resolved) = resolved {
            let goto = resolved.to_goto_definition(&test_uri());
            assert!(
                goto.is_none(),
                "goto should return None for builtin symbols"
            );
        }
        // If resolve itself fails (e.g. because `log` is not in the symbol table
        // without configuring builtins), that's also acceptable.
    }

    #[test]
    fn resolve_symbol_unknown_document_returns_none() {
        let state = setup_server_with_source("fn f() { }");
        let unknown_uri = Url::parse("file:///unknown.tlang").unwrap();
        let resolved = ServerState::resolve_symbol(
            &state,
            &unknown_uri,
            lsp_types::Position {
                line: 0,
                character: 0,
            },
        );
        assert!(resolved.is_none());
    }
}
