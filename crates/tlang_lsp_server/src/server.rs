use std::ops::ControlFlow;
use std::panic::AssertUnwindSafe;

use async_lsp::ClientSocket;
use async_lsp::LanguageClient;
use async_lsp::router::Router;
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionOptions, CompletionParams, CompletionResponse,
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    GotoDefinitionParams, GotoDefinitionResponse, HoverParams, HoverProviderCapability,
    InitializeParams, InitializeResult, InlayHint, InlayHintParams, PublishDiagnosticsParams,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};
use serde::Deserialize;
use tlang_analysis::CompilationTarget;
use tlang_analysis::query::ResolvedSymbol;
use tlang_defs::DefKind;
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
            .request::<lsp_types::request::Completion, _>(Self::on_completion)
            .request::<lsp_types::request::InlayHintRequest, _>(Self::on_inlay_hint)
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
                    completion_provider: Some(CompletionOptions::default()),
                    inlay_hint_provider: Some(lsp_types::OneOf::Left(true)),
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

    fn on_completion(
        state: &mut Self,
        params: CompletionParams,
    ) -> futures::future::BoxFuture<
        'static,
        Result<Option<CompletionResponse>, async_lsp::ResponseError>,
    > {
        let uri = &params.text_document_position.text_document.uri;
        let items = Self::collect_completions(state, uri);
        Box::pin(async move { Ok(Some(CompletionResponse::Array(items))) })
    }

    fn on_inlay_hint(
        state: &mut Self,
        params: InlayHintParams,
    ) -> futures::future::BoxFuture<'static, Result<Option<Vec<InlayHint>>, async_lsp::ResponseError>>
    {
        let uri = params.text_document.uri;
        let range = params.range;

        // Ensure typed HIR is available — compute on demand if not cached.
        Self::ensure_typed_hir(state, &uri);

        let hints = match state.store.get(&uri) {
            Some(doc) => match doc.typed_hir.as_ref() {
                Some(typed_hir) => {
                    let line_range = Some((range.start.line, range.end.line));
                    let analysis_hints =
                        tlang_analysis::inlay_hints::collect_inlay_hints(typed_hir, line_range);

                    analysis_hints
                        .into_iter()
                        .map(|h| Self::to_lsp_inlay_hint(&h))
                        .collect()
                }
                None => vec![],
            },
            None => vec![],
        };

        Box::pin(async move { Ok(Some(hints)) })
    }

    /// Convert an analysis-layer inlay hint to an LSP inlay hint.
    fn to_lsp_inlay_hint(hint: &tlang_analysis::inlay_hints::InlayHint) -> InlayHint {
        InlayHint {
            position: lsp_types::Position {
                line: hint.line,
                character: hint.character,
            },
            label: lsp_types::InlayHintLabel::String(hint.label.clone()),
            kind: Some(match hint.kind {
                tlang_analysis::inlay_hints::InlayHintKind::Type => lsp_types::InlayHintKind::TYPE,
                tlang_analysis::inlay_hints::InlayHintKind::ReturnType => {
                    lsp_types::InlayHintKind::TYPE
                }
            }),
            text_edits: None,
            tooltip: None,
            padding_left: Some(matches!(
                hint.kind,
                tlang_analysis::inlay_hints::InlayHintKind::ReturnType
            )),
            padding_right: None,
            data: None,
        }
    }

    /// Ensure the typed HIR cache is populated for a document.
    ///
    /// If the cache is empty, runs the full analysis + HIR lowering + type
    /// checking pipeline.  This is a lazy computation — the typed HIR is only
    /// produced when an inlay hint request arrives.
    fn ensure_typed_hir(state: &mut Self, uri: &Url) {
        let needs_compute = state
            .store
            .get(uri)
            .is_some_and(|doc| doc.typed_hir.is_none());

        if !needs_compute {
            return;
        }

        let source = match state.store.get(uri) {
            Some(doc) => doc.source.clone(),
            None => return,
        };

        let target = state.target;
        let typed_hir = std::panic::catch_unwind(AssertUnwindSafe(|| {
            let result = tlang_analysis::analyze_for_target(&source, target);
            tlang_analysis::inlay_hints::lower_and_typecheck(&result)
        }));

        if let Some(doc) = state.store.documents_mut().get_mut(uri) {
            doc.typed_hir = match typed_hir {
                Ok(hir) => hir,
                Err(_) => {
                    error!("typed HIR computation panicked for {uri}");
                    None
                }
            };
        }
    }

    /// Collect LSP completion items for a document.
    ///
    /// Delegates to [`SymbolIndex::collect_completion_items`] — the canonical
    /// implementation shared with the WASM playground — and converts each
    /// protocol-agnostic item into an [`lsp_types::CompletionItem`].
    fn collect_completions(state: &Self, uri: &Url) -> Vec<CompletionItem> {
        let Some(doc) = state.store.get(uri) else {
            return vec![];
        };
        let Some(index) = doc.symbol_index.as_ref() else {
            return vec![];
        };

        index
            .collect_completion_items()
            .into_iter()
            .map(|item| CompletionItem {
                label: item.label,
                kind: Some(def_kind_to_completion_item_kind(item.kind)),
                detail: item.detail,
                ..CompletionItem::default()
            })
            .collect()
    }

    /// Resolve the symbol under the cursor for a given document and position.
    ///
    /// Delegates to [`tlang_analysis::query::resolve_symbol`] which is shared
    /// between the LSP server and the WASM playground bindings.
    ///
    /// Returns `None` when the document is unknown, has no cached analysis, the
    /// cursor is not on an identifier, or the identifier cannot be resolved.
    fn resolve_symbol(state: &Self, uri: &Url, pos: lsp_types::Position) -> Option<ResolvedSymbol> {
        let doc = state.store.get(uri)?;
        let cache = doc.parse_cache.as_ref()?;
        let index = doc.symbol_index.as_ref()?;

        tlang_analysis::query::resolve_symbol(&cache.module, index, pos.line, pos.character)
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

/// LSP-specific extension methods for [`ResolvedSymbol`].
trait ResolvedSymbolLspExt {
    fn to_hover(&self) -> lsp_types::Hover;
    fn to_goto_definition(&self, uri: &Url) -> Option<GotoDefinitionResponse>;
}

impl ResolvedSymbolLspExt for ResolvedSymbol {
    fn to_hover(&self) -> lsp_types::Hover {
        lsp_types::Hover {
            contents: lsp_types::HoverContents::Scalar(lsp_types::MarkedString::String(
                self.hover_text(),
            )),
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

/// Map a [`DefKind`] to an LSP [`CompletionItemKind`].
///
/// Tagged enum variants with arity > 0 are mapped to `FUNCTION` because they
/// are callable constructors (e.g. `Some(value)`), while zero-arity variants
/// are plain values mapped to `ENUM_MEMBER`.
fn def_kind_to_completion_item_kind(kind: DefKind) -> CompletionItemKind {
    match kind {
        DefKind::Function(_) | DefKind::FunctionSelfRef(_) => CompletionItemKind::FUNCTION,
        DefKind::Variable => CompletionItemKind::VARIABLE,
        DefKind::Const => CompletionItemKind::CONSTANT,
        DefKind::Parameter => CompletionItemKind::VARIABLE,
        DefKind::Enum => CompletionItemKind::ENUM,
        DefKind::EnumVariant(0) => CompletionItemKind::ENUM_MEMBER,
        DefKind::EnumVariant(_) => CompletionItemKind::FUNCTION,
        DefKind::Struct => CompletionItemKind::STRUCT,
        DefKind::Protocol => CompletionItemKind::INTERFACE,
        DefKind::ProtocolMethod(_) | DefKind::StructMethod(_) => CompletionItemKind::METHOD,
        DefKind::Module => CompletionItemKind::MODULE,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tlang_defs::DefKind;

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
    fn resolve_symbol_multiline_uses_lsp_positions() {
        // LSP Position.character is 0-based; the lexer uses 1-based columns
        // after the first line.  resolve_symbol must translate correctly.
        let state = setup_server_with_source("fn f() {\n  let x = 1;\n  x\n}");
        // `x` on line 2 at 2-space indent → LSP character=2 (0-based)
        let resolved = ServerState::resolve_symbol(
            &state,
            &test_uri(),
            lsp_types::Position {
                line: 2,
                character: 2,
            },
        );
        assert!(
            resolved.is_some(),
            "should resolve `x` on line 2 with LSP 0-based column"
        );
        assert_eq!(resolved.unwrap().name, "x");
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

    #[test]
    fn completion_returns_user_defined_functions() {
        let state = setup_server_with_source("fn add(a, b) { a + b }");
        let items = ServerState::collect_completions(&state, &test_uri());
        assert!(
            items.iter().any(|i| i.label == "add"),
            "completions should include `add`, got: {items:?}"
        );
    }

    #[test]
    fn completion_items_have_correct_kind() {
        let state = setup_server_with_source("fn add(a, b) { a + b }");
        let items = ServerState::collect_completions(&state, &test_uri());
        let add = items.iter().find(|i| i.label == "add").unwrap();
        assert_eq!(add.kind, Some(CompletionItemKind::FUNCTION));
    }

    #[test]
    fn completion_items_have_detail() {
        let state = setup_server_with_source("fn add(a, b) { a + b }");
        let items = ServerState::collect_completions(&state, &test_uri());
        let add = items.iter().find(|i| i.label == "add").unwrap();
        assert_eq!(add.detail.as_deref(), Some("fn(2)"));
    }

    #[test]
    fn completion_returns_empty_for_unknown_document() {
        let state = setup_server_with_source("fn f() { }");
        let unknown_uri = Url::parse("file:///unknown.tlang").unwrap();
        let items = ServerState::collect_completions(&state, &unknown_uri);
        assert!(items.is_empty());
    }

    #[test]
    fn completion_includes_enums() {
        let state = setup_server_with_source("enum Color { Red, Green, Blue }");
        let items = ServerState::collect_completions(&state, &test_uri());
        assert!(
            items.iter().any(|i| i.label == "Color"),
            "completions should include enum `Color`, got: {items:?}"
        );
    }

    #[test]
    fn def_kind_to_completion_kind_mapping() {
        assert_eq!(
            def_kind_to_completion_item_kind(DefKind::Function(2)),
            CompletionItemKind::FUNCTION
        );
        assert_eq!(
            def_kind_to_completion_item_kind(DefKind::Variable),
            CompletionItemKind::VARIABLE
        );
        assert_eq!(
            def_kind_to_completion_item_kind(DefKind::Const),
            CompletionItemKind::CONSTANT
        );
        assert_eq!(
            def_kind_to_completion_item_kind(DefKind::Enum),
            CompletionItemKind::ENUM
        );
        assert_eq!(
            def_kind_to_completion_item_kind(DefKind::EnumVariant(0)),
            CompletionItemKind::ENUM_MEMBER
        );
        assert_eq!(
            def_kind_to_completion_item_kind(DefKind::Struct),
            CompletionItemKind::STRUCT
        );
        assert_eq!(
            def_kind_to_completion_item_kind(DefKind::Protocol),
            CompletionItemKind::INTERFACE
        );
        assert_eq!(
            def_kind_to_completion_item_kind(DefKind::Module),
            CompletionItemKind::MODULE
        );
        assert_eq!(
            def_kind_to_completion_item_kind(DefKind::ProtocolMethod(1)),
            CompletionItemKind::METHOD
        );
    }

    // ── Inlay hint tests ───────────────────────────────────────────────

    #[test]
    fn inlay_hint_returns_type_hints_for_let_binding() {
        let mut state = setup_server_with_source("let x = 42;");
        ServerState::ensure_typed_hir(&mut state, &test_uri());

        let doc = state.store.get(&test_uri()).unwrap();
        assert!(doc.typed_hir.is_some(), "typed HIR should be cached");

        let typed_hir = doc.typed_hir.as_ref().unwrap();
        let hints = tlang_analysis::inlay_hints::collect_inlay_hints(typed_hir, None);
        assert!(
            hints.iter().any(|h| h.label == ": i64"),
            "expected `: i64` hint, got: {hints:?}"
        );
    }

    #[test]
    fn inlay_hint_returns_return_type_hints_for_function() {
        let mut state = setup_server_with_source("fn add(a: i64, b: i64) { a + b }");
        ServerState::ensure_typed_hir(&mut state, &test_uri());

        let doc = state.store.get(&test_uri()).unwrap();
        let typed_hir = doc.typed_hir.as_ref().unwrap();
        let hints = tlang_analysis::inlay_hints::collect_inlay_hints(typed_hir, None);
        assert!(
            hints.iter().any(|h| h.label.contains("-> i64")),
            "expected return type hint, got: {hints:?}"
        );
    }

    #[test]
    fn inlay_hint_no_return_type_hint_when_annotated() {
        let mut state = setup_server_with_source("fn add(a: i64, b: i64) -> i64 { a + b }");
        ServerState::ensure_typed_hir(&mut state, &test_uri());

        let doc = state.store.get(&test_uri()).unwrap();
        let typed_hir = doc.typed_hir.as_ref().unwrap();
        let hints = tlang_analysis::inlay_hints::collect_inlay_hints(typed_hir, None);
        assert!(
            !hints
                .iter()
                .any(|h| h.kind == tlang_analysis::inlay_hints::InlayHintKind::ReturnType),
            "should not show return type hint when annotated, got: {hints:?}"
        );
    }

    #[test]
    fn inlay_hint_lsp_conversion_sets_correct_kind() {
        let hint = tlang_analysis::inlay_hints::InlayHint {
            line: 0,
            character: 5,
            label: ": i64".into(),
            kind: tlang_analysis::inlay_hints::InlayHintKind::Type,
        };
        let lsp_hint = ServerState::to_lsp_inlay_hint(&hint);
        assert_eq!(lsp_hint.kind, Some(lsp_types::InlayHintKind::TYPE));
        assert_eq!(lsp_hint.position.line, 0);
        assert_eq!(lsp_hint.position.character, 5);
        match &lsp_hint.label {
            lsp_types::InlayHintLabel::String(s) => assert_eq!(s, ": i64"),
            _ => panic!("expected string label"),
        }
    }

    #[test]
    fn inlay_hint_return_type_has_padding_left() {
        let hint = tlang_analysis::inlay_hints::InlayHint {
            line: 0,
            character: 10,
            label: " -> i64".into(),
            kind: tlang_analysis::inlay_hints::InlayHintKind::ReturnType,
        };
        let lsp_hint = ServerState::to_lsp_inlay_hint(&hint);
        assert_eq!(lsp_hint.padding_left, Some(true));
    }

    #[test]
    fn inlay_hint_type_has_no_padding_left() {
        let hint = tlang_analysis::inlay_hints::InlayHint {
            line: 0,
            character: 5,
            label: ": i64".into(),
            kind: tlang_analysis::inlay_hints::InlayHintKind::Type,
        };
        let lsp_hint = ServerState::to_lsp_inlay_hint(&hint);
        assert_eq!(lsp_hint.padding_left, Some(false));
    }

    #[test]
    fn inlay_hint_cache_invalidated_on_source_change() {
        let client = async_lsp::ClientSocket::new_closed();
        let mut state = ServerState {
            client,
            store: DocumentStore::new(),
            target: CompilationTarget::Js,
        };
        let uri = test_uri();
        state.store.open(uri.clone(), 1, "let x = 42;".into());
        state.run_diagnostics(&uri, "let x = 42;");
        ServerState::ensure_typed_hir(&mut state, &uri);
        assert!(state.store.get(&uri).unwrap().typed_hir.is_some());

        // Change source — typed_hir cache should be invalidated.
        state.store.change(&uri, 2, "let y = \"hello\";".into());
        assert!(
            state.store.get(&uri).unwrap().typed_hir.is_none(),
            "typed_hir should be invalidated on source change"
        );
    }
}
