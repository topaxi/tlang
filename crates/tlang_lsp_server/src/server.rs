use std::ops::ControlFlow;
use std::panic::AssertUnwindSafe;

use async_lsp::ClientSocket;
use async_lsp::LanguageClient;
use async_lsp::router::Router;
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionOptions, CompletionParams, CompletionResponse,
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DidSaveTextDocumentParams, GotoDefinitionParams, GotoDefinitionResponse, HoverParams,
    HoverProviderCapability, InitializeParams, InitializeResult, InitializedParams, InlayHint,
    InlayHintParams, NumberOrString, ParameterInformation, ParameterLabel, ProgressParams,
    ProgressParamsValue, PublishDiagnosticsParams, SemanticTokensFullOptions, SemanticTokensParams,
    SemanticTokensResult, SemanticTokensServerCapabilities, ServerCapabilities,
    SignatureHelpOptions, SignatureHelpParams, SignatureInformation, TextDocumentSyncCapability,
    TextDocumentSyncKind, TextDocumentSyncOptions, TextDocumentSyncSaveOptions, Url,
    WorkDoneProgress, WorkDoneProgressBegin, WorkDoneProgressCreateParams, WorkDoneProgressEnd,
    WorkDoneProgressReport,
};
use serde::Deserialize;
use tlang_analysis::CompilationTarget;
use tlang_analysis::query::ResolvedSymbol;
use tlang_defs::DefKind;
use tracing::{error, warn};

use crate::diagnostics;
use crate::document_store::{DocumentStore, ParseCache, SymbolIndex};
use crate::semantic_tokens;

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
    progress_supported: bool,
    next_progress_token: u64,
    startup_progress_reported: bool,
}

impl ServerState {
    /// Create a new `Router` wired up with all LSP handlers.
    pub fn new_router(client: ClientSocket) -> Router<Self> {
        let mut router = Router::new(Self {
            client,
            store: DocumentStore::new(),
            target: CompilationTarget::default(),
            progress_supported: false,
            next_progress_token: 0,
            startup_progress_reported: false,
        });

        router
            .request::<lsp_types::request::Initialize, _>(Self::on_initialize)
            .request::<lsp_types::request::Shutdown, _>(|_, _| Box::pin(async { Ok(()) }))
            .request::<lsp_types::request::HoverRequest, _>(Self::on_hover)
            .request::<lsp_types::request::GotoDefinition, _>(Self::on_goto_definition)
            .request::<lsp_types::request::Completion, _>(Self::on_completion)
            .request::<lsp_types::request::SignatureHelpRequest, _>(Self::on_signature_help)
            .request::<lsp_types::request::InlayHintRequest, _>(Self::on_inlay_hint)
            .request::<lsp_types::request::SemanticTokensFullRequest, _>(
                Self::on_semantic_tokens_full,
            )
            .notification::<lsp_types::notification::Initialized>(Self::on_initialized)
            .notification::<lsp_types::notification::Exit>(|_, _| ControlFlow::Break(Ok(())))
            .notification::<lsp_types::notification::DidOpenTextDocument>(Self::on_did_open)
            .notification::<lsp_types::notification::DidChangeTextDocument>(Self::on_did_change)
            .notification::<lsp_types::notification::DidSaveTextDocument>(Self::on_did_save)
            .notification::<lsp_types::notification::DidCloseTextDocument>(Self::on_did_close);

        router
    }

    fn on_initialize(
        state: &mut Self,
        params: InitializeParams,
    ) -> futures::future::BoxFuture<'static, Result<InitializeResult, async_lsp::ResponseError>>
    {
        let InitializeParams {
            initialization_options,
            capabilities,
            ..
        } = params;

        // Parse initializationOptions if provided.
        let settings: ServerSettings = initialization_options
            .and_then(|v| match serde_json::from_value(v) {
                Ok(s) => Some(s),
                Err(e) => {
                    warn!("failed to parse initializationOptions: {e}");
                    None
                }
            })
            .unwrap_or_default();

        state.target = settings.target.into();
        state.progress_supported = capabilities
            .window
            .as_ref()
            .and_then(|window| window.work_done_progress)
            == Some(true);

        Box::pin(async move {
            Ok(InitializeResult {
                capabilities: ServerCapabilities {
                    text_document_sync: Some(TextDocumentSyncCapability::Options(
                        TextDocumentSyncOptions {
                            open_close: Some(true),
                            change: Some(TextDocumentSyncKind::FULL),
                            save: Some(TextDocumentSyncSaveOptions::Supported(true)),
                            ..TextDocumentSyncOptions::default()
                        },
                    )),
                    hover_provider: Some(HoverProviderCapability::Simple(true)),
                    definition_provider: Some(lsp_types::OneOf::Left(true)),
                    completion_provider: Some(CompletionOptions {
                        trigger_characters: Some(vec![".".into(), ":".into()]),
                        ..CompletionOptions::default()
                    }),
                    signature_help_provider: Some(SignatureHelpOptions {
                        trigger_characters: Some(vec!["(".into(), ",".into()]),
                        retrigger_characters: Some(vec![",".into()]),
                        ..SignatureHelpOptions::default()
                    }),
                    inlay_hint_provider: Some(lsp_types::OneOf::Left(true)),
                    semantic_tokens_provider: Some(
                        SemanticTokensServerCapabilities::SemanticTokensOptions(
                            lsp_types::SemanticTokensOptions {
                                work_done_progress_options: Default::default(),
                                legend: semantic_tokens::legend(),
                                range: None,
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                        ),
                    ),
                    ..ServerCapabilities::default()
                },
                server_info: Some(lsp_types::ServerInfo {
                    name: "tlang-lsp-server".into(),
                    version: Some(env!("CARGO_PKG_VERSION").into()),
                }),
            })
        })
    }

    fn on_initialized(
        state: &mut Self,
        _: InitializedParams,
    ) -> ControlFlow<async_lsp::Result<()>> {
        if state.startup_progress_reported {
            return ControlFlow::Continue(());
        }

        if let Some(progress) = state.begin_progress(
            "Starting tlang language server",
            Some(format!(
                "Preparing {target} analysis pipeline",
                target = Self::target_label(state.target)
            )),
            Some(0),
        ) {
            progress.report("Ready to analyze documents", 100);
            progress.finish("Language server ready");
        }

        state.startup_progress_reported = true;
        ControlFlow::Continue(())
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

    fn on_did_save(
        state: &mut Self,
        params: DidSaveTextDocumentParams,
    ) -> ControlFlow<async_lsp::Result<()>> {
        let uri = params.text_document.uri;

        let Some(source) = state.store.get(&uri).map(|doc| doc.source.clone()) else {
            warn!("ignoring didSave for unopened document: {uri}");
            return ControlFlow::Continue(());
        };

        state.run_diagnostics(&uri, &source);

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

        // Ensure typed HIR is available for type enrichment.
        Self::ensure_typed_hir(state, uri);

        let mut resolved = Self::resolve_symbol(state, uri, pos);

        // Enrich shared hover details from the typed HIR when available.
        if let Some(ref mut sym) = resolved
            && let Some(doc) = state.store.get(uri)
            && let Some(cache) = doc.parse_cache.as_ref()
        {
            tlang_analysis::query::enrich_hover_symbol(
                &cache.module,
                doc.typed_hir.as_ref().and_then(Option::as_ref),
                sym,
            );
        }

        // If standard symbol resolution didn't find anything, try the
        // shared member resolution module for `receiver.member` expressions
        // (e.g. `re"foo".replace_all` where the member is a builtin method).
        if resolved.is_none()
            && let Some(member) = Self::resolve_member(state, uri, pos)
        {
            let hover_text = if let Some(sig) = &member.signature {
                sig.label.clone()
            } else {
                let ret_str = member
                    .return_ty
                    .as_ref()
                    .map_or("unknown".to_string(), |ty| ty.to_string());
                format!("{}: {ret_str}", member.name)
            };

            let hover = lsp_types::Hover {
                contents: lsp_types::HoverContents::Scalar(lsp_types::MarkedString::String(
                    hover_text,
                )),
                range: None,
            };
            return Box::pin(async move { Ok(Some(hover)) });
        }

        // Last resort: try type_at_definition directly at the cursor position.
        // This handles cases like hovering on the `fn` keyword of a closure,
        // which is not a symbol reference but has type information in the HIR.
        if resolved.is_none()
            && let Some(doc) = state.store.get(uri)
            && let Some(Some(typed_hir)) = doc.typed_hir.as_ref()
            && let Some(type_str) =
                tlang_analysis::inlay_hints::type_at_definition(typed_hir, pos.line, pos.character)
        {
            let hover = lsp_types::Hover {
                contents: lsp_types::HoverContents::Scalar(lsp_types::MarkedString::String(
                    type_str,
                )),
                range: None,
            };
            return Box::pin(async move { Ok(Some(hover)) });
        }

        Box::pin(async move { Ok(resolved.map(|info| info.to_hover())) })
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
        Self::ensure_typed_hir(state, &uri);
        let result = Self::resolve_symbol(state, &uri, pos)
            .and_then(|info| info.to_goto_definition(&uri))
            .or_else(|| {
                Self::resolve_member(state, &uri, pos)
                    .and_then(|m| member_to_goto_definition(&m, &uri))
            });
        Box::pin(async move { Ok(result) })
    }

    fn on_completion(
        state: &mut Self,
        params: CompletionParams,
    ) -> futures::future::BoxFuture<
        'static,
        Result<Option<CompletionResponse>, async_lsp::ResponseError>,
    > {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;

        // Check if this is a dot-triggered completion for method access.
        let is_dot = params
            .context
            .as_ref()
            .and_then(|ctx| ctx.trigger_character.as_deref())
            == Some(".");

        let items = if is_dot {
            Self::collect_dot_completions(state, uri, pos)
        } else {
            Self::collect_completions(state, uri)
        };

        Box::pin(async move { Ok(Some(CompletionResponse::Array(items))) })
    }

    fn on_signature_help(
        state: &mut Self,
        params: SignatureHelpParams,
    ) -> futures::future::BoxFuture<
        'static,
        Result<Option<lsp_types::SignatureHelp>, async_lsp::ResponseError>,
    > {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let help = Self::collect_signature_help(state, &uri, pos);

        Box::pin(async move { Ok(help) })
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
                Some(Some(typed_hir)) => {
                    // The LSP range `end` is exclusive.  Adjust the end line
                    // so we don't request an extra line of hints when the
                    // client sends `end = { line: N+1, character: 0 }`.
                    let end_line = if range.end.character == 0 && range.end.line > range.start.line
                    {
                        range.end.line.saturating_sub(1)
                    } else {
                        range.end.line
                    };
                    let line_range = Some((range.start.line, end_line));
                    let analysis_hints =
                        tlang_analysis::inlay_hints::collect_inlay_hints(typed_hir, line_range);

                    analysis_hints
                        .into_iter()
                        .map(|h| Self::to_lsp_inlay_hint(&h, &doc.source))
                        .filter(|hint| {
                            let pos = hint.position;
                            let after_start = pos.line > range.start.line
                                || (pos.line == range.start.line
                                    && pos.character >= range.start.character);
                            let before_end = pos.line < range.end.line
                                || (pos.line == range.end.line
                                    && pos.character < range.end.character);
                            after_start && before_end
                        })
                        .collect()
                }
                _ => vec![],
            },
            None => vec![],
        };

        Box::pin(async move { Ok(Some(hints)) })
    }

    fn on_semantic_tokens_full(
        state: &mut Self,
        params: SemanticTokensParams,
    ) -> futures::future::BoxFuture<
        'static,
        Result<Option<SemanticTokensResult>, async_lsp::ResponseError>,
    > {
        let uri = params.text_document.uri;

        Self::ensure_typed_hir(state, &uri);

        let tokens = state
            .store
            .get(&uri)
            .and_then(|doc| {
                let cache = doc.parse_cache.as_ref()?;
                let index = doc.symbol_index.as_ref()?;
                Some(semantic_tokens::encode(&semantic_tokens::collect(
                    &doc.source,
                    &cache.module,
                    index,
                    doc.typed_hir.as_ref().and_then(Option::as_ref),
                )))
            })
            .map(SemanticTokensResult::Tokens);

        Box::pin(async move { Ok(tokens) })
    }

    /// Convert an analysis-layer inlay hint to an LSP inlay hint.
    ///
    /// The analysis layer provides 0-based `(line, character)` positions using
    /// Unicode scalar value counts.  The LSP protocol requires `character` to
    /// be a UTF-16 code unit offset, so we convert here using the document
    /// source.
    fn to_lsp_inlay_hint(hint: &tlang_analysis::inlay_hints::InlayHint, source: &str) -> InlayHint {
        // Convert the 0-based character (Unicode scalar) to a UTF-16 offset.
        let utf16_character = char_column_to_utf16(source, hint.line, hint.character);

        InlayHint {
            position: lsp_types::Position {
                line: hint.line,
                character: utf16_character,
            },
            label: lsp_types::InlayHintLabel::String(hint.label.clone()),
            kind: Some(match hint.kind {
                tlang_analysis::inlay_hints::InlayHintKind::Type => lsp_types::InlayHintKind::TYPE,
                tlang_analysis::inlay_hints::InlayHintKind::ReturnType => {
                    lsp_types::InlayHintKind::TYPE
                }
                tlang_analysis::inlay_hints::InlayHintKind::ChainedPipeline => {
                    lsp_types::InlayHintKind::TYPE
                }
            }),
            text_edits: None,
            tooltip: None,
            padding_left: None,
            padding_right: None,
            data: None,
        }
    }

    /// Ensure the typed HIR cache is populated for a document.
    ///
    /// If the cache is empty (`None`), runs the full analysis + HIR lowering +
    /// type checking pipeline.  If the pipeline previously failed for this
    /// source version, the result is cached as `Some(None)` so we don't
    /// recompute on every request.  The cache is invalidated when the source
    /// changes (see [`DocumentStore::change`]).
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
        let progress = state.begin_progress(
            format!(
                "Preparing editor features for {}",
                Self::document_label(uri)
            ),
            Some("Parsing and semantic analysis".into()),
            Some(10),
        );

        let analysis = std::panic::catch_unwind(AssertUnwindSafe(|| {
            tlang_analysis::analyze_for_target(&source, target)
        }));

        let typed_hir = match analysis {
            Ok(result) => {
                if let Some(progress) = progress.as_ref() {
                    progress.report("Type checking", 70);
                }
                std::panic::catch_unwind(AssertUnwindSafe(|| {
                    tlang_analysis::inlay_hints::lower_and_typecheck(&result)
                }))
            }
            Err(err) => Err(err),
        };

        if let Some(doc) = state.store.documents_mut().get_mut(uri) {
            doc.typed_hir = Some(match typed_hir {
                Ok(hir) => hir,
                Err(_) => {
                    error!("typed HIR computation panicked for {uri}");
                    None
                }
            });
        }

        if let Some(progress) = progress {
            progress.finish("Editor feature analysis complete");
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

    /// Collect dot-triggered method completions for a receiver expression.
    ///
    /// When the user types `v1.`, this delegates to the shared member
    /// resolution module which uses typed HIR to determine the receiver type
    /// and enumerate all available members (user-defined, protocol impls,
    /// and builtin methods).
    fn collect_dot_completions(
        state: &mut Self,
        uri: &Url,
        pos: lsp_types::Position,
    ) -> Vec<CompletionItem> {
        // Ensure typed HIR is available for member resolution.
        Self::ensure_typed_hir(state, uri);

        let Some(doc) = state.store.get(uri) else {
            return vec![];
        };

        // Try typed-HIR-backed member resolution first.
        if let Some(Some(typed_hir)) = doc.typed_hir.as_ref() {
            let candidates = tlang_analysis::member_resolution::complete_members_at_position(
                &doc.source,
                typed_hir,
                doc.symbol_index.as_ref(),
                pos.line,
                pos.character,
            );

            if !candidates.is_empty() {
                return candidates
                    .into_iter()
                    .map(|c| {
                        let kind = match c.kind {
                            tlang_analysis::member_resolution::MemberKind::Method => {
                                CompletionItemKind::METHOD
                            }
                            tlang_analysis::member_resolution::MemberKind::Field => {
                                CompletionItemKind::FIELD
                            }
                        };
                        CompletionItem {
                            label: c.name,
                            kind: Some(kind),
                            detail: c.signature.map(|s| s.label),
                            ..CompletionItem::default()
                        }
                    })
                    .collect();
            }
        }

        // Fallback: use the old symbol-index path for cases where typed HIR
        // did not resolve a receiver type (e.g. identifier receivers).
        let receiver = extract_receiver_before_dot(&doc.source, pos);
        let receiver = match receiver {
            Some(r) => r,
            None => return Self::collect_completions(state, uri),
        };

        let receiver_col = pos.character.saturating_sub(receiver.len() as u32 + 1);
        let receiver_pos = lsp_types::Position::new(pos.line, receiver_col);
        let resolved = Self::resolve_symbol(state, uri, receiver_pos);

        // Try to get the type name from type info enrichment.
        let type_name = resolved
            .as_ref()
            .and_then(|r| r.type_info.as_deref())
            .or_else(|| {
                resolved
                    .as_ref()
                    .filter(|r| matches!(r.def_kind, DefKind::Struct | DefKind::Enum))
                    .map(|r| r.name.as_str())
            });

        let type_name = match type_name {
            Some(t) => t.to_string(),
            None => {
                let doc = match state.store.get(uri) {
                    Some(d) => d,
                    None => return vec![],
                };
                match doc.typed_hir.as_ref().and_then(|t| t.as_ref()) {
                    Some(typed_hir) => {
                        let line = receiver_pos.line;
                        let col = receiver_pos.character;
                        // type_at_definition expects 0-based editor positions
                        // and handles coordinate conversion internally.
                        match tlang_analysis::inlay_hints::type_at_definition(typed_hir, line, col)
                        {
                            Some(ty) => ty,
                            None => return Self::collect_completions(state, uri),
                        }
                    }
                    None => return Self::collect_completions(state, uri),
                }
            }
        };

        // Use the centralized member resolution for the resolved type name.
        let doc = match state.store.get(uri) {
            Some(d) => d,
            None => return vec![],
        };

        if let Some(Some(typed_hir)) = doc.typed_hir.as_ref() {
            let candidates = tlang_analysis::member_resolution::complete_members_for_type(
                typed_hir,
                doc.symbol_index.as_ref(),
                &type_name,
            );

            if !candidates.is_empty() {
                return candidates
                    .into_iter()
                    .map(|c| {
                        let kind = match c.kind {
                            tlang_analysis::member_resolution::MemberKind::Method => {
                                CompletionItemKind::METHOD
                            }
                            tlang_analysis::member_resolution::MemberKind::Field => {
                                CompletionItemKind::FIELD
                            }
                        };
                        CompletionItem {
                            label: c.name,
                            kind: Some(kind),
                            detail: c.signature.map(|s| s.label),
                            ..CompletionItem::default()
                        }
                    })
                    .collect();
            }
        }

        // Final fallback: symbol-index-only method completions.
        let doc = match state.store.get(uri) {
            Some(d) => d,
            None => return vec![],
        };
        let Some(index) = doc.symbol_index.as_ref() else {
            return vec![];
        };

        let methods = index.collect_method_completions(&type_name);
        if methods.is_empty() {
            return Self::collect_completions(state, uri);
        }

        methods
            .into_iter()
            .map(|item| CompletionItem {
                label: item.label,
                kind: Some(def_kind_to_completion_item_kind(item.kind)),
                detail: item.detail,
                ..CompletionItem::default()
            })
            .collect()
    }

    fn collect_signature_help(
        state: &mut Self,
        uri: &Url,
        pos: lsp_types::Position,
    ) -> Option<lsp_types::SignatureHelp> {
        Self::ensure_typed_hir(state, uri);

        let doc = state.store.get(uri)?;
        let typed_hir = doc.typed_hir.as_ref()?.as_ref()?;
        let help = tlang_analysis::signature_help::signature_help_at(
            &doc.source,
            typed_hir,
            pos.line,
            pos.character,
        )?;

        Some(to_lsp_signature_help(help))
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

    fn resolve_member(
        state: &mut Self,
        uri: &Url,
        pos: lsp_types::Position,
    ) -> Option<tlang_analysis::member_resolution::ResolvedMember> {
        Self::ensure_typed_hir(state, uri);
        let doc = state.store.get(uri)?;
        let typed_hir = doc.typed_hir.as_ref()?.as_ref()?;
        tlang_analysis::member_resolution::resolve_member_at_position(
            &doc.source,
            typed_hir,
            pos.line,
            pos.character,
        )
    }

    /// Collect cached diagnostics from a document state (parse + non-parse).
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
        let progress = self.begin_progress(
            format!("Analyzing {}", Self::document_label(uri)),
            Some("Parsing and semantic analysis".into()),
            Some(10),
        );

        // Run the full analysis pipeline (parse + semantic) with panic protection.
        let analysis = std::panic::catch_unwind(AssertUnwindSafe(|| {
            tlang_analysis::analyze_for_target(source, target)
        }));

        let all_diagnostics = match analysis {
            Ok(result) => {
                if let Some(progress) = progress.as_ref() {
                    progress.report("Indexing symbols", 55);
                }

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

                let has_semantic_errors = result
                    .analyzer
                    .get_diagnostics()
                    .iter()
                    .any(|diagnostic| diagnostic.is_error());

                let symbol_index = SymbolIndex::from_analyzer(&result.analyzer);

                let (typed_hir, typed_diags) = if has_semantic_errors {
                    (Some(None), Vec::new())
                } else {
                    if let Some(progress) = progress.as_ref() {
                        progress.report("Type checking", 80);
                    }

                    match std::panic::catch_unwind(AssertUnwindSafe(|| {
                        tlang_analysis::inlay_hints::lower_and_typecheck(&result)
                    })) {
                        Ok(Some(typed_hir)) => {
                            let typed_diags = typed_hir
                                .diagnostics
                                .iter()
                                .map(|d| diagnostics::from_tlang_diagnostic(d, uri))
                                .collect();
                            (Some(Some(typed_hir)), typed_diags)
                        }
                        Ok(None) => (Some(None), Vec::new()),
                        Err(_) => {
                            error!("typed HIR computation panicked for {uri}");
                            (Some(None), Vec::new())
                        }
                    }
                };

                let mut non_parse_diags = semantic_diags.clone();
                non_parse_diags.extend(typed_diags);

                // Update caches so unchanged-source republishing works.
                // `result.module` is `Some` on a successful parse (even when
                // semantic analysis emits errors) and `None` only when parsing
                // fails outright; `empty_module()` is only a fallback for the
                // parse-failure case.
                if let Some(doc) = self.store.documents_mut().get_mut(uri) {
                    doc.parse_cache = Some(ParseCache {
                        source_hash: doc.source_hash,
                        module: result.module.unwrap_or_else(empty_module),
                        diagnostics: parse_diags.clone(),
                    });
                    doc.semantic_cache = Some(non_parse_diags.clone());
                    doc.symbol_index = Some(symbol_index);
                    doc.typed_hir = typed_hir;
                }

                let mut all = parse_diags;
                all.extend(non_parse_diags);
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
                    doc.typed_hir = Some(None);
                }

                vec![diag]
            }
        };

        if let Some(progress) = progress.as_ref() {
            progress.report("Publishing diagnostics", 95);
        }

        // Publish all collected diagnostics.
        let version = self.store.get(uri).map(|d| d.version);
        self.publish_diagnostics(uri.clone(), all_diagnostics, version);

        if let Some(progress) = progress {
            progress.finish("Analysis complete");
        }
    }

    /// Send diagnostics to the client.
    fn publish_diagnostics(
        &mut self,
        uri: Url,
        diagnostics: Vec<lsp_types::Diagnostic>,
        version: Option<i32>,
    ) {
        if let Err(err) = self.client.publish_diagnostics(PublishDiagnosticsParams {
            uri,
            diagnostics,
            version,
        }) {
            warn!("failed to publish diagnostics: {err}");
        }
    }

    fn begin_progress(
        &mut self,
        title: impl Into<String>,
        message: Option<String>,
        percentage: Option<u32>,
    ) -> Option<WorkDoneProgressReporter> {
        if !self.progress_supported {
            return None;
        }

        let token = NumberOrString::String(format!("tlang/progress/{}", self.next_progress_token));
        self.next_progress_token += 1;

        let mut client = self.client.clone();
        std::mem::drop(
            client.work_done_progress_create(WorkDoneProgressCreateParams {
                token: token.clone(),
            }),
        );

        let reporter = WorkDoneProgressReporter {
            client,
            token: token.clone(),
        };

        if reporter
            .notify(WorkDoneProgress::Begin(WorkDoneProgressBegin {
                title: title.into(),
                cancellable: Some(false),
                message,
                percentage,
            }))
            .is_err()
        {
            return None;
        }

        Some(reporter)
    }

    fn document_label(uri: &Url) -> String {
        uri.path_segments()
            .and_then(|mut segments| segments.rfind(|segment| !segment.is_empty()))
            .map(ToOwned::to_owned)
            .filter(|label| !label.is_empty())
            .unwrap_or_else(|| uri.as_str().to_owned())
    }

    fn target_label(target: CompilationTarget) -> &'static str {
        match target {
            CompilationTarget::Js => "JavaScript",
            CompilationTarget::Interpreter => "interpreter",
        }
    }
}

struct WorkDoneProgressReporter {
    client: ClientSocket,
    token: NumberOrString,
}

impl WorkDoneProgressReporter {
    fn report(&self, message: impl Into<String>, percentage: u32) {
        let _ = self.notify(WorkDoneProgress::Report(WorkDoneProgressReport {
            cancellable: Some(false),
            message: Some(message.into()),
            percentage: Some(percentage),
        }));
    }

    fn finish(&self, message: impl Into<String>) {
        let _ = self.notify(WorkDoneProgress::End(WorkDoneProgressEnd {
            message: Some(message.into()),
        }));
    }

    fn notify(&self, value: WorkDoneProgress) -> Result<(), async_lsp::Error> {
        self.client
            .clone()
            .progress(ProgressParams {
                token: self.token.clone(),
                value: ProgressParamsValue::WorkDone(value),
            })
            .map_err(|err| {
                warn!("failed to publish progress: {err}");
                err
            })
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

fn member_to_goto_definition(
    member: &tlang_analysis::member_resolution::ResolvedMember,
    uri: &Url,
) -> Option<GotoDefinitionResponse> {
    if member.builtin {
        return None;
    }
    let def_span = member.def_span?;
    let range = diagnostics::span_to_range(&def_span);
    Some(GotoDefinitionResponse::Scalar(lsp_types::Location {
        uri: uri.clone(),
        range,
    }))
}

/// Extract the identifier immediately before a dot at the given cursor position.
///
/// When the user types `v1.` and triggers completion, the cursor is at the
/// position after the dot.  This function scans backwards from just before the
/// dot to extract the identifier (`v1` in this case).
///
/// Returns `None` if the character before the cursor is not a dot or if there
/// is no valid identifier before it.
fn extract_receiver_before_dot(source: &str, pos: lsp_types::Position) -> Option<String> {
    // Find the line.
    let line = source.lines().nth(pos.line as usize)?;
    let col = pos.character as usize;

    // The cursor is right after the dot, so the dot is at col-1.
    if col == 0 || col > line.len() {
        return None;
    }
    let before_dot = &line[..col - 1];

    // Scan backwards for an identifier.
    let end = before_dot.len();
    let start = before_dot
        .char_indices()
        .rev()
        .take_while(|(_, c)| c.is_alphanumeric() || *c == '_')
        .last()
        .map(|(i, _)| i)?;

    let ident = &before_dot[start..end];
    if ident.is_empty() {
        return None;
    }

    Some(ident.to_string())
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
        DefKind::StructField => CompletionItemKind::FIELD,
        DefKind::Protocol => CompletionItemKind::INTERFACE,
        DefKind::ProtocolMethod(_) | DefKind::StructMethod(_) => CompletionItemKind::METHOD,
        DefKind::Module => CompletionItemKind::MODULE,
    }
}

fn to_lsp_signature_help(
    help: tlang_analysis::signature_help::SignatureHelp,
) -> lsp_types::SignatureHelp {
    lsp_types::SignatureHelp {
        signatures: help
            .signatures
            .into_iter()
            .map(|signature| SignatureInformation {
                label: signature.label,
                documentation: None,
                parameters: Some(
                    signature
                        .parameters
                        .into_iter()
                        .map(|param| ParameterInformation {
                            label: ParameterLabel::Simple(param.label),
                            documentation: None,
                        })
                        .collect(),
                ),
                active_parameter: None,
            })
            .collect(),
        active_signature: Some(help.active_signature),
        active_parameter: Some(help.active_parameter),
    }
}

/// Convert a 0-based (line, character) position from the analysis layer to
/// a UTF-16 code unit column offset as required by the LSP protocol.
///
/// For pure-ASCII sources the column is returned unchanged.  For sources
/// containing non-BMP characters (surrogate pairs), the column is adjusted
/// so that the LSP client renders the hint at the correct position.
fn char_column_to_utf16(source: &str, line: u32, char_column: u32) -> u32 {
    // Find the start of the target line.
    let mut current_line = 0u32;
    let mut line_start = 0usize;
    for (i, ch) in source.char_indices() {
        if current_line == line {
            line_start = i;
            break;
        }
        if ch == '\n' {
            current_line += 1;
        }
    }

    // Count `char_column` Unicode scalars from `line_start` and accumulate
    // the corresponding number of UTF-16 code units.
    let mut utf16_offset = 0u32;
    for (col, ch) in source[line_start..].chars().enumerate() {
        if col as u32 >= char_column || ch == '\n' {
            break;
        }
        utf16_offset += ch.len_utf16() as u32;
    }

    utf16_offset
}

#[cfg(test)]
mod tests {
    use lsp_types::{SemanticTokenType, TextDocumentIdentifier, TextDocumentPositionParams};

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
    fn run_diagnostics_includes_type_errors_from_typed_hir() {
        let state = setup_server_with_source("fn f() -> String { 1 }");
        let diags = ServerState::collect_cached_diagnostics(state.store.get(&test_uri()).unwrap());

        assert!(
            diags
                .iter()
                .any(|diag| diag.severity == Some(lsp_types::DiagnosticSeverity::ERROR)),
            "expected an error diagnostic for typed-HIR/typecheck failure, got: {diags:?}"
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
            progress_supported: false,
            next_progress_token: 0,
            startup_progress_reported: false,
        };
        let uri = test_uri();
        state.store.open(uri.clone(), 1, source.into());
        state.run_diagnostics(&uri, source);
        state
    }

    fn semantic_token_type_index(token_type: SemanticTokenType) -> u32 {
        semantic_tokens::legend()
            .token_types
            .iter()
            .position(|candidate| *candidate == token_type)
            .expect("token type should exist in legend") as u32
    }

    fn decode_semantic_tokens(tokens: &lsp_types::SemanticTokens) -> Vec<(u32, u32, u32, u32)> {
        let mut line = 0u32;
        let mut start = 0u32;
        let mut decoded = Vec::with_capacity(tokens.data.len());

        for token in &tokens.data {
            line += token.delta_line;
            start = if token.delta_line == 0 {
                start + token.delta_start
            } else {
                token.delta_start
            };
            decoded.push((line, start, token.length, token.token_type));
        }

        decoded
    }

    #[test]
    fn initialize_advertises_semantic_tokens_full_capability() {
        let client = async_lsp::ClientSocket::new_closed();
        let mut state = ServerState {
            client,
            store: DocumentStore::new(),
            target: CompilationTarget::Js,
            progress_supported: false,
            next_progress_token: 0,
            startup_progress_reported: false,
        };

        let result = futures::executor::block_on(ServerState::on_initialize(
            &mut state,
            InitializeParams::default(),
        ))
        .expect("initialize should succeed");

        let provider = result
            .capabilities
            .semantic_tokens_provider
            .expect("semantic token capability should be present");

        match provider {
            SemanticTokensServerCapabilities::SemanticTokensOptions(options) => {
                assert_eq!(options.full, Some(SemanticTokensFullOptions::Bool(true)));
                assert!(options.range.is_none());
                assert!(
                    options
                        .legend
                        .token_types
                        .contains(&SemanticTokenType::FUNCTION)
                );
                assert!(
                    options
                        .legend
                        .token_types
                        .contains(&SemanticTokenType::METHOD)
                );
            }
            _ => panic!("unexpected semantic token capability variant"),
        }
    }

    #[test]
    fn semantic_tokens_full_returns_method_and_property_tokens() {
        let mut state = setup_server_with_source(
            "struct Vector { x: i64 }\nfn Vector.add(self, other: Vector) -> Vector { self.x }\nlet v = Vector { x: 1 };\nv.add(v);",
        );

        let result = futures::executor::block_on(ServerState::on_semantic_tokens_full(
            &mut state,
            SemanticTokensParams {
                text_document: TextDocumentIdentifier { uri: test_uri() },
                work_done_progress_params: Default::default(),
                partial_result_params: Default::default(),
            },
        ))
        .expect("semantic tokens request should succeed")
        .expect("semantic tokens should be present");

        let SemanticTokensResult::Tokens(tokens) = result else {
            panic!("expected full semantic token response");
        };
        let decoded = decode_semantic_tokens(&tokens);

        assert!(
            decoded.iter().any(|&(line, start, length, token_type)| {
                line == 3
                    && start == 2
                    && length == 3
                    && token_type == semantic_token_type_index(SemanticTokenType::METHOD)
            }),
            "expected method token for `add`, got: {decoded:?}"
        );
        assert!(
            decoded.iter().any(|&(line, start, length, token_type)| {
                line == 1
                    && start == 52
                    && length == 1
                    && token_type == semantic_token_type_index(SemanticTokenType::PROPERTY)
            }),
            "expected property token for `self.x`, got: {decoded:?}"
        );
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
    fn hover_includes_signature_and_docs_for_functions() {
        let mut state = setup_server_with_source(
            "/// Add numbers\nfn add(a: i64, b) -> i64 { a }\nlet _ = add(1, 2);",
        );
        let hover = futures::executor::block_on(ServerState::on_hover(
            &mut state,
            HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: test_uri() },
                    position: lsp_types::Position {
                        line: 1,
                        character: 3,
                    },
                },
                work_done_progress_params: Default::default(),
            },
        ))
        .expect("hover request should succeed")
        .expect("hover should be present");
        match hover.contents {
            lsp_types::HoverContents::Scalar(lsp_types::MarkedString::String(s)) => {
                assert_eq!(s, "fn add(a: i64, b: unknown) -> i64\n\nAdd numbers");
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
    fn goto_definition_position_is_zero_based() {
        // `add` is defined on line 1 at 0-based column 3 (i.e. `fn add(...)`)
        // Goto definition from a call on line 2 should point to that position.
        let source = "fn f() { 1 }\nfn add(a, b) { a + b }\nadd(1, 2);";
        let state = setup_server_with_source(source);
        // Resolve `add` on line 2, col 0 (0-based)
        let resolved = ServerState::resolve_symbol(
            &state,
            &test_uri(),
            lsp_types::Position {
                line: 2,
                character: 0,
            },
        )
        .expect("should resolve `add` call");

        let goto = resolved.to_goto_definition(&test_uri()).unwrap();
        match goto {
            GotoDefinitionResponse::Scalar(loc) => {
                // Definition is `fn add(...)` on line 1, `add` starts at 0-based col 3.
                assert_eq!(loc.range.start.line, 1, "definition should be on line 1");
                assert_eq!(
                    loc.range.start.character, 3,
                    "definition column should be 0-based (3 = position of 'a' in 'add')"
                );
            }
            _ => panic!("expected scalar location"),
        }
    }

    #[test]
    fn goto_definition_falls_back_to_member_resolution_for_dot_methods() {
        let source = "struct Vector { x: i64 }\nfn Vector.add(self, other: Vector) -> Vector { self }\nlet v = Vector { x: 1 };\nv.add(v);";
        let mut state = setup_server_with_source(source);
        let uri = test_uri();
        let pos = lsp_types::Position {
            line: 3,
            character: 2,
        };

        let member = ServerState::resolve_member(&mut state, &uri, pos)
            .expect("member resolution should find `v.add`");
        let goto = member_to_goto_definition(&member, &uri)
            .expect("goto should use member definition span");

        match goto {
            GotoDefinitionResponse::Scalar(loc) => {
                assert_eq!(loc.range.start.line, 1);
                assert_eq!(loc.range.start.character, 3);
            }
            _ => panic!("expected scalar location"),
        }
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

    #[test]
    fn signature_help_returns_user_defined_function_signature() {
        let mut state =
            setup_server_with_source("fn add(a: i64, b: i64) -> i64 { a + b }\nlet _ = add(1, 2);");

        let help = ServerState::collect_signature_help(
            &mut state,
            &test_uri(),
            lsp_types::Position {
                line: 1,
                character: 15,
            },
        )
        .expect("signature help should be available");

        assert_eq!(help.active_parameter, Some(1));
        assert_eq!(help.signatures[0].label, "add(a: i64, b: i64) -> i64");
    }

    #[test]
    fn signature_help_omits_self_for_dot_methods() {
        let mut state = setup_server_with_source(
            "struct Vector { x: i64 }\nfn Vector.add(self, other: Vector) -> Vector { self }\nlet v1 = Vector { x: 1 };\nlet v2 = Vector { x: 2 };\nlet _ = v1.add(v2);",
        );

        let help = ServerState::collect_signature_help(
            &mut state,
            &test_uri(),
            lsp_types::Position {
                line: 4,
                character: 16,
            },
        )
        .expect("signature help should be available");

        assert_eq!(
            help.signatures[0].label,
            "Vector.add(other: Vector) -> Vector"
        );
        assert_eq!(
            help.signatures[0].parameters.as_ref().map(Vec::len),
            Some(1)
        );
    }

    // ── Dot completion tests ───────────────────────────────────────────

    #[test]
    fn extract_receiver_simple() {
        let source = "let v1 = Vector::new(1.0, 2.0)\nv1.";
        let pos = lsp_types::Position {
            line: 1,
            character: 3,
        };
        assert_eq!(
            extract_receiver_before_dot(source, pos),
            Some("v1".to_string())
        );
    }

    #[test]
    fn extract_receiver_no_dot() {
        let source = "let v1 = 42";
        let pos = lsp_types::Position {
            line: 0,
            character: 0,
        };
        assert_eq!(extract_receiver_before_dot(source, pos), None);
    }

    #[test]
    fn extract_receiver_at_col_zero() {
        let source = ".foo";
        let pos = lsp_types::Position {
            line: 0,
            character: 0,
        };
        assert_eq!(extract_receiver_before_dot(source, pos), None);
    }

    // ── Inlay hint tests ───────────────────────────────────────────────

    #[test]
    fn inlay_hint_returns_type_hints_for_let_binding() {
        let mut state = setup_server_with_source("let x = 42;");
        ServerState::ensure_typed_hir(&mut state, &test_uri());

        let doc = state.store.get(&test_uri()).unwrap();
        assert!(doc.typed_hir.is_some(), "typed HIR should be cached");

        let typed_hir = doc.typed_hir.as_ref().unwrap().as_ref().unwrap();
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
        let typed_hir = doc.typed_hir.as_ref().unwrap().as_ref().unwrap();
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
        let typed_hir = doc.typed_hir.as_ref().unwrap().as_ref().unwrap();
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
        let source = "let x = 42;";
        let hint = tlang_analysis::inlay_hints::InlayHint {
            line: 0,
            character: 5,
            label: ": i64".into(),
            kind: tlang_analysis::inlay_hints::InlayHintKind::Type,
        };
        let lsp_hint = ServerState::to_lsp_inlay_hint(&hint, source);
        assert_eq!(lsp_hint.kind, Some(lsp_types::InlayHintKind::TYPE));
        assert_eq!(lsp_hint.position.line, 0);
        assert_eq!(lsp_hint.position.character, 5);
        match &lsp_hint.label {
            lsp_types::InlayHintLabel::String(s) => assert_eq!(s, ": i64"),
            _ => panic!("expected string label"),
        }
    }

    #[test]
    fn inlay_hint_return_type_has_no_padding() {
        let source = "fn f() { 1 }";
        let hint = tlang_analysis::inlay_hints::InlayHint {
            line: 0,
            character: 7,
            label: "-> i64 ".into(),
            kind: tlang_analysis::inlay_hints::InlayHintKind::ReturnType,
        };
        let lsp_hint = ServerState::to_lsp_inlay_hint(&hint, source);
        assert_eq!(lsp_hint.padding_left, None);
        assert_eq!(lsp_hint.padding_right, None);
    }

    #[test]
    fn inlay_hint_type_has_no_padding() {
        let source = "let x = 42;";
        let hint = tlang_analysis::inlay_hints::InlayHint {
            line: 0,
            character: 5,
            label: ": i64".into(),
            kind: tlang_analysis::inlay_hints::InlayHintKind::Type,
        };
        let lsp_hint = ServerState::to_lsp_inlay_hint(&hint, source);
        assert_eq!(lsp_hint.padding_left, None);
        assert_eq!(lsp_hint.padding_right, None);
    }

    #[test]
    fn inlay_hint_cache_invalidated_on_source_change() {
        let client = async_lsp::ClientSocket::new_closed();
        let mut state = ServerState {
            client,
            store: DocumentStore::new(),
            target: CompilationTarget::Js,
            progress_supported: false,
            next_progress_token: 0,
            startup_progress_reported: false,
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

    #[test]
    fn inlay_hint_utf16_conversion_for_ascii() {
        // For ASCII text, char columns and UTF-16 columns are the same.
        assert_eq!(char_column_to_utf16("let x = 42;", 0, 5), 5);
    }

    #[test]
    fn inlay_hint_utf16_conversion_multiline() {
        // Line 1 column 3 in "abc\ndef" is 'd', 'e', 'f' → UTF-16 offset 3.
        assert_eq!(char_column_to_utf16("abc\ndef", 1, 3), 3);
    }

    #[test]
    fn inlay_hint_utf16_conversion_non_bmp() {
        // '😀' is U+1F600 — a non-BMP character that takes 2 UTF-16 code units.
        // Source: "😀x = 1;" — column 1 (the 'x') should be UTF-16 offset 2.
        let source = "😀x = 1;";
        assert_eq!(char_column_to_utf16(source, 0, 1), 2);
    }

    #[test]
    fn inlay_hint_range_end_exclusive_at_line_boundary() {
        // When the client sends end = { line: 1, character: 0 }, it means
        // "up to but not including line 1".  We should only see hints on
        // line 0, not line 1.
        let source = "let a = 1;\nlet b = 2;\nlet c = 3;";
        let mut state = setup_server_with_source(source);
        ServerState::ensure_typed_hir(&mut state, &test_uri());

        let doc = state.store.get(&test_uri()).unwrap();
        let typed_hir = doc.typed_hir.as_ref().unwrap().as_ref().unwrap();
        let all_hints = tlang_analysis::inlay_hints::collect_inlay_hints(typed_hir, None);
        // Verify there are hints on line 1 in the full set.
        assert!(
            all_hints.iter().any(|h| h.line == 1),
            "expected hints on line 1, got: {all_hints:?}"
        );

        // Request range { start: { line: 0, char: 0 }, end: { line: 1, char: 0 } }
        // This is exclusive — should only return hints on line 0.
        let range = lsp_types::Range {
            start: lsp_types::Position {
                line: 0,
                character: 0,
            },
            end: lsp_types::Position {
                line: 1,
                character: 0,
            },
        };
        let end_line = if range.end.character == 0 && range.end.line > range.start.line {
            range.end.line.saturating_sub(1)
        } else {
            range.end.line
        };
        let line_range = Some((range.start.line, end_line));
        let filtered = tlang_analysis::inlay_hints::collect_inlay_hints(typed_hir, line_range);
        assert!(
            filtered.iter().all(|h| h.line < range.end.line),
            "all hints should be before end line {}, got: {filtered:?}",
            range.end.line
        );
    }

    #[test]
    fn inlay_hint_pipeline_chain_shows_intermediate_types() {
        // Multi-line pipeline: each `|>` step on its own line should produce
        // a ChainedPipeline hint showing the type flowing into that step.
        let source = "fn double(x: i64) -> i64 { x * 2 }\nfn inc(x: i64) -> i64 { x + 1 }\n42\n|> double()\n|> inc();";
        let mut state = setup_server_with_source(source);
        ServerState::ensure_typed_hir(&mut state, &test_uri());

        let doc = state.store.get(&test_uri()).unwrap();
        let typed_hir = doc.typed_hir.as_ref().unwrap().as_ref().unwrap();
        let hints = tlang_analysis::inlay_hints::collect_inlay_hints(typed_hir, None);

        let chain_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == tlang_analysis::inlay_hints::InlayHintKind::ChainedPipeline)
            .collect();
        assert!(
            !chain_hints.is_empty(),
            "expected ChainedPipeline hints for multi-line pipeline, got: {hints:?}"
        );
        assert!(
            chain_hints.iter().all(|h| h.label.starts_with(": ")),
            "chain hints should have ': <type>' label, got: {chain_hints:?}"
        );
    }

    #[test]
    fn inlay_hint_pipeline_no_chain_hint_for_single_line() {
        // A single-line pipeline should NOT produce a ChainedPipeline hint.
        let source = "fn double(x: i64) -> i64 { x * 2 }\n42 |> double();";
        let mut state = setup_server_with_source(source);
        ServerState::ensure_typed_hir(&mut state, &test_uri());

        let doc = state.store.get(&test_uri()).unwrap();
        let typed_hir = doc.typed_hir.as_ref().unwrap().as_ref().unwrap();
        let hints = tlang_analysis::inlay_hints::collect_inlay_hints(typed_hir, None);

        assert!(
            !hints
                .iter()
                .any(|h| h.kind == tlang_analysis::inlay_hints::InlayHintKind::ChainedPipeline),
            "should not show chain hints for single-line pipeline, got: {hints:?}"
        );
    }
}
