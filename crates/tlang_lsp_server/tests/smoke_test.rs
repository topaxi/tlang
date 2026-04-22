use std::ops::ControlFlow;

use async_lsp::LanguageServer;
use async_lsp::router::Router;
use async_lsp::server::LifecycleLayer;
use futures::{AsyncReadExt, StreamExt};
use lsp_types::{
    ClientCapabilities, DidOpenTextDocumentParams, DidSaveTextDocumentParams, InitializeParams,
    InitializedParams, ProgressParams, PublishDiagnosticsParams, TextDocumentIdentifier,
    TextDocumentItem, Url, WindowClientCapabilities, WorkDoneProgress,
    WorkDoneProgressCreateParams, notification, request,
};
use tlang_lsp_server::server::ServerState;
use tokio_util::compat::TokioAsyncReadCompatExt;
use tower::ServiceBuilder;

const MEMORY_CHANNEL_SIZE: usize = 64 << 10;

struct ClientState {
    create_tx: futures::channel::mpsc::UnboundedSender<WorkDoneProgressCreateParams>,
    progress_tx: futures::channel::mpsc::UnboundedSender<ProgressParams>,
    diag_tx: futures::channel::mpsc::UnboundedSender<PublishDiagnosticsParams>,
}

#[tokio::test(flavor = "current_thread")]
async fn smoke_test_diagnostics_on_open() {
    // Set up the server.
    let (server_main, _client) = async_lsp::MainLoop::new_server(|client| {
        ServiceBuilder::new()
            .layer(LifecycleLayer::default())
            .service(ServerState::new_router(client))
    });

    // Set up a mock client that captures publishDiagnostics notifications.
    let (create_tx, _create_rx) = futures::channel::mpsc::unbounded();
    let (progress_tx, _progress_rx) = futures::channel::mpsc::unbounded();
    let (diag_tx, mut diag_rx) = futures::channel::mpsc::unbounded();
    let (client_main, mut server) = async_lsp::MainLoop::new_client(|_server| {
        let mut router = Router::new(ClientState {
            create_tx: create_tx.clone(),
            progress_tx: progress_tx.clone(),
            diag_tx: diag_tx.clone(),
        });
        router.request::<request::WorkDoneProgressCreate, _>(|st, params| {
            st.create_tx.unbounded_send(params).unwrap();
            Box::pin(async { Ok(()) })
        });
        router.notification::<notification::Progress>(|st, params| {
            st.progress_tx.unbounded_send(params).unwrap();
            ControlFlow::Continue(())
        });
        router.notification::<notification::PublishDiagnostics>(|st, params| {
            st.diag_tx.unbounded_send(params).unwrap();
            ControlFlow::Continue(())
        });
        ServiceBuilder::new().service(router)
    });

    // Wire up a loopback channel.
    let (server_stream, client_stream) = tokio::io::duplex(MEMORY_CHANNEL_SIZE);
    let (server_rx, server_tx) = server_stream.compat().split();
    let server_main = tokio::spawn(async move {
        server_main
            .run_buffered(server_rx, server_tx)
            .await
            .unwrap();
    });
    let (client_rx, client_tx) = client_stream.compat().split();
    let client_main = tokio::spawn(async move {
        let err = client_main
            .run_buffered(client_rx, client_tx)
            .await
            .unwrap_err();
        assert!(
            matches!(err, async_lsp::Error::Eof),
            "should fail due to EOF: {err}"
        );
    });

    // Initialize the server.
    let init_result = server
        .initialize(InitializeParams::default())
        .await
        .unwrap();
    assert!(init_result.server_info.is_some());
    assert_eq!(
        init_result.server_info.as_ref().unwrap().name,
        "tlang-lsp-server"
    );
    server.initialized(InitializedParams {}).unwrap();

    // Open a document with a syntax error.
    let uri = Url::parse("file:///test/bad.tlang").unwrap();
    server
        .notify::<notification::DidOpenTextDocument>(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.clone(),
                language_id: "tlang".into(),
                version: 1,
                text: "fn foo( {".into(), // intentional syntax error
            },
        })
        .unwrap();

    // Wait for publishDiagnostics notification.
    let diag_params = diag_rx.next().await.unwrap();
    assert_eq!(diag_params.uri, uri);
    assert!(
        !diag_params.diagnostics.is_empty(),
        "expected at least one diagnostic for syntax error, got: {:?}",
        diag_params.diagnostics
    );
    assert_eq!(
        diag_params.diagnostics[0].severity,
        Some(lsp_types::DiagnosticSeverity::ERROR)
    );

    // Open a valid document — should get empty diagnostics (or only warnings).
    let valid_uri = Url::parse("file:///test/good.tlang").unwrap();
    server
        .notify::<notification::DidOpenTextDocument>(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: valid_uri.clone(),
                language_id: "tlang".into(),
                version: 1,
                text: "fn add(a, b) { a + b }".into(),
            },
        })
        .unwrap();

    let valid_diag_params = diag_rx.next().await.unwrap();
    assert_eq!(valid_diag_params.uri, valid_uri);
    let errors: Vec<_> = valid_diag_params
        .diagnostics
        .iter()
        .filter(|d| d.severity == Some(lsp_types::DiagnosticSeverity::ERROR))
        .collect();
    assert!(
        errors.is_empty(),
        "expected no error diagnostics for valid code, got: {errors:?}"
    );

    // Save the valid document — this should not crash the server and should
    // republish diagnostics for the current document contents.
    server
        .notify::<notification::DidSaveTextDocument>(DidSaveTextDocumentParams {
            text_document: TextDocumentIdentifier {
                uri: valid_uri.clone(),
            },
            text: None,
        })
        .unwrap();

    let saved_diag_params = diag_rx.next().await.unwrap();
    assert_eq!(saved_diag_params.uri, valid_uri);
    let save_errors: Vec<_> = saved_diag_params
        .diagnostics
        .iter()
        .filter(|d| d.severity == Some(lsp_types::DiagnosticSeverity::ERROR))
        .collect();
    assert!(
        save_errors.is_empty(),
        "expected no error diagnostics after save, got: {save_errors:?}"
    );

    // Close the bad document — should get empty diagnostics to clear.
    server
        .notify::<notification::DidCloseTextDocument>(lsp_types::DidCloseTextDocumentParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
        })
        .unwrap();

    let close_diag_params = diag_rx.next().await.unwrap();
    assert_eq!(close_diag_params.uri, uri);
    assert!(
        close_diag_params.diagnostics.is_empty(),
        "expected empty diagnostics on close"
    );

    // Shutdown.
    server.shutdown(()).await.unwrap();
    server.exit(()).unwrap();

    server_main.await.expect("server main loop panicked");
    client_main.await.expect("client main loop panicked");
}

#[tokio::test(flavor = "current_thread")]
async fn smoke_test_reports_startup_and_analysis_progress() {
    let (server_main, _client) = async_lsp::MainLoop::new_server(|client| {
        ServiceBuilder::new()
            .layer(LifecycleLayer::default())
            .service(ServerState::new_router(client))
    });

    let (create_tx, mut create_rx) = futures::channel::mpsc::unbounded();
    let (progress_tx, mut progress_rx) = futures::channel::mpsc::unbounded();
    let (diag_tx, mut diag_rx) = futures::channel::mpsc::unbounded();
    let (client_main, mut server) = async_lsp::MainLoop::new_client(|_server| {
        let mut router = Router::new(ClientState {
            create_tx: create_tx.clone(),
            progress_tx: progress_tx.clone(),
            diag_tx: diag_tx.clone(),
        });
        router.request::<request::WorkDoneProgressCreate, _>(|st, params| {
            st.create_tx.unbounded_send(params).unwrap();
            Box::pin(async { Ok(()) })
        });
        router.notification::<notification::Progress>(|st, params| {
            st.progress_tx.unbounded_send(params).unwrap();
            ControlFlow::Continue(())
        });
        router.notification::<notification::PublishDiagnostics>(|st, params| {
            st.diag_tx.unbounded_send(params).unwrap();
            ControlFlow::Continue(())
        });
        ServiceBuilder::new().service(router)
    });

    let (server_stream, client_stream) = tokio::io::duplex(MEMORY_CHANNEL_SIZE);
    let (server_rx, server_tx) = server_stream.compat().split();
    let server_main = tokio::spawn(async move {
        server_main
            .run_buffered(server_rx, server_tx)
            .await
            .unwrap();
    });
    let (client_rx, client_tx) = client_stream.compat().split();
    let client_main = tokio::spawn(async move {
        let err = client_main
            .run_buffered(client_rx, client_tx)
            .await
            .unwrap_err();
        assert!(
            matches!(err, async_lsp::Error::Eof),
            "should fail due to EOF: {err}"
        );
    });

    let init_result = server
        .initialize(InitializeParams {
            capabilities: ClientCapabilities {
                window: Some(WindowClientCapabilities {
                    work_done_progress: Some(true),
                    ..WindowClientCapabilities::default()
                }),
                ..ClientCapabilities::default()
            },
            ..InitializeParams::default()
        })
        .await
        .unwrap();
    assert!(init_result.server_info.is_some());

    server.initialized(InitializedParams {}).unwrap();

    let startup_create = create_rx.next().await.unwrap();
    let startup_begin = progress_rx.next().await.unwrap();
    let startup_report = progress_rx.next().await.unwrap();
    let startup_end = progress_rx.next().await.unwrap();

    assert_eq!(startup_create.token, startup_begin.token);
    assert_eq!(startup_begin.token, startup_report.token);
    assert_eq!(startup_report.token, startup_end.token);

    match startup_begin.value {
        lsp_types::ProgressParamsValue::WorkDone(WorkDoneProgress::Begin(begin)) => {
            assert_eq!(begin.title, "Starting tlang language server");
            assert_eq!(
                begin.message.as_deref(),
                Some("Preparing JavaScript analysis pipeline")
            );
        }
        other => panic!("expected startup begin progress, got: {other:?}"),
    }

    match startup_report.value {
        lsp_types::ProgressParamsValue::WorkDone(WorkDoneProgress::Report(report)) => {
            assert_eq!(
                report.message.as_deref(),
                Some("Ready to analyze documents")
            );
            assert_eq!(report.percentage, Some(100));
        }
        other => panic!("expected startup report progress, got: {other:?}"),
    }

    match startup_end.value {
        lsp_types::ProgressParamsValue::WorkDone(WorkDoneProgress::End(end)) => {
            assert_eq!(end.message.as_deref(), Some("Language server ready"));
        }
        other => panic!("expected startup end progress, got: {other:?}"),
    }

    let uri = Url::parse("file:///test/progress.tlang").unwrap();
    server
        .notify::<notification::DidOpenTextDocument>(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.clone(),
                language_id: "tlang".into(),
                version: 1,
                text: "fn add(a, b) { a + b }".into(),
            },
        })
        .unwrap();

    let analysis_create = create_rx.next().await.unwrap();
    let analysis_begin = progress_rx.next().await.unwrap();
    let analysis_indexing = progress_rx.next().await.unwrap();
    let analysis_typecheck = progress_rx.next().await.unwrap();
    let analysis_publish = progress_rx.next().await.unwrap();
    let analysis_end = progress_rx.next().await.unwrap();

    assert_eq!(analysis_create.token, analysis_begin.token);
    assert_eq!(analysis_begin.token, analysis_indexing.token);
    assert_eq!(analysis_indexing.token, analysis_typecheck.token);
    assert_eq!(analysis_typecheck.token, analysis_publish.token);
    assert_eq!(analysis_publish.token, analysis_end.token);

    match analysis_begin.value {
        lsp_types::ProgressParamsValue::WorkDone(WorkDoneProgress::Begin(begin)) => {
            assert_eq!(begin.title, "Analyzing progress.tlang");
            assert_eq!(
                begin.message.as_deref(),
                Some("Parsing and semantic analysis")
            );
            assert_eq!(begin.percentage, Some(10));
        }
        other => panic!("expected analysis begin progress, got: {other:?}"),
    }

    match analysis_indexing.value {
        lsp_types::ProgressParamsValue::WorkDone(WorkDoneProgress::Report(report)) => {
            assert_eq!(report.message.as_deref(), Some("Indexing symbols"));
            assert_eq!(report.percentage, Some(55));
        }
        other => panic!("expected indexing progress, got: {other:?}"),
    }

    match analysis_typecheck.value {
        lsp_types::ProgressParamsValue::WorkDone(WorkDoneProgress::Report(report)) => {
            assert_eq!(report.message.as_deref(), Some("Type checking"));
            assert_eq!(report.percentage, Some(80));
        }
        other => panic!("expected type checking progress, got: {other:?}"),
    }

    match analysis_publish.value {
        lsp_types::ProgressParamsValue::WorkDone(WorkDoneProgress::Report(report)) => {
            assert_eq!(report.message.as_deref(), Some("Publishing diagnostics"));
            assert_eq!(report.percentage, Some(95));
        }
        other => panic!("expected diagnostics publish progress, got: {other:?}"),
    }

    match analysis_end.value {
        lsp_types::ProgressParamsValue::WorkDone(WorkDoneProgress::End(end)) => {
            assert_eq!(end.message.as_deref(), Some("Analysis complete"));
        }
        other => panic!("expected analysis end progress, got: {other:?}"),
    }

    let diag_params = diag_rx.next().await.unwrap();
    assert_eq!(diag_params.uri, uri);
    assert!(
        diag_params
            .diagnostics
            .iter()
            .all(|d| d.severity != Some(lsp_types::DiagnosticSeverity::ERROR)),
        "expected no error diagnostics, got: {:?}",
        diag_params.diagnostics
    );

    server.shutdown(()).await.unwrap();
    server.exit(()).unwrap();

    server_main.await.expect("server main loop panicked");
    client_main.await.expect("client main loop panicked");
}
