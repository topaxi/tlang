use std::ops::ControlFlow;

use async_lsp::LanguageServer;
use async_lsp::router::Router;
use async_lsp::server::LifecycleLayer;
use futures::{AsyncReadExt, StreamExt};
use lsp_types::{
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, InitializeParams, InitializedParams,
    PublishDiagnosticsParams, TextDocumentIdentifier, TextDocumentItem, Url, notification,
};
use tlang_lsp_server::server::ServerState;
use tokio_util::compat::TokioAsyncReadCompatExt;
use tower::ServiceBuilder;

const MEMORY_CHANNEL_SIZE: usize = 64 << 10;

struct ClientState {
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
    let (diag_tx, mut diag_rx) = futures::channel::mpsc::unbounded();
    let (client_main, mut server) = async_lsp::MainLoop::new_client(|_server| {
        let mut router = Router::new(ClientState {
            diag_tx: diag_tx.clone(),
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
