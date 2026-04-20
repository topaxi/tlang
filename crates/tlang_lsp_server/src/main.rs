use async_lsp::concurrency::ConcurrencyLayer;
use async_lsp::panic::CatchUnwindLayer;
use async_lsp::server::LifecycleLayer;
use async_lsp::tracing::TracingLayer;
use std::process::ExitCode;
use tlang_lsp_server::server::ServerState;
use tower::ServiceBuilder;
use tracing::{Level, error};

#[tokio::main(flavor = "current_thread")]
async fn main() -> ExitCode {
    let (server, _) = async_lsp::MainLoop::new_server(|client| {
        ServiceBuilder::new()
            .layer(TracingLayer::default())
            .layer(LifecycleLayer::default())
            .layer(CatchUnwindLayer::default())
            .layer(ConcurrencyLayer::default())
            .service(ServerState::new_router(client))
    });

    tracing_subscriber::fmt()
        .with_max_level(Level::INFO)
        .with_ansi(false)
        .with_writer(std::io::stderr)
        .init();

    // Prefer truly asynchronous piped stdin/stdout without blocking tasks.
    #[cfg(unix)]
    let (stdin, stdout) = match (
        async_lsp::stdio::PipeStdin::lock_tokio(),
        async_lsp::stdio::PipeStdout::lock_tokio(),
    ) {
        (Ok(stdin), Ok(stdout)) => (stdin, stdout),
        (Err(err), _) => {
            error!("failed to lock stdin: {err}");
            return ExitCode::FAILURE;
        }
        (_, Err(err)) => {
            error!("failed to lock stdout: {err}");
            return ExitCode::FAILURE;
        }
    };
    #[cfg(not(unix))]
    let (stdin, stdout) = (
        tokio_util::compat::TokioAsyncReadCompatExt::compat(tokio::io::stdin()),
        tokio_util::compat::TokioAsyncWriteCompatExt::compat_write(tokio::io::stdout()),
    );

    match server.run_buffered(stdin, stdout).await {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            error!("LSP server exited with error: {err}");
            ExitCode::FAILURE
        }
    }
}
