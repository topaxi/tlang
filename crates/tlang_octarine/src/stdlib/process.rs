//! Child process spawning.
//!
//! Process execution is offloaded to the IO worker (tokio thread) so the main
//! event loop stays responsive while waiting for the child to exit.

use std::io;
use std::path::PathBuf;

use crate::runtime::Runtime;

/// Captured output of a finished child process.
#[derive(Debug)]
pub struct Output {
    pub status: i32,
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>,
}

/// Spawn a child process and collect its output asynchronously.
pub fn spawn(
    runtime: &mut Runtime,
    program: PathBuf,
    args: Vec<String>,
    callback: impl FnOnce(io::Result<Output>, &mut Runtime) + 'static,
) {
    runtime.spawn_io(
        async move {
            tokio::process::Command::new(program)
                .args(args)
                .output()
                .await
                .map(|o| Output {
                    status: o.status.code().unwrap_or(-1),
                    stdout: o.stdout,
                    stderr: o.stderr,
                })
        },
        callback,
    );
}
