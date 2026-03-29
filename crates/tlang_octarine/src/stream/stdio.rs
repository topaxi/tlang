//! Standard I/O streams (stdin, stdout, stderr).

use std::io::{self, Write};

use super::{FlushOp, ReadOp, ReadableStream, StreamOp, WritableStream, WriteOp};

// ---------------------------------------------------------------------------
// Stdin
// ---------------------------------------------------------------------------

/// Readable stream backed by the process's standard input.
///
/// Reads are dispatched to the tokio IO worker because `std::io::Stdin` blocks
/// the calling thread.
pub struct StdinStream {
    open: bool,
}

impl StdinStream {
    pub fn new() -> Self {
        Self { open: true }
    }
}

impl ReadableStream for StdinStream {
    fn read(&mut self, size: usize) -> ReadOp {
        if !self.open {
            return StreamOp::Closed;
        }
        StreamOp::Async(Box::pin(async move {
            use tokio::io::AsyncReadExt;
            let mut buf = vec![0u8; size];
            let n = tokio::io::stdin().read(&mut buf).await?;
            buf.truncate(n);
            Ok(buf)
        }))
    }

    fn close_read(&mut self) {
        self.open = false;
    }

    fn is_readable(&self) -> bool {
        self.open
    }
}

// ---------------------------------------------------------------------------
// Stdout
// ---------------------------------------------------------------------------

/// Writable stream backed by the process's standard output.
///
/// Writes complete synchronously on the main thread.
pub struct StdoutStream {
    open: bool,
}

impl StdoutStream {
    pub fn new() -> Self {
        Self { open: true }
    }
}

impl WritableStream for StdoutStream {
    fn write(&mut self, data: &[u8]) -> WriteOp {
        if !self.open {
            return StreamOp::Closed;
        }
        StreamOp::Ready(io::stdout().write(data))
    }

    fn flush(&mut self) -> FlushOp {
        if !self.open {
            return StreamOp::Closed;
        }
        StreamOp::Ready(io::stdout().flush())
    }

    fn close_write(&mut self) {
        self.open = false;
    }

    fn is_writable(&self) -> bool {
        self.open
    }
}

// ---------------------------------------------------------------------------
// Stderr
// ---------------------------------------------------------------------------

/// Writable stream backed by the process's standard error.
///
/// Writes complete synchronously on the main thread.
pub struct StderrStream {
    open: bool,
}

impl StderrStream {
    pub fn new() -> Self {
        Self { open: true }
    }
}

impl WritableStream for StderrStream {
    fn write(&mut self, data: &[u8]) -> WriteOp {
        if !self.open {
            return StreamOp::Closed;
        }
        StreamOp::Ready(io::stderr().write(data))
    }

    fn flush(&mut self) -> FlushOp {
        if !self.open {
            return StreamOp::Closed;
        }
        StreamOp::Ready(io::stderr().flush())
    }

    fn close_write(&mut self) {
        self.open = false;
    }

    fn is_writable(&self) -> bool {
        self.open
    }
}
