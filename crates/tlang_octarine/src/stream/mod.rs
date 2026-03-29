//! Stream protocol for the octarine runtime.
//!
//! Streams are the primary abstraction for I/O in octarine. Every I/O source
//! (stdin, files, TCP sockets, in-memory buffers) implements the
//! [`ReadableStream`] and/or [`WritableStream`] traits.
//!
//! Stream operations return a [`StreamOp`] that tells the [`Runtime`] how to
//! complete the operation:
//!
//! - **`Ready`** — the operation completed synchronously (e.g. stdout write).
//! - **`Async`** — the operation must run on the tokio IO worker thread
//!   (e.g. file read, stdin read).
//! - **`WouldBlock`** — the underlying mio source isn't ready yet; the runtime
//!   stores the pending operation and retries when the token fires
//!   (e.g. TCP socket).
//! - **`Closed`** — the stream has been shut down.
//!
//! [`Runtime`]: crate::runtime::Runtime

pub mod buffer;
pub mod file;
pub mod native;
pub mod stdio;
pub mod tcp;

use std::collections::HashMap;
use std::future::Future;
use std::io;
use std::pin::Pin;

pub use buffer::BufferStream;
pub use file::{FileReadStream, FileWriteStream};
pub use stdio::{StderrStream, StdinStream, StdoutStream};
pub use tcp::TcpStreamHandle;

// ---------------------------------------------------------------------------
// StreamId
// ---------------------------------------------------------------------------

/// Opaque handle to an open stream.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StreamId(u64);

/// Standard input (pre-registered).
pub const STDIN: StreamId = StreamId(0);
/// Standard output (pre-registered).
pub const STDOUT: StreamId = StreamId(1);
/// Standard error (pre-registered).
pub const STDERR: StreamId = StreamId(2);

// ---------------------------------------------------------------------------
// StreamOp — action returned by stream trait methods
// ---------------------------------------------------------------------------

/// Describes how a stream operation should be completed.
pub enum StreamOp<T: Send + 'static> {
    /// The operation completed immediately.
    Ready(io::Result<T>),
    /// The operation must be executed asynchronously on the IO worker.
    Async(Pin<Box<dyn Future<Output = io::Result<T>> + Send>>),
    /// The mio source would block — retry when the given token is ready.
    WouldBlock(mio::Token),
    /// The stream is closed.
    Closed,
}

/// A read operation that produces bytes.
pub type ReadOp = StreamOp<Vec<u8>>;
/// A write operation that reports bytes written.
pub type WriteOp = StreamOp<usize>;
/// A flush operation.
pub type FlushOp = StreamOp<()>;

// ---------------------------------------------------------------------------
// Protocol traits
// ---------------------------------------------------------------------------

/// Protocol for streams that produce data.
pub trait ReadableStream {
    /// Attempt to read up to `size` bytes.
    fn read(&mut self, size: usize) -> ReadOp;

    /// Close the readable end of the stream.
    fn close_read(&mut self);

    /// Whether the stream is still open for reading.
    fn is_readable(&self) -> bool;
}

/// Protocol for streams that consume data.
pub trait WritableStream {
    /// Attempt to write `data`.
    fn write(&mut self, data: &[u8]) -> WriteOp;

    /// Flush any internally buffered data.
    fn flush(&mut self) -> FlushOp;

    /// Close the writable end of the stream.
    fn close_write(&mut self);

    /// Whether the stream is still open for writing.
    fn is_writable(&self) -> bool;
}

// ---------------------------------------------------------------------------
// StreamKind — concrete storage for all stream flavours
// ---------------------------------------------------------------------------

/// All supported stream types behind a single enum for registry storage.
pub enum StreamKind {
    Stdin(StdinStream),
    Stdout(StdoutStream),
    Stderr(StderrStream),
    FileRead(FileReadStream),
    FileWrite(FileWriteStream),
    Tcp(TcpStreamHandle),
    Buffer(BufferStream),
}

impl StreamKind {
    pub fn read(&mut self, size: usize) -> Option<ReadOp> {
        match self {
            Self::Stdin(s) => Some(s.read(size)),
            Self::FileRead(s) => Some(s.read(size)),
            Self::Tcp(s) => Some(s.read(size)),
            Self::Buffer(s) => Some(s.read(size)),
            _ => None,
        }
    }

    pub fn write(&mut self, data: &[u8]) -> Option<WriteOp> {
        match self {
            Self::Stdout(s) => Some(s.write(data)),
            Self::Stderr(s) => Some(s.write(data)),
            Self::FileWrite(s) => Some(s.write(data)),
            Self::Tcp(s) => Some(s.write(data)),
            Self::Buffer(s) => Some(s.write(data)),
            _ => None,
        }
    }

    pub fn flush(&mut self) -> Option<FlushOp> {
        match self {
            Self::Stdout(s) => Some(s.flush()),
            Self::Stderr(s) => Some(s.flush()),
            Self::FileWrite(s) => Some(s.flush()),
            Self::Tcp(s) => Some(s.flush()),
            Self::Buffer(s) => Some(s.flush()),
            _ => None,
        }
    }

    pub fn close(&mut self) {
        match self {
            Self::Stdin(s) => s.close_read(),
            Self::Stdout(s) => s.close_write(),
            Self::Stderr(s) => s.close_write(),
            Self::FileRead(s) => s.close_read(),
            Self::FileWrite(s) => s.close_write(),
            Self::Tcp(s) => {
                s.close_read();
                s.close_write();
            }
            Self::Buffer(s) => {
                s.close_read();
                s.close_write();
            }
        }
    }

    pub fn is_readable(&self) -> bool {
        match self {
            Self::Stdin(s) => s.is_readable(),
            Self::FileRead(s) => s.is_readable(),
            Self::Tcp(s) => s.is_readable(),
            Self::Buffer(s) => s.is_readable(),
            _ => false,
        }
    }

    pub fn is_writable(&self) -> bool {
        match self {
            Self::Stdout(s) => s.is_writable(),
            Self::Stderr(s) => s.is_writable(),
            Self::FileWrite(s) => s.is_writable(),
            Self::Tcp(s) => s.is_writable(),
            Self::Buffer(s) => s.is_writable(),
            _ => false,
        }
    }
}

// ---------------------------------------------------------------------------
// StreamRegistry
// ---------------------------------------------------------------------------

/// Central registry of all open streams, owned by the [`Runtime`].
///
/// Stdin, stdout, and stderr are pre-registered at construction time with
/// well-known [`StreamId`]s.
pub struct StreamRegistry {
    streams: HashMap<StreamId, StreamKind>,
    next_id: u64,
}

impl Default for StreamRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl StreamRegistry {
    pub fn new() -> Self {
        let mut registry = Self {
            streams: HashMap::new(),
            // 0–2 are reserved for stdio.
            next_id: 3,
        };
        registry
            .streams
            .insert(STDIN, StreamKind::Stdin(StdinStream::new()));
        registry
            .streams
            .insert(STDOUT, StreamKind::Stdout(StdoutStream::new()));
        registry
            .streams
            .insert(STDERR, StreamKind::Stderr(StderrStream::new()));
        registry
    }

    /// Register a new stream and return its [`StreamId`].
    pub fn register(&mut self, kind: StreamKind) -> StreamId {
        let id = StreamId(self.next_id);
        self.next_id += 1;
        self.streams.insert(id, kind);
        id
    }

    /// Look up a stream by id.
    pub fn get_mut(&mut self, id: StreamId) -> Option<&mut StreamKind> {
        self.streams.get_mut(&id)
    }

    /// Remove and return a stream.
    pub fn remove(&mut self, id: StreamId) -> Option<StreamKind> {
        self.streams.remove(&id)
    }

    /// Attempt a read on the given stream.
    pub fn read(&mut self, id: StreamId, size: usize) -> Option<ReadOp> {
        self.get_mut(id)?.read(size)
    }

    /// Attempt a write on the given stream.
    pub fn write(&mut self, id: StreamId, data: &[u8]) -> Option<WriteOp> {
        self.get_mut(id)?.write(data)
    }

    /// Attempt a flush on the given stream.
    pub fn flush(&mut self, id: StreamId) -> Option<FlushOp> {
        self.get_mut(id)?.flush()
    }

    /// Close a stream.
    pub fn close(&mut self, id: StreamId) {
        if let Some(stream) = self.get_mut(id) {
            stream.close();
        }
    }
}
