//! File-backed streams.
//!
//! All operations are dispatched to the tokio IO worker thread since standard
//! file I/O is blocking on most platforms.

use std::path::PathBuf;

use super::{FlushOp, ReadOp, ReadableStream, StreamOp, WritableStream, WriteOp};

// ---------------------------------------------------------------------------
// FileReadStream
// ---------------------------------------------------------------------------

/// Readable stream that reads from a file via the IO worker.
///
/// The entire file is read on the first `read()` call. Subsequent reads return
/// `Closed`. For incremental/seekable reads, a persistent file-handle variant
/// can be added later.
pub struct FileReadStream {
    path: PathBuf,
    exhausted: bool,
    open: bool,
}

impl FileReadStream {
    pub fn new(path: PathBuf) -> Self {
        Self {
            path,
            exhausted: false,
            open: true,
        }
    }
}

impl ReadableStream for FileReadStream {
    fn read(&mut self, _size: usize) -> ReadOp {
        if !self.open || self.exhausted {
            return StreamOp::Closed;
        }
        self.exhausted = true;
        let path = self.path.clone();
        StreamOp::Async(Box::pin(async move { tokio::fs::read(path).await }))
    }

    fn close_read(&mut self) {
        self.open = false;
    }

    fn is_readable(&self) -> bool {
        self.open && !self.exhausted
    }
}

// ---------------------------------------------------------------------------
// FileWriteStream
// ---------------------------------------------------------------------------

/// Writable stream that writes to a file via the IO worker.
pub struct FileWriteStream {
    path: PathBuf,
    append: bool,
    open: bool,
}

impl FileWriteStream {
    /// Create a stream that truncates the file on first write.
    pub fn new(path: PathBuf) -> Self {
        Self {
            path,
            append: false,
            open: true,
        }
    }

    /// Create a stream that appends to the file.
    pub fn new_append(path: PathBuf) -> Self {
        Self {
            path,
            append: true,
            open: true,
        }
    }
}

impl WritableStream for FileWriteStream {
    fn write(&mut self, data: &[u8]) -> WriteOp {
        if !self.open {
            return StreamOp::Closed;
        }
        let path = self.path.clone();
        let data = data.to_vec();
        let append = self.append;
        StreamOp::Async(Box::pin(async move {
            let len = data.len();
            if append {
                use tokio::io::AsyncWriteExt;
                let mut file = tokio::fs::OpenOptions::new()
                    .append(true)
                    .create(true)
                    .open(&path)
                    .await?;
                file.write_all(&data).await?;
            } else {
                tokio::fs::write(&path, &data).await?;
            }
            Ok(len)
        }))
    }

    fn flush(&mut self) -> FlushOp {
        // Tokio file writes are complete when the future resolves.
        if !self.open {
            return StreamOp::Closed;
        }
        StreamOp::Ready(Ok(()))
    }

    fn close_write(&mut self) {
        self.open = false;
    }

    fn is_writable(&self) -> bool {
        self.open
    }
}

// ---------------------------------------------------------------------------
// Convenience constructors
// ---------------------------------------------------------------------------

/// Open a file for reading as a stream.
pub fn open_read(path: impl Into<PathBuf>) -> FileReadStream {
    FileReadStream::new(path.into())
}

/// Open a file for writing (truncate) as a stream.
pub fn open_write(path: impl Into<PathBuf>) -> FileWriteStream {
    FileWriteStream::new(path.into())
}

/// Open a file for appending as a stream.
pub fn open_append(path: impl Into<PathBuf>) -> FileWriteStream {
    FileWriteStream::new_append(path.into())
}

/// Create a pair of read + write streams for the same file path.
///
/// The write stream truncates on first write; subsequent reads see the updated
/// content (each read re-opens the file).
pub fn open_read_write(path: impl Into<PathBuf>) -> (FileReadStream, FileWriteStream) {
    let p: PathBuf = path.into();
    (FileReadStream::new(p.clone()), FileWriteStream::new(p))
}
