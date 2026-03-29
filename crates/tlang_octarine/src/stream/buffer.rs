//! In-memory duplex stream.
//!
//! Useful for testing, piping between producers and consumers on the same
//! thread, or buffering data that will be consumed later.

use std::collections::VecDeque;

use super::{FlushOp, ReadOp, ReadableStream, StreamOp, WritableStream, WriteOp};

/// A fully synchronous in-memory stream that supports both reading and writing.
///
/// Written bytes are appended to an internal ring buffer and can be read back
/// in FIFO order.
pub struct BufferStream {
    buf: VecDeque<u8>,
    read_open: bool,
    write_open: bool,
}

impl BufferStream {
    /// Create an empty buffer stream.
    pub fn new() -> Self {
        Self {
            buf: VecDeque::new(),
            read_open: true,
            write_open: true,
        }
    }

    /// Create a buffer stream pre-filled with `data`.
    pub fn from_bytes(data: impl Into<Vec<u8>>) -> Self {
        Self {
            buf: VecDeque::from(data.into()),
            read_open: true,
            write_open: true,
        }
    }

    /// Number of bytes currently buffered.
    pub fn len(&self) -> usize {
        self.buf.len()
    }

    /// Whether the buffer is empty.
    pub fn is_empty(&self) -> bool {
        self.buf.is_empty()
    }
}

impl ReadableStream for BufferStream {
    fn read(&mut self, size: usize) -> ReadOp {
        if self.buf.is_empty() {
            if !self.read_open {
                return StreamOp::Closed;
            }
            // No data available but the stream is still open — return an empty
            // read to signal "try again later" without blocking.
            return StreamOp::Ready(Ok(Vec::new()));
        }
        let n = size.min(self.buf.len());
        let chunk: Vec<u8> = self.buf.drain(..n).collect();
        StreamOp::Ready(Ok(chunk))
    }

    fn close_read(&mut self) {
        self.read_open = false;
    }

    fn is_readable(&self) -> bool {
        self.read_open
    }
}

impl WritableStream for BufferStream {
    fn write(&mut self, data: &[u8]) -> WriteOp {
        if !self.write_open {
            return StreamOp::Closed;
        }
        self.buf.extend(data);
        StreamOp::Ready(Ok(data.len()))
    }

    fn flush(&mut self) -> FlushOp {
        if !self.write_open {
            return StreamOp::Closed;
        }
        StreamOp::Ready(Ok(()))
    }

    fn close_write(&mut self) {
        self.write_open = false;
    }

    fn is_writable(&self) -> bool {
        self.write_open
    }
}

/// Create a connected pair of buffer streams that share no state.
///
/// Data written to the first can be read from the first (it's a single duplex
/// buffer, not a pipe). For a true pipe, write to one BufferStream and have
/// the runtime transfer data to another.
pub fn pipe() -> (BufferStream, BufferStream) {
    (BufferStream::new(), BufferStream::new())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trip() {
        let mut stream = BufferStream::new();

        // Write
        let StreamOp::Ready(Ok(n)) = stream.write(b"hello") else {
            panic!("expected Ready");
        };
        assert_eq!(n, 5);

        // Read back
        let StreamOp::Ready(Ok(data)) = stream.read(1024) else {
            panic!("expected Ready");
        };
        assert_eq!(data, b"hello");

        // Empty after consumption
        assert!(stream.is_empty());
    }

    #[test]
    fn partial_read() {
        let mut stream = BufferStream::from_bytes(b"hello world".to_vec());
        let StreamOp::Ready(Ok(data)) = stream.read(5) else {
            panic!("expected Ready");
        };
        assert_eq!(data, b"hello");
        assert_eq!(stream.len(), 6); // " world"
    }

    #[test]
    fn closed_stream() {
        let mut stream = BufferStream::new();
        stream.close_write();
        assert!(matches!(stream.write(b"x"), StreamOp::Closed));
    }
}
