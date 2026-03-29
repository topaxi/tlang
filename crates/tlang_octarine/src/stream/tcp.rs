//! TCP stream backed by [`mio::net::TcpStream`].
//!
//! TCP sockets are non-blocking and driven directly by the mio event loop on
//! the main thread. When a read or write would block, the stream returns
//! [`StreamOp::WouldBlock`] with its mio token so the runtime can retry once
//! the socket becomes ready.

use std::io::{self, Read, Write};

use super::{FlushOp, ReadOp, ReadableStream, StreamOp, WritableStream, WriteOp};

/// Duplex stream wrapping a non-blocking [`mio::net::TcpStream`].
pub struct TcpStreamHandle {
    inner: mio::net::TcpStream,
    token: mio::Token,
    read_open: bool,
    write_open: bool,
}

impl TcpStreamHandle {
    /// Wrap an already-connected and mio-registered TCP stream.
    pub fn new(inner: mio::net::TcpStream, token: mio::Token) -> Self {
        Self {
            inner,
            token,
            read_open: true,
            write_open: true,
        }
    }

    /// The mio token this stream is registered under.
    pub fn token(&self) -> mio::Token {
        self.token
    }

    /// Access the underlying mio stream (e.g. for re-registration).
    pub fn inner_mut(&mut self) -> &mut mio::net::TcpStream {
        &mut self.inner
    }
}

impl ReadableStream for TcpStreamHandle {
    fn read(&mut self, size: usize) -> ReadOp {
        if !self.read_open {
            return StreamOp::Closed;
        }
        let mut buf = vec![0u8; size];
        match self.inner.read(&mut buf) {
            Ok(0) => {
                self.read_open = false;
                StreamOp::Closed
            }
            Ok(n) => {
                buf.truncate(n);
                StreamOp::Ready(Ok(buf))
            }
            Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => StreamOp::WouldBlock(self.token),
            Err(e) => StreamOp::Ready(Err(e)),
        }
    }

    fn close_read(&mut self) {
        self.read_open = false;
    }

    fn is_readable(&self) -> bool {
        self.read_open
    }
}

impl WritableStream for TcpStreamHandle {
    fn write(&mut self, data: &[u8]) -> WriteOp {
        if !self.write_open {
            return StreamOp::Closed;
        }
        match self.inner.write(data) {
            Ok(n) => StreamOp::Ready(Ok(n)),
            Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => StreamOp::WouldBlock(self.token),
            Err(e) => StreamOp::Ready(Err(e)),
        }
    }

    fn flush(&mut self) -> FlushOp {
        if !self.write_open {
            return StreamOp::Closed;
        }
        match self.inner.flush() {
            Ok(()) => StreamOp::Ready(Ok(())),
            Err(ref e) if e.kind() == io::ErrorKind::WouldBlock => StreamOp::WouldBlock(self.token),
            Err(e) => StreamOp::Ready(Err(e)),
        }
    }

    fn close_write(&mut self) {
        self.write_open = false;
        let _ = self.inner.shutdown(std::net::Shutdown::Write);
    }

    fn is_writable(&self) -> bool {
        self.write_open
    }
}
