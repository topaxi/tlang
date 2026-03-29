//! Networking primitives.
//!
//! # Architecture
//!
//! TCP sockets are non-blocking and managed directly by the mio event loop on
//! the main thread — this keeps latency low and avoids cross-thread
//! coordination for the hot path.
//!
//! Blocking operations like DNS resolution are offloaded to the IO worker
//! (tokio thread).

use std::io;
use std::net::SocketAddr;

use crate::runtime::Runtime;

/// Resolve a hostname to a list of socket addresses (via the IO worker).
pub fn dns_lookup(
    runtime: &mut Runtime,
    host: String,
    callback: impl FnOnce(io::Result<Vec<SocketAddr>>, &mut Runtime) + 'static,
) {
    runtime.spawn_io(
        async move {
            tokio::net::lookup_host(host)
                .await
                .map(|iter| iter.collect())
        },
        callback,
    );
}

/// A non-blocking TCP listener backed by [`mio::net::TcpListener`].
pub struct TcpListener {
    inner: mio::net::TcpListener,
    token: mio::Token,
}

impl TcpListener {
    /// Bind to `addr` and register with the event loop for incoming
    /// connections.
    pub fn bind(runtime: &mut Runtime, addr: SocketAddr) -> io::Result<Self> {
        let mut listener = mio::net::TcpListener::bind(addr)?;
        let token = runtime.event_loop.next_token();
        runtime.event_loop.poll().registry().register(
            &mut listener,
            token,
            mio::Interest::READABLE,
        )?;
        Ok(Self {
            inner: listener,
            token,
        })
    }

    /// Accept a new connection. Returns `WouldBlock` if none is ready.
    pub fn accept(&self) -> io::Result<(TcpStream, SocketAddr)> {
        let (stream, addr) = self.inner.accept()?;
        Ok((TcpStream { inner: stream }, addr))
    }

    /// The mio token this listener is registered under.
    pub fn token(&self) -> mio::Token {
        self.token
    }
}

/// A non-blocking TCP stream backed by [`mio::net::TcpStream`].
pub struct TcpStream {
    inner: mio::net::TcpStream,
}

impl TcpStream {
    /// Connect to `addr` and register with the event loop.
    pub fn connect(_runtime: &mut Runtime, addr: SocketAddr) -> io::Result<Self> {
        let stream = mio::net::TcpStream::connect(addr)?;
        Ok(Self { inner: stream })
    }

    /// Register this stream with the event loop for read/write readiness.
    pub fn register(
        &mut self,
        runtime: &mut Runtime,
        token: mio::Token,
        interest: mio::Interest,
    ) -> io::Result<()> {
        runtime
            .event_loop
            .poll()
            .registry()
            .register(&mut self.inner, token, interest)
    }
}

impl io::Read for TcpStream {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        io::Read::read(&mut self.inner, buf)
    }
}

impl io::Write for TcpStream {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        io::Write::write(&mut self.inner, buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        io::Write::flush(&mut self.inner)
    }
}
