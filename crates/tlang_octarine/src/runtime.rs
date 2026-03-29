use std::any::Any;
use std::collections::HashMap;
use std::future::Future;
use std::io;

use crate::event_loop::{EventLoop, WAKER_TOKEN};
use crate::io_worker::{IoWorker, RequestId};
use crate::stream::{ReadOp, StreamId, StreamOp, StreamRegistry, WriteOp};

type CompletionCallback = Box<dyn FnOnce(Box<dyn Any + Send>, &mut Runtime)>;
type ReadCallback = Box<dyn FnOnce(io::Result<Vec<u8>>, &mut Runtime)>;
type WriteCallback = Box<dyn FnOnce(io::Result<usize>, &mut Runtime)>;

struct PendingRead {
    stream_id: StreamId,
    size: usize,
    callback: ReadCallback,
}

struct PendingWrite {
    stream_id: StreamId,
    data: Vec<u8>,
    callback: WriteCallback,
}

/// The octarine runtime.
///
/// Ties together the single-threaded mio [`EventLoop`] with the background
/// [`IoWorker`] (tokio), the [`StreamRegistry`], and provides a unified API
/// for scheduling timers, I/O handlers, and async operations.
pub struct Runtime {
    pub event_loop: EventLoop,
    io_worker: IoWorker,
    completion_callbacks: HashMap<RequestId, CompletionCallback>,
    /// All open streams keyed by [`StreamId`]. Stdin, stdout, and stderr are
    /// pre-registered at construction time.
    pub streams: StreamRegistry,
    /// Reads waiting for mio readability (keyed by mio token).
    pending_reads: HashMap<mio::Token, PendingRead>,
    /// Writes waiting for mio writability (keyed by mio token).
    pending_writes: HashMap<mio::Token, PendingWrite>,
}

impl Runtime {
    /// Create a new runtime with stdin/stdout/stderr pre-registered.
    pub fn new() -> std::io::Result<Self> {
        let event_loop = EventLoop::new()?;
        let waker = mio::Waker::new(event_loop.registry(), WAKER_TOKEN)?;
        let io_worker = IoWorker::new(waker);

        Ok(Self {
            event_loop,
            io_worker,
            completion_callbacks: HashMap::new(),
            streams: StreamRegistry::new(),
            pending_reads: HashMap::new(),
            pending_writes: HashMap::new(),
        })
    }

    // -----------------------------------------------------------------------
    // Generic IO worker integration
    // -----------------------------------------------------------------------

    /// Submit an async operation to the IO worker thread and register a typed
    /// callback that will run on the main thread when the result is ready.
    ///
    /// This is the primary way stdlib modules schedule background work.
    pub fn spawn_io<F, T>(
        &mut self,
        future: F,
        callback: impl FnOnce(T, &mut Runtime) + 'static,
    ) -> RequestId
    where
        F: Future<Output = T> + Send + 'static,
        T: Send + 'static,
    {
        let id = self.io_worker.submit(future);
        self.completion_callbacks.insert(
            id,
            Box::new(move |boxed, rt| {
                let value = *boxed.downcast::<T>().expect("IO completion type mismatch");
                callback(value, rt);
            }),
        );
        id
    }

    // -----------------------------------------------------------------------
    // Stream operations
    // -----------------------------------------------------------------------

    /// Read up to `size` bytes from the stream identified by `id`.
    ///
    /// Depending on the stream type the read may complete synchronously, be
    /// dispatched to the IO worker, or deferred until the mio source is ready.
    pub fn stream_read(
        &mut self,
        id: StreamId,
        size: usize,
        callback: impl FnOnce(io::Result<Vec<u8>>, &mut Runtime) + 'static,
    ) {
        let action = self.streams.read(id, size);
        self.dispatch_read(id, size, action, callback);
    }

    /// Write `data` to the stream identified by `id`.
    pub fn stream_write(
        &mut self,
        id: StreamId,
        data: Vec<u8>,
        callback: impl FnOnce(io::Result<usize>, &mut Runtime) + 'static,
    ) {
        let action = self.streams.write(id, &data);
        self.dispatch_write(id, data, action, callback);
    }

    /// Flush the stream identified by `id`.
    pub fn stream_flush(
        &mut self,
        id: StreamId,
        callback: impl FnOnce(io::Result<()>, &mut Runtime) + 'static,
    ) {
        let action = self.streams.flush(id);
        match action {
            Some(StreamOp::Ready(result)) => callback(result, self),
            Some(StreamOp::Async(future)) => {
                self.spawn_io(future, |result, rt| callback(result, rt));
            }
            Some(StreamOp::WouldBlock(_token)) => {
                // Flush with WouldBlock is unlikely in practice; just succeed.
                callback(Ok(()), self);
            }
            Some(StreamOp::Closed) | None => {
                callback(
                    Err(io::Error::new(
                        io::ErrorKind::BrokenPipe,
                        "stream not flushable",
                    )),
                    self,
                );
            }
        }
    }

    /// Close the stream identified by `id`.
    pub fn stream_close(&mut self, id: StreamId) {
        self.streams.close(id);
    }

    // -----------------------------------------------------------------------
    // Run loop
    // -----------------------------------------------------------------------

    /// Run the event loop until all work is done or the loop is stopped.
    pub fn run(&mut self) -> std::io::Result<()> {
        while !self.event_loop.is_stopped() && self.has_pending_work() {
            self.tick()?;
        }
        Ok(())
    }

    /// Returns `true` when there is still work pending on either the event loop
    /// or the IO worker.
    pub fn has_pending_work(&self) -> bool {
        self.event_loop.has_pending_work()
            || self.io_worker.pending_count() > 0
            || !self.completion_callbacks.is_empty()
            || !self.pending_reads.is_empty()
            || !self.pending_writes.is_empty()
    }

    /// Execute one iteration of the runtime loop:
    ///
    /// 1. Process IO completions delivered by the worker thread.
    /// 2. Tick the mio event loop (tasks → timers → poll → I/O dispatch).
    /// 3. Retry any stream operations whose mio tokens became ready.
    fn tick(&mut self) -> std::io::Result<()> {
        self.process_completions();
        self.event_loop.tick()?;
        self.retry_pending_stream_ops();
        Ok(())
    }

    /// Drain ready IO completions and invoke their callbacks.
    fn process_completions(&mut self) {
        let completions = self.io_worker.drain_completions();
        for completion in completions {
            if let Some(callback) = self.completion_callbacks.remove(&completion.id) {
                callback(completion.result, self);
            }
        }
    }

    /// Check which mio tokens became ready and retry their pending stream
    /// reads/writes.
    fn retry_pending_stream_ops(&mut self) {
        let ready: Vec<(mio::Token, bool, bool)> = self.event_loop.ready_tokens().to_vec();

        for (token, readable, writable) in ready {
            if readable && let Some(pending) = self.pending_reads.remove(&token) {
                let action = self.streams.read(pending.stream_id, pending.size);
                self.dispatch_read(pending.stream_id, pending.size, action, pending.callback);
            }
            if writable && let Some(pending) = self.pending_writes.remove(&token) {
                let action = self.streams.write(pending.stream_id, &pending.data);
                self.dispatch_write(pending.stream_id, pending.data, action, pending.callback);
            }
        }
    }

    // -----------------------------------------------------------------------
    // Internal dispatch helpers
    // -----------------------------------------------------------------------

    fn dispatch_read(
        &mut self,
        id: StreamId,
        size: usize,
        action: Option<ReadOp>,
        callback: impl FnOnce(io::Result<Vec<u8>>, &mut Runtime) + 'static,
    ) {
        match action {
            Some(StreamOp::Ready(result)) => callback(result, self),
            Some(StreamOp::Async(future)) => {
                self.spawn_io(future, |result, rt| callback(result, rt));
            }
            Some(StreamOp::WouldBlock(token)) => {
                self.pending_reads.insert(
                    token,
                    PendingRead {
                        stream_id: id,
                        size,
                        callback: Box::new(callback),
                    },
                );
            }
            Some(StreamOp::Closed) | None => {
                callback(
                    Err(io::Error::new(
                        io::ErrorKind::BrokenPipe,
                        "stream not readable",
                    )),
                    self,
                );
            }
        }
    }

    fn dispatch_write(
        &mut self,
        id: StreamId,
        data: Vec<u8>,
        action: Option<WriteOp>,
        callback: impl FnOnce(io::Result<usize>, &mut Runtime) + 'static,
    ) {
        match action {
            Some(StreamOp::Ready(result)) => callback(result, self),
            Some(StreamOp::Async(future)) => {
                self.spawn_io(future, |result, rt| callback(result, rt));
            }
            Some(StreamOp::WouldBlock(token)) => {
                self.pending_writes.insert(
                    token,
                    PendingWrite {
                        stream_id: id,
                        data,
                        callback: Box::new(callback),
                    },
                );
            }
            Some(StreamOp::Closed) | None => {
                callback(
                    Err(io::Error::new(
                        io::ErrorKind::BrokenPipe,
                        "stream not writable",
                    )),
                    self,
                );
            }
        }
    }
}
