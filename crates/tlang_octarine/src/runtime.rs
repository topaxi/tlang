use std::any::Any;
use std::collections::HashMap;
use std::future::Future;

use crate::event_loop::{EventLoop, WAKER_TOKEN};
use crate::io_worker::{IoWorker, RequestId};

type CompletionCallback = Box<dyn FnOnce(Box<dyn Any + Send>, &mut Runtime)>;

/// The octarine runtime.
///
/// Ties together the single-threaded mio [`EventLoop`] with the background
/// [`IoWorker`] (tokio) and provides a unified API for scheduling timers, I/O
/// handlers, and async operations.
pub struct Runtime {
    pub event_loop: EventLoop,
    io_worker: IoWorker,
    completion_callbacks: HashMap<RequestId, CompletionCallback>,
}

impl Runtime {
    /// Create a new runtime.
    pub fn new() -> std::io::Result<Self> {
        let event_loop = EventLoop::new()?;
        let waker = mio::Waker::new(event_loop.registry(), WAKER_TOKEN)?;
        let io_worker = IoWorker::new(waker);

        Ok(Self {
            event_loop,
            io_worker,
            completion_callbacks: HashMap::new(),
        })
    }

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
    }

    /// Execute one iteration of the runtime loop:
    ///
    /// 1. Process IO completions delivered by the worker thread.
    /// 2. Tick the mio event loop (tasks → timers → poll → I/O dispatch).
    fn tick(&mut self) -> std::io::Result<()> {
        self.process_completions();
        self.event_loop.tick()
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
}
