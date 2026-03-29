use std::any::Any;
use std::future::Future;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::mpsc;
use std::thread;

/// Unique identifier for an in-flight IO request.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RequestId(u64);

/// A completion delivered from the IO worker back to the main thread.
pub struct IoCompletion {
    pub id: RequestId,
    pub result: Box<dyn Any + Send>,
}

/// Background IO worker backed by a single-threaded [`tokio`] runtime.
///
/// Heavy or blocking IO (filesystem, DNS, child processes) is offloaded here
/// so the main mio event loop stays responsive.  Results flow back through a
/// standard [`mpsc`] channel and the mio [`Waker`](mio::Waker) nudges the
/// poll so the main thread picks them up promptly.
pub struct IoWorker {
    runtime_handle: tokio::runtime::Handle,
    completion_tx: mpsc::Sender<IoCompletion>,
    completion_rx: mpsc::Receiver<IoCompletion>,
    waker: Arc<mio::Waker>,
    next_request_id: u64,
    pending: Arc<AtomicUsize>,
    _thread: thread::JoinHandle<()>,
}

impl IoWorker {
    /// Spawn the IO worker.
    ///
    /// `waker` is used to wake the mio poll when a completion is ready.
    pub fn new(waker: mio::Waker) -> Self {
        let waker = Arc::new(waker);
        let (completion_tx, completion_rx) = mpsc::channel();

        // Spin up the tokio runtime on a dedicated OS thread.
        let (handle_tx, handle_rx) = mpsc::channel();
        let thread = thread::Builder::new()
            .name("octarine-io".into())
            .spawn(move || {
                let rt = tokio::runtime::Builder::new_current_thread()
                    .enable_all()
                    .build()
                    .expect("failed to build tokio runtime");
                handle_tx
                    .send(rt.handle().clone())
                    .expect("main thread gone");
                // Keep the runtime alive until the process exits.
                rt.block_on(std::future::pending::<()>());
            })
            .expect("failed to spawn IO worker thread");

        let runtime_handle = handle_rx.recv().expect("IO worker thread panicked");

        Self {
            runtime_handle,
            completion_tx,
            completion_rx,
            waker,
            next_request_id: 0,
            pending: Arc::new(AtomicUsize::new(0)),
            _thread: thread,
        }
    }

    /// Submit an async operation to the tokio runtime.
    ///
    /// Returns a [`RequestId`] that can be used to match the completion later.
    pub fn submit<F, T>(&mut self, future: F) -> RequestId
    where
        F: Future<Output = T> + Send + 'static,
        T: Send + 'static,
    {
        let id = RequestId(self.next_request_id);
        self.next_request_id += 1;

        let tx = self.completion_tx.clone();
        let waker = self.waker.clone();
        let pending = self.pending.clone();

        pending.fetch_add(1, Ordering::Relaxed);

        self.runtime_handle.spawn(async move {
            let result = future.await;
            pending.fetch_sub(1, Ordering::Relaxed);
            let _ = tx.send(IoCompletion {
                id,
                result: Box::new(result),
            });
            let _ = waker.wake();
        });

        id
    }

    /// Drain all completions that are currently ready.
    pub fn drain_completions(&self) -> Vec<IoCompletion> {
        let mut out = Vec::new();
        while let Ok(c) = self.completion_rx.try_recv() {
            out.push(c);
        }
        out
    }

    /// Number of requests that have been submitted but not yet completed.
    pub fn pending_count(&self) -> usize {
        self.pending.load(Ordering::Relaxed)
    }
}
