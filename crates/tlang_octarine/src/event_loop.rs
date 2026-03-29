use std::collections::VecDeque;
use std::time::{Duration, Instant};

use mio::{Events, Poll, Token};

/// Reserved token used by [`mio::Waker`] to wake the poll from the IO worker
/// thread.
pub const WAKER_TOKEN: Token = Token(usize::MAX);

/// Unique identifier for a scheduled timer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TimerId(u64);

/// A timer that fires a callback after a deadline.
struct Timer {
    id: TimerId,
    deadline: Instant,
    callback: Box<dyn FnOnce(&mut EventLoop)>,
}

type Task = Box<dyn FnOnce(&mut EventLoop)>;
type IoHandler = Box<dyn FnMut(IoEvent)>;

/// Lightweight event info passed to I/O handlers.
#[derive(Debug, Clone, Copy)]
pub struct IoEvent {
    pub readable: bool,
    pub writable: bool,
}

/// Single-threaded event loop built on top of [`mio::Poll`].
///
/// Modelled after the Node.js event loop: one thread drives I/O polling,
/// timers, and a microtask/macrotask queue. Nothing here is `Send` — all
/// interaction happens on the thread that owns the loop.
pub struct EventLoop {
    poll: Poll,
    events: Events,
    /// Monotonically increasing token counter for mio registrations.
    next_token: usize,
    /// Monotonically increasing id counter for timers.
    next_timer_id: u64,
    /// Pending timers sorted by deadline (earliest first).
    timers: Vec<Timer>,
    /// FIFO queue of tasks ready to execute (analogous to the microtask queue).
    task_queue: VecDeque<Task>,
    /// Registered I/O event handlers keyed by mio token.
    io_handlers: Vec<(Token, IoHandler)>,
    /// When `true` the loop will exit at the next opportunity.
    stopped: bool,
}

impl EventLoop {
    /// Create a new event loop.
    pub fn new() -> std::io::Result<Self> {
        Ok(Self {
            poll: Poll::new()?,
            events: Events::with_capacity(1024),
            next_token: 0,
            next_timer_id: 0,
            timers: Vec::new(),
            task_queue: VecDeque::new(),
            io_handlers: Vec::new(),
            stopped: false,
        })
    }

    /// Return a reference to the inner [`mio::Poll`] so callers can register
    /// I/O sources.
    pub fn poll(&self) -> &Poll {
        &self.poll
    }

    /// Return the registry for external mio registrations (e.g. creating a
    /// [`mio::Waker`]).
    pub fn registry(&self) -> &mio::Registry {
        self.poll.registry()
    }

    /// Allocate the next unique [`mio::Token`].
    pub fn next_token(&mut self) -> Token {
        let token = Token(self.next_token);
        self.next_token += 1;
        token
    }

    /// Schedule a one-shot timer that fires `callback` after `delay`.
    pub fn set_timeout(
        &mut self,
        delay: Duration,
        callback: impl FnOnce(&mut EventLoop) + 'static,
    ) -> TimerId {
        let id = TimerId(self.next_timer_id);
        self.next_timer_id += 1;

        let timer = Timer {
            id,
            deadline: Instant::now() + delay,
            callback: Box::new(callback),
        };

        // Insert in sorted order (earliest deadline first).
        let pos = self
            .timers
            .binary_search_by(|t| t.deadline.cmp(&timer.deadline))
            .unwrap_or_else(|pos| pos);
        self.timers.insert(pos, timer);

        id
    }

    /// Cancel a previously scheduled timer. Returns `true` if the timer was
    /// found and removed.
    pub fn clear_timeout(&mut self, id: TimerId) -> bool {
        if let Some(pos) = self.timers.iter().position(|t| t.id == id) {
            self.timers.remove(pos);
            true
        } else {
            false
        }
    }

    /// Register a handler for I/O readiness events on the given token.
    pub fn register_io_handler(&mut self, token: Token, handler: impl FnMut(IoEvent) + 'static) {
        self.io_handlers.push((token, Box::new(handler)));
    }

    /// Remove the I/O handler for the given token.
    pub fn deregister_io_handler(&mut self, token: Token) {
        self.io_handlers.retain(|(t, _)| *t != token);
    }

    /// Push a task onto the ready queue. It will execute before the next I/O
    /// poll (like a microtask).
    pub fn enqueue(&mut self, task: impl FnOnce(&mut EventLoop) + 'static) {
        self.task_queue.push_back(Box::new(task));
    }

    /// Signal the event loop to stop after draining the current task queue.
    pub fn stop(&mut self) {
        self.stopped = true;
    }

    /// Returns `true` if the loop has been asked to stop.
    pub fn is_stopped(&self) -> bool {
        self.stopped
    }

    /// Returns `true` if there is still work to do (pending tasks, timers, or
    /// registered I/O handlers).
    pub fn has_pending_work(&self) -> bool {
        !self.task_queue.is_empty() || !self.timers.is_empty() || !self.io_handlers.is_empty()
    }

    /// Run the event loop until there is no more work to do or [`stop`] is
    /// called.
    pub fn run(&mut self) -> std::io::Result<()> {
        while !self.stopped && self.has_pending_work() {
            self.tick()?;
        }
        Ok(())
    }

    /// Execute a single iteration of the event loop:
    ///
    /// 1. Drain the task queue.
    /// 2. Fire any timers whose deadline has passed.
    /// 3. Poll for I/O events (with a timeout based on the next timer).
    pub fn tick(&mut self) -> std::io::Result<()> {
        // 1. Drain task queue.
        while let Some(task) = self.task_queue.pop_front() {
            task(self);
            if self.stopped {
                return Ok(());
            }
        }

        // 2. Fire ready timers.
        self.fire_timers();
        if self.stopped {
            return Ok(());
        }

        // 3. Poll for I/O.
        let timeout = self.next_timer_deadline().map(|d| {
            let now = Instant::now();
            if d > now { d - now } else { Duration::ZERO }
        });

        self.poll.poll(&mut self.events, timeout)?;

        // Collect events before dispatching so we don't hold a borrow on self.
        let events: Vec<(Token, bool, bool)> = self
            .events
            .iter()
            .map(|e| (e.token(), e.is_readable(), e.is_writable()))
            .collect();

        for &(token, readable, writable) in &events {
            if token == WAKER_TOKEN {
                // The IO worker woke us up — completions are processed by
                // Runtime::tick(), not here.
                continue;
            }

            log::trace!(
                "mio event: token={:?}, readable={readable}, writable={writable}",
                token,
            );

            // Dispatch to handler. We use index-based lookup so the handler can
            // be called without conflicting borrows.
            if let Some(idx) = self.io_handlers.iter().position(|(t, _)| *t == token) {
                let (_, handler) = &mut self.io_handlers[idx];
                handler(IoEvent { readable, writable });
            }
        }

        // Timers may have become ready while we were polling.
        self.fire_timers();

        Ok(())
    }

    /// Fire all timers whose deadline is <= now.
    fn fire_timers(&mut self) {
        let now = Instant::now();
        while let Some(timer) = self.timers.first() {
            if timer.deadline > now {
                break;
            }
            // Remove and invoke.
            let timer = self.timers.remove(0);
            (timer.callback)(self);
        }
    }

    /// Return the deadline of the earliest pending timer, if any.
    fn next_timer_deadline(&self) -> Option<Instant> {
        self.timers.first().map(|t| t.deadline)
    }
}
