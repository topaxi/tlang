//! Native tlang bindings for the octarine stream protocol.
//!
//! Registers stream types (`Streams.ReadStream`, `Streams.WriteStream`,
//! `Streams.DuplexStream`) and factory/helper functions (`Streams::stdin`,
//! `Streams::open_read`, etc.) via the `inventory` crate so the interpreter
//! discovers them automatically at startup.
//!
//! # Thread-local context
//!
//! Stream handles stored in tlang values contain only a numeric ID. The actual
//! I/O resources live in a [`NativeStreamRegistry`] accessed through a
//! thread-local pointer. The octarine runtime must call [`set_stream_context`]
//! before invoking the interpreter and [`clear_stream_context`] afterward.
//!
//! Because octarine is single-threaded, this is safe.

use std::cell::Cell;
use std::collections::HashMap;
use std::io::{self, Read, Write};

use tlang_macros::{define_struct, native_fn};
use tlang_memory::value::object::{TlangObjectKind, TlangStruct};
use tlang_memory::{TlangValue, VMState};

// ---------------------------------------------------------------------------
// Native stream storage (synchronous I/O)
// ---------------------------------------------------------------------------

type NativeStreamId = u64;

/// A stream resource for synchronous I/O from native functions.
pub enum NativeStream {
    Stdin,
    Stdout,
    Stderr,
    FileRead(std::fs::File),
    FileWrite(std::fs::File),
    Buffer {
        data: std::collections::VecDeque<u8>,
        write_open: bool,
    },
}

/// Registry of native stream resources, keyed by numeric ID.
///
/// Pre-registers stdin (0), stdout (1), stderr (2).
pub struct NativeStreamRegistry {
    streams: HashMap<NativeStreamId, NativeStream>,
    next_id: NativeStreamId,
}

impl NativeStreamRegistry {
    pub fn new() -> Self {
        let mut reg = Self {
            streams: HashMap::new(),
            next_id: 3,
        };
        reg.streams.insert(0, NativeStream::Stdin);
        reg.streams.insert(1, NativeStream::Stdout);
        reg.streams.insert(2, NativeStream::Stderr);
        reg
    }

    pub fn register(&mut self, stream: NativeStream) -> NativeStreamId {
        let id = self.next_id;
        self.next_id += 1;
        self.streams.insert(id, stream);
        id
    }

    pub fn get_mut(&mut self, id: NativeStreamId) -> Option<&mut NativeStream> {
        self.streams.get_mut(&id)
    }

    pub fn remove(&mut self, id: NativeStreamId) -> Option<NativeStream> {
        self.streams.remove(&id)
    }
}

impl Default for NativeStreamRegistry {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Thread-local context
// ---------------------------------------------------------------------------

thread_local! {
    static STREAM_CTX: Cell<*mut NativeStreamRegistry> = const { Cell::new(std::ptr::null_mut()) };
}

/// Install the native stream registry for the current thread.
///
/// # Safety
///
/// The caller must ensure `registry` outlives any interpreter calls and that
/// [`clear_stream_context`] is called before the registry is dropped.
pub fn set_stream_context(registry: &mut NativeStreamRegistry) {
    STREAM_CTX.set(registry as *mut NativeStreamRegistry);
}

/// Remove the stream context.
pub fn clear_stream_context() {
    STREAM_CTX.set(std::ptr::null_mut());
}

/// Access the stream registry. Panics if the context hasn't been set.
fn with_streams<R>(f: impl FnOnce(&mut NativeStreamRegistry) -> R) -> R {
    STREAM_CTX.with(|cell| {
        let ptr = cell.get();
        assert!(
            !ptr.is_null(),
            "octarine stream context not set — call set_stream_context before the interpreter"
        );
        // Safety: single-threaded runtime; the pointer is valid while the
        // interpreter is running.
        f(unsafe { &mut *ptr })
    })
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Extract the stream ID from a tlang stream struct.
fn stream_id(vm: &VMState, handle: TlangValue) -> NativeStreamId {
    let obj = vm.get_struct(handle).expect("expected stream struct");
    obj[0].as_usize() as NativeStreamId
}

/// Create a `Streams.ReadStream` value from a stream ID.
fn new_read_stream(vm: &mut VMState, id: NativeStreamId) -> TlangValue {
    let shape = vm
        .heap
        .builtin_shapes
        .lookup("Streams.ReadStream")
        .expect("Streams.ReadStream shape not registered");
    vm.new_object(TlangObjectKind::Struct(TlangStruct::new(
        shape,
        vec![TlangValue::I64(id as i64)],
    )))
}

/// Create a `Streams.WriteStream` value from a stream ID.
fn new_write_stream(vm: &mut VMState, id: NativeStreamId) -> TlangValue {
    let shape = vm
        .heap
        .builtin_shapes
        .lookup("Streams.WriteStream")
        .expect("Streams.WriteStream shape not registered");
    vm.new_object(TlangObjectKind::Struct(TlangStruct::new(
        shape,
        vec![TlangValue::I64(id as i64)],
    )))
}

/// Create a `Streams.DuplexStream` value from a stream ID.
fn new_duplex_stream(vm: &mut VMState, id: NativeStreamId) -> TlangValue {
    let shape = vm
        .heap
        .builtin_shapes
        .lookup("Streams.DuplexStream")
        .expect("Streams.DuplexStream shape not registered");
    vm.new_object(TlangObjectKind::Struct(TlangStruct::new(
        shape,
        vec![TlangValue::I64(id as i64)],
    )))
}

/// Perform a synchronous read of up to `size` bytes from a native stream.
fn sync_read(stream: &mut NativeStream, size: usize) -> io::Result<Vec<u8>> {
    match stream {
        NativeStream::Stdin => {
            let mut buf = vec![0u8; size];
            let n = io::stdin().read(&mut buf)?;
            buf.truncate(n);
            Ok(buf)
        }
        NativeStream::FileRead(f) => {
            let mut buf = vec![0u8; size];
            let n = f.read(&mut buf)?;
            buf.truncate(n);
            Ok(buf)
        }
        NativeStream::Buffer { data, .. } => {
            let n = size.min(data.len());
            Ok(data.drain(..n).collect())
        }
        _ => Err(io::Error::new(io::ErrorKind::Unsupported, "not readable")),
    }
}

/// Perform a synchronous write to a native stream.
fn sync_write(stream: &mut NativeStream, bytes: &[u8]) -> io::Result<usize> {
    match stream {
        NativeStream::Stdout => io::stdout().write(bytes),
        NativeStream::Stderr => io::stderr().write(bytes),
        NativeStream::FileWrite(f) => f.write(bytes),
        NativeStream::Buffer { data, write_open } => {
            if !*write_open {
                return Err(io::Error::new(io::ErrorKind::BrokenPipe, "stream closed"));
            }
            data.extend(bytes);
            Ok(bytes.len())
        }
        _ => Err(io::Error::new(io::ErrorKind::Unsupported, "not writable")),
    }
}

/// Perform a synchronous flush.
fn sync_flush(stream: &mut NativeStream) -> io::Result<()> {
    match stream {
        NativeStream::Stdout => io::stdout().flush(),
        NativeStream::Stderr => io::stderr().flush(),
        NativeStream::FileWrite(f) => f.flush(),
        NativeStream::Buffer { .. } => Ok(()),
        _ => Err(io::Error::new(io::ErrorKind::Unsupported, "not flushable")),
    }
}

/// Read all remaining bytes from a stream.
fn sync_read_all(stream: &mut NativeStream) -> io::Result<Vec<u8>> {
    match stream {
        NativeStream::Stdin => {
            let mut buf = Vec::new();
            io::stdin().read_to_end(&mut buf)?;
            Ok(buf)
        }
        NativeStream::FileRead(f) => {
            let mut buf = Vec::new();
            f.read_to_end(&mut buf)?;
            Ok(buf)
        }
        NativeStream::Buffer { data, .. } => Ok(data.drain(..).collect()),
        _ => Err(io::Error::new(io::ErrorKind::Unsupported, "not readable")),
    }
}

// ---------------------------------------------------------------------------
// Factory functions
// ---------------------------------------------------------------------------

/// `Streams::stdin()` — returns a ReadStream handle for standard input.
#[native_fn(name = "Streams::stdin")]
pub fn streams_stdin(vm: &mut VMState) -> TlangValue {
    new_read_stream(vm, 0)
}

/// `Streams::stdout()` — returns a WriteStream handle for standard output.
#[native_fn(name = "Streams::stdout")]
pub fn streams_stdout(vm: &mut VMState) -> TlangValue {
    new_write_stream(vm, 1)
}

/// `Streams::stderr()` — returns a WriteStream handle for standard error.
#[native_fn(name = "Streams::stderr")]
pub fn streams_stderr(vm: &mut VMState) -> TlangValue {
    new_write_stream(vm, 2)
}

/// `Streams::open_read(path)` — opens a file for reading.
#[native_fn(name = "Streams::open_read")]
pub fn streams_open_read(vm: &mut VMState, path_val: TlangValue) -> TlangValue {
    let path = vm
        .get_object(path_val)
        .and_then(|o| o.as_str())
        .unwrap_or_else(|| vm.panic("Streams.open_read: expected string path".to_string()));
    let file =
        std::fs::File::open(path).unwrap_or_else(|e| vm.panic(format!("Streams.open_read: {e}")));
    let id = with_streams(|reg| reg.register(NativeStream::FileRead(file)));
    new_read_stream(vm, id)
}

/// `Streams::open_write(path)` — opens a file for writing (creates/truncates).
#[native_fn(name = "Streams::open_write")]
pub fn streams_open_write(vm: &mut VMState, path_val: TlangValue) -> TlangValue {
    let path = vm
        .get_object(path_val)
        .and_then(|o| o.as_str())
        .unwrap_or_else(|| vm.panic("Streams.open_write: expected string path".to_string()));
    let file = std::fs::File::create(path)
        .unwrap_or_else(|e| vm.panic(format!("Streams.open_write: {e}")));
    let id = with_streams(|reg| reg.register(NativeStream::FileWrite(file)));
    new_write_stream(vm, id)
}

/// `Streams::open_append(path)` — opens a file for appending.
#[native_fn(name = "Streams::open_append")]
pub fn streams_open_append(vm: &mut VMState, path_val: TlangValue) -> TlangValue {
    let path = vm
        .get_object(path_val)
        .and_then(|o| o.as_str())
        .unwrap_or_else(|| vm.panic("Streams.open_append: expected string path".to_string()));
    let file = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(path)
        .unwrap_or_else(|e| vm.panic(format!("Streams.open_append: {e}")));
    let id = with_streams(|reg| reg.register(NativeStream::FileWrite(file)));
    new_write_stream(vm, id)
}

/// `Streams::buffer()` — creates an empty in-memory duplex stream.
#[native_fn(name = "Streams::buffer")]
pub fn streams_buffer(vm: &mut VMState) -> TlangValue {
    let id = with_streams(|reg| {
        reg.register(NativeStream::Buffer {
            data: std::collections::VecDeque::new(),
            write_open: true,
        })
    });
    new_duplex_stream(vm, id)
}

/// `Streams::buffer_from(data)` — creates a duplex stream pre-filled with data.
#[native_fn(name = "Streams::buffer_from")]
pub fn streams_buffer_from(vm: &mut VMState, data_val: TlangValue) -> TlangValue {
    let s = vm
        .get_object(data_val)
        .and_then(|o| o.as_str())
        .unwrap_or_else(|| vm.panic("Streams.buffer_from: expected string data".to_string()));
    let bytes = s.as_bytes().to_vec();
    let id = with_streams(|reg| {
        reg.register(NativeStream::Buffer {
            data: std::collections::VecDeque::from(bytes),
            write_open: true,
        })
    });
    new_duplex_stream(vm, id)
}

// ---------------------------------------------------------------------------
// Struct definitions with methods
// ---------------------------------------------------------------------------

define_struct! {
    struct Streams.ReadStream { id }

    impl Streams.ReadStream {
        fn read(this, size) {
            let sid = stream_id(vm, this);
            let sz = if size.is_nil() { 4096 } else { size.as_usize() };
            let bytes = with_streams(|reg| {
                let stream = reg
                    .get_mut(sid)
                    .expect("stream not found");
                sync_read(stream, sz)
            });
            match bytes {
                Ok(b) => vm.new_string(String::from_utf8_lossy(&b).into_owned()),
                Err(e) => vm.panic(format!("ReadStream.read: {e}")),
            }
        }

        fn read_all(this) {
            let sid = stream_id(vm, this);
            let bytes = with_streams(|reg| {
                let stream = reg
                    .get_mut(sid)
                    .expect("stream not found");
                sync_read_all(stream)
            });
            match bytes {
                Ok(b) => vm.new_string(String::from_utf8_lossy(&b).into_owned()),
                Err(e) => vm.panic(format!("ReadStream.read_all: {e}")),
            }
        }

        fn close(this) {
            let sid = stream_id(vm, this);
            with_streams(|reg| { reg.remove(sid); });
            TlangValue::Nil
        }
    }

    impl Display for Streams.ReadStream {
        fn to_string(this) {
            let sid = stream_id(vm, this);
            vm.new_string(format!("ReadStream({})", sid))
        }
    }
}

define_struct! {
    struct Streams.WriteStream { id }

    impl Streams.WriteStream {
        fn write(this, data) {
            let sid = stream_id(vm, this);
            let s = vm.stringify(data);
            let result = with_streams(|reg| {
                let stream = reg
                    .get_mut(sid)
                    .expect("stream not found");
                sync_write(stream, s.as_bytes())
            });
            match result {
                Ok(n) => TlangValue::from(n as i64),
                Err(e) => vm.panic(format!("WriteStream.write: {e}")),
            }
        }

        fn flush(this) {
            let sid = stream_id(vm, this);
            let result = with_streams(|reg| {
                let stream = reg
                    .get_mut(sid)
                    .expect("stream not found");
                sync_flush(stream)
            });
            match result {
                Ok(()) => TlangValue::Nil,
                Err(e) => vm.panic(format!("WriteStream.flush: {e}")),
            }
        }

        fn close(this) {
            let sid = stream_id(vm, this);
            with_streams(|reg| { reg.remove(sid); });
            TlangValue::Nil
        }
    }

    impl Display for Streams.WriteStream {
        fn to_string(this) {
            let sid = stream_id(vm, this);
            vm.new_string(format!("WriteStream({})", sid))
        }
    }
}

define_struct! {
    struct Streams.DuplexStream { id }

    impl Streams.DuplexStream {
        fn read(this, size) {
            let sid = stream_id(vm, this);
            let sz = if size.is_nil() { 4096 } else { size.as_usize() };
            let bytes = with_streams(|reg| {
                let stream = reg
                    .get_mut(sid)
                    .expect("stream not found");
                sync_read(stream, sz)
            });
            match bytes {
                Ok(b) => vm.new_string(String::from_utf8_lossy(&b).into_owned()),
                Err(e) => vm.panic(format!("DuplexStream.read: {e}")),
            }
        }

        fn read_all(this) {
            let sid = stream_id(vm, this);
            let bytes = with_streams(|reg| {
                let stream = reg
                    .get_mut(sid)
                    .expect("stream not found");
                sync_read_all(stream)
            });
            match bytes {
                Ok(b) => vm.new_string(String::from_utf8_lossy(&b).into_owned()),
                Err(e) => vm.panic(format!("DuplexStream.read_all: {e}")),
            }
        }

        fn write(this, data) {
            let sid = stream_id(vm, this);
            let s = vm.stringify(data);
            let result = with_streams(|reg| {
                let stream = reg
                    .get_mut(sid)
                    .expect("stream not found");
                sync_write(stream, s.as_bytes())
            });
            match result {
                Ok(n) => TlangValue::from(n as i64),
                Err(e) => vm.panic(format!("DuplexStream.write: {e}")),
            }
        }

        fn flush(this) {
            let sid = stream_id(vm, this);
            let result = with_streams(|reg| {
                let stream = reg
                    .get_mut(sid)
                    .expect("stream not found");
                sync_flush(stream)
            });
            match result {
                Ok(()) => TlangValue::Nil,
                Err(e) => vm.panic(format!("DuplexStream.flush: {e}")),
            }
        }

        fn close(this) {
            let sid = stream_id(vm, this);
            with_streams(|reg| { reg.remove(sid); });
            TlangValue::Nil
        }
    }

    impl Display for Streams.DuplexStream {
        fn to_string(this) {
            let sid = stream_id(vm, this);
            vm.new_string(format!("DuplexStream({})", sid))
        }
    }
}
