//! Asynchronous filesystem operations.
//!
//! All functions offload work to the IO worker (tokio thread) and invoke the
//! callback on the main event-loop thread when complete.

use std::io;
use std::path::PathBuf;

use crate::runtime::Runtime;

/// Read the entire contents of a file as raw bytes.
pub fn read_file(
    runtime: &mut Runtime,
    path: PathBuf,
    callback: impl FnOnce(io::Result<Vec<u8>>, &mut Runtime) + 'static,
) {
    runtime.spawn_io(async move { tokio::fs::read(path).await }, callback);
}

/// Read the entire contents of a file as a UTF-8 string.
pub fn read_to_string(
    runtime: &mut Runtime,
    path: PathBuf,
    callback: impl FnOnce(io::Result<String>, &mut Runtime) + 'static,
) {
    runtime.spawn_io(
        async move { tokio::fs::read_to_string(path).await },
        callback,
    );
}

/// Write `contents` to a file, creating it if it doesn't exist and truncating
/// it if it does.
pub fn write_file(
    runtime: &mut Runtime,
    path: PathBuf,
    contents: Vec<u8>,
    callback: impl FnOnce(io::Result<()>, &mut Runtime) + 'static,
) {
    runtime.spawn_io(
        async move { tokio::fs::write(path, contents).await },
        callback,
    );
}

/// Remove a file.
pub fn remove_file(
    runtime: &mut Runtime,
    path: PathBuf,
    callback: impl FnOnce(io::Result<()>, &mut Runtime) + 'static,
) {
    runtime.spawn_io(async move { tokio::fs::remove_file(path).await }, callback);
}

/// Create a directory and all of its parent components if they are missing.
pub fn create_dir_all(
    runtime: &mut Runtime,
    path: PathBuf,
    callback: impl FnOnce(io::Result<()>, &mut Runtime) + 'static,
) {
    runtime.spawn_io(
        async move { tokio::fs::create_dir_all(path).await },
        callback,
    );
}

/// File metadata returned by [`metadata`].
#[derive(Debug)]
pub struct Metadata {
    pub len: u64,
    pub is_file: bool,
    pub is_dir: bool,
    pub is_symlink: bool,
    pub readonly: bool,
}

impl From<std::fs::Metadata> for Metadata {
    fn from(m: std::fs::Metadata) -> Self {
        Self {
            len: m.len(),
            is_file: m.is_file(),
            is_dir: m.is_dir(),
            is_symlink: m.is_symlink(),
            readonly: m.permissions().readonly(),
        }
    }
}

/// Retrieve metadata for the file at `path`.
pub fn metadata(
    runtime: &mut Runtime,
    path: PathBuf,
    callback: impl FnOnce(io::Result<Metadata>, &mut Runtime) + 'static,
) {
    runtime.spawn_io(
        async move { tokio::fs::metadata(path).await.map(Metadata::from) },
        callback,
    );
}
