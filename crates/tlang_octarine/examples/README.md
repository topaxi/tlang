# Octarine Runtime Examples

Example tlang programs demonstrating the octarine runtime's stream-based I/O.

These examples are designed to run with the `octarine` binary, which provides
the `Streams` module for file, network, and stdio operations.

## Examples

| File | Description |
|---|---|
| `hello_stream.tlang` | Minimal stdout write using the stream API |
| `buffer_stream.tlang` | In-memory duplex streams: write, partial read, read_all |
| `file_io.tlang` | File create, read, append using stream handles |
| `stderr.tlang` | Separating stdout and stderr output |
| `echo.tlang` | Read from stdin, echo to stdout (pipe-friendly) |
| `copy_file.tlang` | Stream-to-stream file copy |
| `line_processing.tlang` | Read CSV, generate a report file |
| `tee.tlang` | Write to multiple destinations (stdout + file + buffer) |

## Running

```bash
# Build the runtime
cargo build --release --bin octarine

# Run an example
./target/release/octarine crates/tlang_octarine/examples/hello_stream.tlang

# Pipe input for the echo example
echo "hello octarine" | ./target/release/octarine crates/tlang_octarine/examples/echo.tlang
```

## Stream API Quick Reference

### Factory Functions

```tlang
Streams::stdin()            // → ReadStream  (fd 0)
Streams::stdout()           // → WriteStream (fd 1)
Streams::stderr()           // → WriteStream (fd 2)
Streams::open_read(path)    // → ReadStream  (file)
Streams::open_write(path)   // → WriteStream (create/truncate)
Streams::open_append(path)  // → WriteStream (create/append)
Streams::buffer()           // → DuplexStream (empty)
Streams::buffer_from(data)  // → DuplexStream (pre-filled)
```

### Methods

```tlang
// ReadStream / DuplexStream
stream.read(size)   // read up to size bytes (default 4096)
stream.read_all()   // read everything remaining
stream.close()

// WriteStream / DuplexStream
stream.write(data)  // write string data, returns bytes written
stream.flush()      // flush buffered output
stream.close()
```
