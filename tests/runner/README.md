# tlang_test_runner

This crate contains the integration test runner for tlang. It is a standalone executable that orchestrates the execution of the compiler and interpreter against a suite of `.tlang` test files.

## How it works

The runner finds all `.tlang` files in the `tests/` directory and executes them using two backends:
1.  **Interpreter**: Runs the code directly using the `tlangdi` binary.
2.  **JavaScript**: Compiles the code using `tlang compile` and then executes the resulting JavaScript with `node`.

It uses `insta` for snapshot testing to verify that the output (stdout/stderr) matches the expected output.

## Usage

The recommended way to run tests is via the `Makefile` in the project root:

```bash
make test
```

This ensures that the `tlang` and `tlangdi` binaries are built and up-to-date before running the tests.

To update snapshots after making changes:

```bash
make test-review  # Interactive review
make test-accept  # Accept all changes
```

## Structure

*   `src/main.rs`: Contains the test logic, including:
    *   Finding test files.
    *   Running the interpreter and compiler/node.
    *   Normalizing output (redacting timestamps, memory addresses, etc.).
    *   Comparing with snapshots.
