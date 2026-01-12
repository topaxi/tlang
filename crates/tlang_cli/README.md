# tlang_cli

The unified command-line interface for tlang.

## Commands

*   `tlang compile <file>`: Compiles tlang source code.
    *   Flags: `--target <target>` (e.g. `js`), `--output-format <format>` (source, hir, ast).
*   `tlang run <file>`: Interprets tlang source code directly.

## Structure

*   `src/main.rs`: Entry point and argument parsing.
*   `src/commands/`: Implementation of specific commands.
*   `src/error.rs`: CLI-specific error handling.
