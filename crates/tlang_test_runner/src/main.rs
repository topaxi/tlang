use std::path::Path;
use std::process::{Command, Stdio};

use regex::Regex;

#[derive(Clone, Copy)]
enum Backend {
    Interpreter,
    JavaScript,
}

impl Backend {
    fn as_str(&self) -> &'static str {
        match self {
            Backend::Interpreter => "interpreter",
            Backend::JavaScript => "javascript",
        }
    }

    fn values() -> impl Iterator<Item = Backend> {
        [Backend::Interpreter, Backend::JavaScript].into_iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Test all .tlang files using insta's glob functionality
    #[test]
    fn test_all_tlang_files() {
        // Use absolute path to the tests directory
        let current_dir = std::env::current_dir().unwrap();
        let workspace_root = current_dir
            .ancestors()
            .find(|path| path.join("Cargo.toml").exists() && path.join("tests").exists())
            .expect("Could not find workspace root");

        let tests_dir = workspace_root.join("tests");
        let tests_dir_clone = tests_dir.clone();

        insta::glob!(&tests_dir, "**/*.tlang", |path| {
            for backend in Backend::values() {
                let relative_path = path.strip_prefix(&tests_dir_clone).unwrap();
                let test_name = format!(
                    "{}_{}",
                    relative_path
                        .with_extension("")
                        .to_string_lossy()
                        .replace(['/', '\\'], "_"),
                    backend.as_str()
                );

                let is_known_failure = path.to_string_lossy().contains("known_failures");

                // Change to workspace root for command execution
                let original_dir = std::env::current_dir().unwrap();
                std::env::set_current_dir(workspace_root)
                    .expect("Failed to change to workspace root");

                match std::panic::catch_unwind(|| run_test_with_backend(path, backend)) {
                    Ok(output) => {
                        // Restore original directory
                        std::env::set_current_dir(&original_dir)
                            .expect("Failed to restore working directory");

                        if is_known_failure {
                            // For known failures, we still want to capture the snapshot
                            // but we mark it specially and don't fail if it doesn't match
                            insta::with_settings!({
                                description => "Known failure - output may not match expected",
                            }, {
                                insta::assert_snapshot!(test_name, apply_redactions(&output));
                            });
                        } else {
                            insta::assert_snapshot!(test_name, apply_redactions(&output));
                        }
                    }
                    Err(_) => {
                        // Restore original directory even on panic
                        std::env::set_current_dir(&original_dir)
                            .expect("Failed to restore working directory");

                        if is_known_failure {
                            println!(
                                "Known failing test panicked as expected: {}",
                                path.display()
                            );
                            // Create a placeholder snapshot for panicked known failures
                            insta::with_settings!({
                                description => "Known failure - panic during execution",
                            }, {
                                insta::assert_snapshot!(format!("{}_panic", test_name), format!("Test panicked during execution"));
                            });
                        } else {
                            panic!("Test panicked for {}", path.display());
                        }
                    }
                }
            }
        });
    }
}

fn main() {
    println!("Test runner now uses insta snapshots. Run with 'cargo test' instead.");
    println!("Use 'cargo insta review' to review and accept snapshot changes.");
}

fn run_test_with_backend(file_path: &Path, backend: Backend) -> String {
    let start = std::time::Instant::now();
    let exec_start;
    let output = match backend {
        Backend::Interpreter => {
            exec_start = std::time::Instant::now();

            Command::new("./target/release/tlangdi")
                .arg(file_path)
                .output()
                .unwrap_or_else(|e| {
                    panic!(
                        "Failed to execute interpreter for {}: {}",
                        file_path.display(),
                        e
                    )
                })
        }
        Backend::JavaScript => {
            #[allow(clippy::zombie_processes)]
            let tlang_js_compiler_output = Command::new("./target/release/tlang_cli_js")
                .arg(file_path)
                .stdout(Stdio::piped())
                .spawn()
                .unwrap_or_else(|err| {
                    panic!(
                        "Failed to compile to JavaScript for {}\n{}",
                        file_path.display(),
                        err,
                    )
                });

            exec_start = std::time::Instant::now();

            let javascript_output = Command::new("node")
                .stdin(tlang_js_compiler_output.stdout.unwrap())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()
                .unwrap_or_else(|err| {
                    panic!(
                        "Failed to execute JavaScript for {}\n{}",
                        file_path.display(),
                        err
                    )
                });

            javascript_output.wait_with_output().unwrap()
        }
    };
    let elapsed = start.elapsed();
    let exec_elapsed = exec_start.elapsed();

    let actual_output = format!(
        "{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let actual_output = normalize_output(&actual_output);

    println!(
        "Test executed for {} with {} backend in {:?} (total {:?})",
        file_path.display(),
        backend.as_str(),
        exec_elapsed,
        elapsed
    );
    actual_output.trim().to_string()
}

fn normalize_output(output: &str) -> std::borrow::Cow<'_, str> {
    // Normalize rust panic message from
    // thread 'main' (6734) panicked at...
    // to
    // thread 'main' panicked at...
    let re = Regex::new(r"thread '(\w+)' \(\d+\) panicked at").expect("Failed to compile regex");

    re.replace_all(output, "thread '$1' panicked at")
}

fn apply_redactions(output: &str) -> String {
    let mut result = output.to_string();

    // Redact log timestamps that may vary between runs
    let timestamp_re =
        Regex::new(r"\[\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z (WARN|ERROR|INFO|DEBUG)")
            .expect("Failed to compile timestamp redaction regex");
    result = timestamp_re
        .replace_all(&result, "[TIMESTAMP] $1")
        .into_owned();

    // Redact the entire stack backtrace section (including header) that may or may not be present
    // Some environments show backtraces, others don't - remove the section entirely if present
    let backtrace_section_re = Regex::new(r"stack backtrace:\n(   \d+: .+\n)*")
        .expect("Failed to compile backtrace section redaction regex");
    result = backtrace_section_re.replace_all(&result, "").into_owned();

    // Redact numbered stack trace lines that appear with RUST_BACKTRACE=1
    let numbered_trace_re =
        Regex::new(r"  \d+: [^\n]+\n").expect("Failed to compile numbered trace redaction regex");
    result = numbered_trace_re.replace_all(&result, "").into_owned();

    // Normalize backtrace notes that may vary between environments
    let note_re = Regex::new(
        r"note: Some details are omitted, run with `RUST_BACKTRACE=full` for a verbose backtrace\.",
    )
    .expect("Failed to compile backtrace note redaction regex");
    result = note_re
        .replace_all(
            &result,
            "note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace",
        )
        .into_owned();

    // Redact internal compiler symbol dumps that contain varying identifiers
    let symbol_dump_re = Regex::new(r"Available symbols: \[\n(.*?\n)*?\s*\]")
        .expect("Failed to compile symbol dump redaction regex");
    result = symbol_dump_re
        .replace_all(&result, "Available symbols: [INTERNAL_SYMBOL_TABLE]")
        .into_owned();

    // Redact memory state dumps in panic messages that contain varying data structures
    let memory_dump_re = Regex::new(r"Current scope: \[\n(.*?\n)*?\]")
        .expect("Failed to compile memory dump redaction regex");
    result = memory_dump_re
        .replace_all(&result, "Current scope: [INTERNAL_MEMORY_STATE]")
        .into_owned();

    // Redact specific internal IDs that may vary between runs
    let id_tag_re = Regex::new(r"(SymbolIdTag|NodeIdTag|HirIdTag)\(\d+\)")
        .expect("Failed to compile ID tag redaction regex");
    result = id_tag_re.replace_all(&result, "$1([ID])").into_owned();

    // Redact Node.js version numbers that may vary between environments
    let nodejs_version_re = Regex::new(r"Node\.js v\d+\.\d+\.\d+")
        .expect("Failed to compile Node.js version redaction regex");
    result = nodejs_version_re
        .replace_all(&result, "Node.js [VERSION]")
        .into_owned();

    // Comprehensive Node.js syntax error redaction to handle version differences
    // Redact various forms of syntax error details that change between Node.js versions
    let syntax_error_patterns = [
        r"Expected '[^']*'(, '[^']*')* or <eof>",
        r"Unexpected token '[^']*'",
        r"Unexpected (token|identifier|keyword|number|string) '[^']*'",
        r"Unexpected (token|identifier|keyword|number|string) '[^']*' in [^'\n]*",
        r"Missing .*? in .*?",
        r"Invalid .*? in .*?",
        r"Unexpected end of input",
        r"Unexpected end of JSON input",
    ];

    for pattern in &syntax_error_patterns {
        let re = Regex::new(pattern).expect("Failed to compile syntax error redaction regex");
        result = re
            .replace_all(&result, "[NODE_SYNTAX_ERROR_DETAILS]")
            .into_owned();
    }

    // More comprehensive Node.js stack trace redactions
    // Redact various forms of Node.js internal stack traces that may vary between versions
    let stack_patterns = [
        r"    at .* \(node:internal/[^)]+\)",
        r"    at node:internal/[^:]+:\d+:\d+",
        r"    at .* \(internal/[^)]+\)",
        r"    at internal/[^:]+:\d+:\d+",
        r"    at .* \(<anonymous>\)",
        r"    at <anonymous>:\d+:\d+",
    ];

    for pattern in &stack_patterns {
        let re =
            Regex::new(pattern).expect("Failed to compile Node.js internal stack redaction regex");
        result = re
            .replace_all(&result, "    at [NODE_INTERNAL_STACK]")
            .into_owned();
    }

    // Redact Node.js core module stack traces (events, fs, etc.) with more patterns
    let core_patterns = [
        r"    at .* \(node:[^)]+\)",
        r"    at node:[^:]+:\d+:\d+",
        r"    at .* \(events\.js:\d+:\d+\)",
        r"    at .* \(fs\.js:\d+:\d+\)",
        r"    at .* \(util\.js:\d+:\d+\)",
        r"    at .* \(stream\.js:\d+:\d+\)",
    ];

    for pattern in &core_patterns {
        let re = Regex::new(pattern).expect("Failed to compile Node.js core stack redaction regex");
        result = re
            .replace_all(&result, "    at [NODE_CORE_STACK]")
            .into_owned();
    }

    // Redact stdin wrapper stack traces with more comprehensive patterns
    let stdin_patterns = [
        r"    at \[stdin\][^:]*:\d+:\d+",
        r"    at .* \(\[stdin\]:\d+:\d+\)",
        r"    at \[eval\]:\d+:\d+",
        r"    at .* \(\[eval\]:\d+:\d+\)",
    ];

    for pattern in &stdin_patterns {
        let re = Regex::new(pattern).expect("Failed to compile stdin wrapper redaction regex");
        result = re
            .replace_all(&result, "    at [STDIN_WRAPPER]")
            .into_owned();
    }

    // Redact Node.js error codes and additional diagnostics that vary between versions
    let error_code_re =
        Regex::new(r"\s+code: '[^']*'").expect("Failed to compile error code redaction regex");
    result = error_code_re
        .replace_all(&result, " code: '[ERROR_CODE]'")
        .into_owned();

    // Redact process exit information that may vary
    let exit_info_re = Regex::new(r"Process exited with code \d+")
        .expect("Failed to compile exit code redaction regex");
    result = exit_info_re
        .replace_all(&result, "Process exited with code [EXIT_CODE]")
        .into_owned();

    // Redact Node.js module resolution paths that may vary between versions
    let module_path_re = Regex::new(r"Cannot resolve module '[^']*' from '[^']*'")
        .expect("Failed to compile module path redaction regex");
    result = module_path_re
        .replace_all(&result, "Cannot resolve module '[MODULE]' from '[PATH]'")
        .into_owned();

    // Redact additional error context lines that might vary between Node.js versions
    let error_context_re =
        Regex::new(r"    \^+\s*$").expect("Failed to compile error context redaction regex");
    result = error_context_re
        .replace_all(&result, "    [ERROR_CONTEXT]")
        .into_owned();

    result
}
