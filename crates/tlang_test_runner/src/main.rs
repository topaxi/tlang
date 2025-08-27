use std::path::Path;
use std::process::{Command, Stdio};
use std::sync::LazyLock;

use regex::Regex;

// Static regex patterns for redactions
static THREAD_PANIC_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"thread '(\w+)' \(\d+\) panicked at").expect("Failed to compile regex")
});

static TIMESTAMP_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"\[\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z (WARN|ERROR|INFO|DEBUG)")
        .expect("Failed to compile timestamp redaction regex")
});

static BACKTRACE_SECTION_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"stack backtrace:\n(   \d+: .+\n)*")
        .expect("Failed to compile backtrace section redaction regex")
});

static NUMBERED_TRACE_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"  \d+: [^\n]+\n").expect("Failed to compile numbered trace redaction regex")
});

static NOTE_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(
        r"note: Some details are omitted, run with `RUST_BACKTRACE=full` for a verbose backtrace\.",
    )
    .expect("Failed to compile backtrace note redaction regex")
});

static SYMBOL_DUMP_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"Available symbols: \[\n(.*?\n)*?\s*\]")
        .expect("Failed to compile symbol dump redaction regex")
});

static MEMORY_DUMP_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"Current scope: \[\n(.*?\n)*?\]")
        .expect("Failed to compile memory dump redaction regex")
});

static ID_TAG_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"(SymbolIdTag|NodeIdTag|HirIdTag)\(\d+\)")
        .expect("Failed to compile ID tag redaction regex")
});

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
        // Verify Node.js version before running JavaScript tests
        check_nodejs_version();

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

                let output_result = run_test_with_backend(path, backend);

                // Restore original directory
                std::env::set_current_dir(&original_dir)
                    .expect("Failed to restore working directory");

                match output_result {
                    Ok(output) => {
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
                    Err(error_msg) => {
                        if is_known_failure {
                            println!("Known failing test failed as expected: {}", path.display());
                            // Create a snapshot for the error output
                            insta::with_settings!({
                                description => "Known failure - error during execution",
                            }, {
                                insta::assert_snapshot!(format!("{}_error", test_name), error_msg);
                            });
                        } else {
                            panic!("Test failed for {}: {}", path.display(), error_msg);
                        }
                    }
                }
            }
        });
    }
}

fn main() {
    println!("Test runner now uses insta snapshots.");
    println!("Recommended: Use 'make test' to run all tests.");
    println!(
        "Use 'make test-review' to review snapshot changes, and 'make test-accept' to accept them."
    );
    println!(
        "(Advanced: You can also use 'cargo test', 'cargo insta review', and 'cargo insta accept' directly.)"
    );
}

fn check_nodejs_version() {
    // Read the Node.js version from the workspace package.json file
    let current_dir = std::env::current_dir().unwrap();
    let workspace_root = current_dir
        .ancestors()
        .find(|path| path.join("Cargo.toml").exists() && path.join("package.json").exists())
        .expect("Could not find workspace root with package.json");

    let package_json_path = workspace_root.join("package.json");
    let package_json_content =
        std::fs::read_to_string(&package_json_path).expect("Failed to read package.json");

    let package_json: serde_json::Value =
        serde_json::from_str(&package_json_content).expect("Failed to parse package.json");

    let required_version = package_json
        .get("engines")
        .and_then(|engines| engines.get("node"))
        .and_then(|node| node.as_str())
        .expect("Failed to read Node.js version from package.json engines field");

    // Enforce the exact Node.js version specified in package.json instead of using redactions
    // to normalize output differences between Node.js versions. This ensures consistent test
    // results by requiring developers to use the exact Node.js version the project targets.
    let output = Command::new("node")
        .arg("--version")
        .output()
        .expect("Failed to execute 'node --version'. Make sure Node.js is installed.");

    let version_output = String::from_utf8_lossy(&output.stdout);
    let version = version_output
        .trim()
        .strip_prefix('v')
        .unwrap_or(&version_output.trim());

    if version != required_version {
        panic!(
            "Node.js version {} is required (as specified in package.json), but found version {}. \
             Please install Node.js {} or use a version manager like volta to switch to the required version.",
            required_version, version, required_version
        );
    }

    println!("✓ Node.js version {} verified", required_version);
}

fn run_test_with_backend(file_path: &Path, backend: Backend) -> Result<String, String> {
    let start = std::time::Instant::now();
    let exec_start;
    let output = match backend {
        Backend::Interpreter => {
            exec_start = std::time::Instant::now();

            Command::new("./target/release/tlangdi")
                .arg(file_path)
                .output()
                .map_err(|e| {
                    format!(
                        "Failed to execute interpreter for {}: {}",
                        file_path.display(),
                        e
                    )
                })?
        }
        Backend::JavaScript => {
            #[allow(clippy::zombie_processes)]
            let tlang_js_compiler_output = Command::new("./target/release/tlang_cli_js")
                .arg(file_path)
                .stdout(Stdio::piped())
                .spawn()
                .map_err(|err| {
                    format!(
                        "Failed to compile to JavaScript for {}\n{}",
                        file_path.display(),
                        err,
                    )
                })?;

            exec_start = std::time::Instant::now();

            let javascript_output = Command::new("node")
                .stdin(tlang_js_compiler_output.stdout.unwrap())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()
                .map_err(|err| {
                    format!(
                        "Failed to execute JavaScript for {}\n{}",
                        file_path.display(),
                        err
                    )
                })?;

            javascript_output.wait_with_output().map_err(|e| {
                format!(
                    "Failed to get JavaScript output for {}: {}",
                    file_path.display(),
                    e
                )
            })?
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
    Ok(actual_output.trim().to_string())
}

fn normalize_output(output: &str) -> std::borrow::Cow<'_, str> {
    // Normalize rust panic message from
    // thread 'main' (6734) panicked at...
    // to
    // thread 'main' panicked at...
    THREAD_PANIC_RE.replace_all(output, "thread '$1' panicked at")
}

fn apply_redactions(output: &str) -> String {
    let mut result = output.to_string();

    // Redact log timestamps that may vary between runs
    result = TIMESTAMP_RE
        .replace_all(&result, "[TIMESTAMP] $1")
        .into_owned();

    // Redact the entire stack backtrace section (including header) that may or may not be present
    // Some environments show backtraces, others don't - remove the section entirely if present
    result = BACKTRACE_SECTION_RE.replace_all(&result, "").into_owned();

    // Redact numbered stack trace lines that appear with RUST_BACKTRACE=1
    result = NUMBERED_TRACE_RE.replace_all(&result, "").into_owned();

    // Normalize backtrace notes that may vary between environments
    result = NOTE_RE
        .replace_all(
            &result,
            "note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace",
        )
        .into_owned();

    // Redact internal compiler symbol dumps that contain varying identifiers
    result = SYMBOL_DUMP_RE
        .replace_all(&result, "Available symbols: [INTERNAL_SYMBOL_TABLE]")
        .into_owned();

    // Redact memory state dumps in panic messages that contain varying data structures
    result = MEMORY_DUMP_RE
        .replace_all(&result, "Current scope: [INTERNAL_MEMORY_STATE]")
        .into_owned();

    // Redact specific internal IDs that may vary between runs
    result = ID_TAG_RE.replace_all(&result, "$1([ID])").into_owned();

    result
}
