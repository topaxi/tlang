use std::fs;
use std::path::Path;
use std::process::{Command, Stdio};

use glob::glob;

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
        vec![Backend::Interpreter, Backend::JavaScript].into_iter()
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let pattern = "tests/**/*.tlang";
    let mut errors: Vec<_> = vec![];

    for (i, backend) in Backend::values().enumerate() {
        if i > 0 {
            println!();
        }

        println!("Running tests with backend: {}", backend.as_str());

        for entry in glob(pattern).expect("Failed to read glob pattern") {
            let file_path = entry.expect("Failed to read test file path");

            match run_test(&file_path, backend.as_str()) {
                Ok(()) => {}
                Err(e) => {
                    errors.push(e);
                }
            }
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors.join("\n").into())
    }
}

fn run_test(file_path: &Path, backend: &str) -> Result<(), String> {
    let expected_output_path = {
        let base_path = file_path.with_extension(""); // Strip `.tlang` extension
        let backend_path = base_path.with_extension(format!("expected.{}.txt", backend));
        if backend_path.exists() {
            backend_path
        } else {
            base_path.with_extension("expected.txt") // Fallback to default
        }
    };

    let expected_output = fs::read_to_string(&expected_output_path)
        .map_err(|e| format!("Failed to read {}: {}", expected_output_path.display(), e))?;

    let start = std::time::Instant::now();
    let exec_start;
    let output = match backend {
        "interpreter" => {
            exec_start = std::time::Instant::now();

            Command::new("./target/release/tlangdi")
                .arg(file_path)
                .output()
                .map_err(|e| format!("Failed to execute interpreter: {}", e))?
        }
        "javascript" => {
            #[allow(clippy::zombie_processes)]
            let tlang_js_compiler_output = Command::new("./target/release/tlang_cli_js")
                .arg(file_path)
                .stdout(Stdio::piped())
                .spawn()
                .expect("Failed to compile to JavaScript");

            exec_start = std::time::Instant::now();

            let javascript_output = Command::new("node")
                .stdin(tlang_js_compiler_output.stdout.unwrap())
                .stdout(Stdio::piped())
                .spawn()
                .expect("Failed to execute JavaScript");

            javascript_output.wait_with_output().unwrap()
        }
        _ => return Err(format!("Unsupported backend: {}", backend)),
    };
    let elapsed = start.elapsed();
    let exec_elapsed = exec_start.elapsed();

    let actual_output = if output.status.success() {
        String::from_utf8_lossy(&output.stdout)
    } else {
        String::from_utf8_lossy(&output.stderr)
    };

    if actual_output.trim() == expected_output.trim() {
        println!(
            "Test passed for {} in {:?} (total {:?})",
            file_path.display(),
            exec_elapsed,
            elapsed
        );
        Ok(())
    } else {
        println!(
            "Test failed for {}.\nExpected:\n{}\nActual:\n{}",
            file_path.display(),
            expected_output,
            actual_output
        );
        Err("Test failed".to_string())
    }
}
