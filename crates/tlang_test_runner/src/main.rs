use std::fs;
use std::path::Path;
use std::process::Command;

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

    fn enumerate() -> impl Iterator<Item = Backend> {
        vec![Backend::Interpreter, Backend::JavaScript].into_iter()
    }
}

fn main() {
    let pattern = "tests/**/*.tlang";

    for backend in Backend::enumerate() {
        for entry in glob(pattern).expect("Failed to read glob pattern") {
            let file_path = entry.expect("Failed to read test file path");
            if let Err(e) = run_test(&file_path, backend.as_str()) {
                panic!("{}", e);
            }
        }
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

    let output = match backend {
        "interpreter" => Command::new("./target/debug/tlang_interpreter")
            .arg(file_path)
            .output()
            .map_err(|e| format!("Failed to execute interpreter: {}", e))?,
        "javascript" => Command::new("node")
            .arg(file_path.with_extension("js"))
            .output()
            .map_err(|e| format!("Failed to execute JavaScript: {}", e))?,
        _ => return Err(format!("Unsupported backend: {}", backend)),
    };

    let actual_output = String::from_utf8_lossy(&output.stdout);

    if actual_output.trim() == expected_output.trim() {
        Ok(())
    } else {
        Err(format!(
            "Test failed for {} (backend: {}).\nExpected:\n{}\nActual:\n{}",
            file_path.display(),
            backend,
            expected_output,
            actual_output
        ))
    }
}
