use std::path::PathBuf;
use tlang_codegen_js::generator::CodegenJS;
use tlang_codegen_js::js_hir_opt::JsHirOptimizer;
use tlang_defs::DefKind;
use tlang_hir_opt::hir_opt::HirOptContext;
use tlang_modules::{ModulePath, compile_project};

fn test_project_dir(name: &str) -> PathBuf {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    manifest_dir
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("tests")
        .join("modules")
        .join(name)
}

fn builtin_symbols() -> &'static [(&'static str, DefKind)] {
    CodegenJS::get_standard_library_symbols()
}

fn compile_project_to_js(project_name: &str) -> String {
    let dir = test_project_dir(project_name);
    let result = compile_project(&dir, builtin_symbols()).expect("compilation should succeed");

    let stdlib = CodegenJS::get_precompiled_stdlib_module();
    let mut parts = vec![stdlib.to_string()];

    // Non-root modules first
    for (path, compiled) in &result.modules {
        if path == &ModulePath::root() {
            continue;
        }

        let mut hir_module = compiled.hir.clone();
        let mut optimizer = JsHirOptimizer::default();
        let mut ctx: HirOptContext = compiled.lower_meta.clone().into();
        optimizer.optimize_hir(&mut hir_module, &mut ctx);

        let mut codegen = CodegenJS::default();
        codegen.set_bundle_mode(true);
        codegen.generate_code(&hir_module);
        let code = codegen.get_output().to_string();

        if !code.trim().is_empty() {
            parts.push(format!("// module: {path}\n{code}"));
        }
    }

    // Alias bindings
    let mut aliases = Vec::new();
    for resolved in result.imports.values() {
        for sym in resolved.symbols.values() {
            if sym.local_name != sym.original_name {
                aliases.push(format!("const {} = {};", sym.local_name, sym.original_name));
            }
        }
    }
    if !aliases.is_empty() {
        parts.push(aliases.join("\n"));
    }

    // Root module
    if let Some(compiled) = result.modules.get(&ModulePath::root()) {
        let mut hir_module = compiled.hir.clone();
        let mut optimizer = JsHirOptimizer::default();
        let mut ctx: HirOptContext = compiled.lower_meta.clone().into();
        optimizer.optimize_hir(&mut hir_module, &mut ctx);

        let mut codegen = CodegenJS::default();
        codegen.set_bundle_mode(true);
        codegen.generate_code(&hir_module);
        parts.push(codegen.get_output().to_string());
    }

    parts.join("\n")
}

fn run_js(code: &str) -> String {
    use std::process::Command;
    let output = Command::new("node")
        .arg("--input-type=module")
        .arg("-e")
        .arg(code)
        .output()
        .expect("failed to run node");

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        panic!("Node.js execution failed:\n{stderr}");
    }

    String::from_utf8(output.stdout).unwrap()
}

#[test]
fn test_basic_import() {
    let js = compile_project_to_js("basic_import");
    let output = run_js(&js);
    assert_eq!(output.trim(), "5");
}

#[test]
fn test_grouped_imports() {
    let js = compile_project_to_js("grouped_imports");
    let output = run_js(&js);
    assert_eq!(output.trim(), "5\n20");
}

#[test]
fn test_alias_import() {
    let js = compile_project_to_js("alias_import");
    let output = run_js(&js);
    assert_eq!(output.trim(), "3\n12");
}

#[test]
fn test_nested_modules() {
    let js = compile_project_to_js("nested_modules");
    let output = run_js(&js);
    assert_eq!(output.trim(), "30");
}

#[test]
fn test_cross_module_calls() {
    let js = compile_project_to_js("cross_module_calls");
    let output = run_js(&js);
    assert_eq!(output.trim(), "14");
}

// === Interpreter tests ===

fn run_interpreter(project_name: &str) -> String {
    use std::process::Command;
    let dir = test_project_dir(project_name);
    let lib_path = dir.join("src").join("lib.tlang");

    // Find the tlang binary in target directory
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let workspace_root = manifest_dir.parent().unwrap().parent().unwrap();
    let binary = workspace_root.join("target").join("release").join("tlang");
    let binary = if binary.exists() {
        binary
    } else {
        workspace_root.join("target").join("debug").join("tlang")
    };

    let output = Command::new(&binary)
        .arg("run")
        .arg(&lib_path)
        .output()
        .unwrap_or_else(|e| panic!("failed to run tlang at {}: {e}", binary.display()));

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        panic!("Interpreter execution failed for {project_name}:\n{stderr}");
    }

    String::from_utf8(output.stdout).unwrap()
}

#[test]
fn test_interpreter_basic_import() {
    let output = run_interpreter("basic_import");
    assert_eq!(output.trim(), "5");
}

#[test]
fn test_interpreter_grouped_imports() {
    let output = run_interpreter("grouped_imports");
    assert_eq!(output.trim(), "5\n20");
}

#[test]
fn test_interpreter_alias_import() {
    let output = run_interpreter("alias_import");
    assert_eq!(output.trim(), "3\n12");
}

#[test]
fn test_interpreter_nested_modules() {
    let output = run_interpreter("nested_modules");
    assert_eq!(output.trim(), "30");
}

#[test]
fn test_interpreter_cross_module_calls() {
    let output = run_interpreter("cross_module_calls");
    assert_eq!(output.trim(), "14");
}
