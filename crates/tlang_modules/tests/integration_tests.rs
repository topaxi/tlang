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

    // Collect all protocol names across modules for codegen
    let protocol_names = result.protocol_names();

    let stdlib = {
        static STDLIB: std::sync::OnceLock<String> = std::sync::OnceLock::new();
        STDLIB.get_or_init(CodegenJS::compile_stdlib_module).clone()
    };
    let mut parts = vec![stdlib];

    // Non-root modules first
    for (path, compiled) in &result.modules {
        if path == &ModulePath::root() {
            continue;
        }

        let mut hir_module = compiled.hir.clone();
        let mut optimizer = JsHirOptimizer::default();
        let mut ctx: HirOptContext = compiled.lower_meta.clone().into();
        optimizer
            .optimize_hir(&mut hir_module, &mut ctx)
            .expect("HIR optimization failed");

        let mut codegen = CodegenJS::default();
        codegen.set_bundle_mode(true);
        for name in &protocol_names {
            codegen.register_protocol(name);
        }
        codegen
            .generate_code(&hir_module)
            .expect("codegen should succeed");
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
        optimizer
            .optimize_hir(&mut hir_module, &mut ctx)
            .expect("HIR optimization failed");

        let mut codegen = CodegenJS::default();
        codegen.set_bundle_mode(true);
        for name in &protocol_names {
            codegen.register_protocol(name);
        }
        codegen
            .generate_code(&hir_module)
            .expect("codegen should succeed");
        parts.push(codegen.get_output().to_string());
    }

    parts.join("\n")
}

fn run_js(code: &str) -> String {
    use std::process::Command;
    let output = Command::new("node")
        .arg("--harmony-temporal")
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

// === JS compilation tests ===

#[test]
fn test_private_helpers() {
    let js = compile_project_to_js("private_helpers");
    let output = run_js(&js);
    assert_eq!(output.trim(), "120\n3628800\n55\n6765");
}

#[test]
fn test_cross_module_enum() {
    let js = compile_project_to_js("cross_module_enum");
    let output = run_js(&js);
    assert_eq!(output.trim(), "circle(5)\n75\nrect(3,4)\n12");
}

#[test]
fn test_cross_module_protocol() {
    let js = compile_project_to_js("cross_module_protocol");
    let output = run_js(&js);
    assert_eq!(output.trim(), "Woof! I'm Rex\nMeow! I'm Whiskers");
}

#[test]
fn test_pipeline_chain() {
    let js = compile_project_to_js("pipeline_chain");
    let output = run_js(&js);
    assert_eq!(output.trim(), "121\n35\n200");
}

#[test]
fn test_tail_recursion_cross_module() {
    let js = compile_project_to_js("tail_recursion_cross_module");
    let output = run_js(&js);
    assert_eq!(output.trim(), "5050\n50005000\n1024\n243");
}

#[test]
fn test_guards_cross_module() {
    let js = compile_project_to_js("guards_cross_module");
    let output = run_js(&js);
    assert_eq!(
        output.trim(),
        "negative\nzero\npositive\n1\nfizz\nbuzz\nfizzbuzz"
    );
}

#[test]
fn test_struct_methods_cross_module() {
    let js = compile_project_to_js("struct_methods_cross_module");
    let output = run_js(&js);
    assert_eq!(output.trim(), "3\n4\n7\n13\n24\n37");
}

#[test]
fn test_multi_module_composition() {
    let js = compile_project_to_js("multi_module_composition");
    let output = run_js(&js);
    assert_eq!(output.trim(), "42\n42\nababab\n30");
}
