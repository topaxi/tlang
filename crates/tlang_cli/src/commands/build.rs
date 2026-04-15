use std::path::Path;

use tlang_codegen_js::CodegenError;
use tlang_codegen_js::generator::CodegenJS;
use tlang_diagnostics::render_ice;
use tlang_modules::{CompiledModule, ModulePath, MultiModuleCompileResult, compile_project};
use tlang_typeck::typecheck_module;

use crate::commands::compile::CompileTargetHirOptimizer;
use crate::commands::{print_source_diagnostics, run_hir_passes};

#[derive(Debug)]
pub struct BuildOptions {
    pub project_dir: String,
    pub output_file: Option<String>,
}

/// Render codegen errors without source context (source text is not available
/// in the multi-module build pipeline).  Outputs one line per error to stderr.
fn render_codegen_errors(module_path: &ModulePath, errors: &[CodegenError]) -> String {
    let mut out = String::new();
    for e in errors {
        out.push_str(&format!(
            "error: {}\n  --> {}:{}:{}\n",
            e.message,
            module_path,
            e.span.start_lc.line + 1,
            e.span.start_lc.column + 1,
        ));
    }
    out
}

pub fn handle_build(options: &BuildOptions) -> bool {
    let project_dir = Path::new(&options.project_dir);

    if !project_dir.join("src").join("lib.tlang").exists() {
        eprintln!(
            "Error: no `src/lib.tlang` found in `{}`",
            project_dir.display()
        );
        return false;
    }

    let builtin_symbols = CodegenJS::get_standard_library_symbols();

    let result = match compile_project(project_dir, builtin_symbols) {
        Ok(result) => result,
        Err(err) => {
            eprint!("{}", crate::commands::error::render_compile_error(&err));
            return false;
        }
    };

    // Collect all protocol names across modules for codegen
    let protocol_names = result.protocol_names();

    // Generate JS for each module and bundle
    let stdlib = {
        static STDLIB: std::sync::OnceLock<String> = std::sync::OnceLock::new();
        STDLIB
            .get_or_init(CodegenJS::compile_stdlib_module)
            .as_str()
    };
    let mut output_parts = vec![stdlib.to_string()];

    // Generate code for non-root modules first (they define exported functions)
    for (path, compiled) in &result.modules {
        if path == &ModulePath::root() {
            continue;
        }

        let module_code = match generate_module_code(path, compiled, &result, &protocol_names) {
            Some(module_code) => module_code,
            None => return false,
        };
        if !module_code.trim().is_empty() {
            output_parts.push(format!("// module: {path}\n{module_code}"));
        }
    }

    // Generate import alias bindings (e.g., `const mul = multiply;`)
    // Deduplicated — re-exports can cause identical bindings from multiple modules.
    let mut alias_bindings = Vec::new();
    let mut seen_aliases = std::collections::HashSet::new();
    for resolved in result.imports.values() {
        for sym in resolved.symbols.values() {
            if sym.local_name != sym.original_name {
                let binding = format!("const {} = {};", sym.local_name, sym.original_name);
                if seen_aliases.insert(binding.clone()) {
                    alias_bindings.push(binding);
                }
            }
        }
    }
    if !alias_bindings.is_empty() {
        output_parts.push(alias_bindings.join("\n"));
    }

    // Generate code for root module last
    if let Some(compiled) = result.modules.get(&ModulePath::root()) {
        let root_code =
            match generate_module_code(&ModulePath::root(), compiled, &result, &protocol_names) {
                Some(root_code) => root_code,
                None => return false,
            };

        if !root_code.trim().is_empty() {
            output_parts.push(root_code);
        }
    }

    let output = output_parts.join("\n");

    if let Some(output_file) = &options.output_file {
        if let Err(e) = std::fs::write(output_file, &output) {
            eprintln!("Error writing output: {e}");
            return false;
        }
    } else {
        println!("{output}");
    }

    true
}

fn generate_module_code(
    path: &ModulePath,
    compiled: &CompiledModule,
    result: &MultiModuleCompileResult,
    protocol_names: &[String],
) -> Option<String> {
    let mut hir_module = compiled.hir.clone();
    let mut optimizer =
        CompileTargetHirOptimizer::JavaScript(tlang_codegen_js::js_hir_opt::JsHirOptimizer::default());
    let mut ctx = compiled.lower_meta.clone().into();
    if let Err(err) = run_hir_passes(&mut optimizer, &mut hir_module, &mut ctx) {
        eprint!("{}", render_ice(&err));
        return None;
    }

    let diagnostics = match typecheck_module(&mut hir_module, &mut ctx) {
        Ok(diagnostics) => diagnostics,
        Err(err) => {
            eprint!("{}", render_ice(&err));
            return None;
        }
    };

    if let Some(parsed_module) = result.graph.modules.get(path) {
        let source_name = parsed_module.file_path.to_string_lossy();
        print_source_diagnostics(&source_name, &parsed_module.source, &diagnostics.warnings);
        if diagnostics.has_errors() {
            print_source_diagnostics(&source_name, &parsed_module.source, &diagnostics.errors);
            return None;
        }
    }

    let mut codegen = CodegenJS::default();
    codegen.set_bundle_mode(true);
    for name in protocol_names {
        codegen.register_protocol(name);
    }
    if let Err(errors) = codegen.generate_code(&hir_module) {
        eprint!("{}", render_codegen_errors(path, &errors));
        return None;
    }

    Some(codegen.get_output().to_string())
}
