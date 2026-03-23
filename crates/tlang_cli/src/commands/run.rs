use std::io::IsTerminal;
use std::path::Path;
use std::{fs::File, io::Read};

use tlang_ast_lowering::lower_to_hir;
use tlang_core::{memory::TlangValue, vm::VM};
use tlang_diagnostics::{render_parse_issues, render_semantic_diagnostics};
use tlang_hir as hir;
use tlang_hir_opt::HirOptimizer;
use tlang_modules::{ModulePath, compile_project_with_slots};
use tlang_semantics::SemanticAnalyzer;

pub fn handle_run(input_file: &str) {
    let path = Path::new(input_file);

    // Detect project mode: if input is src/lib.tlang or a directory with src/lib.tlang
    if let Some(project_dir) = detect_project_dir(path) {
        handle_run_project(&project_dir);
    } else {
        handle_run_single_file(input_file);
    }
}

/// Detect if the given path is part of a multi-module project.
/// Returns the project root directory if it is.
fn detect_project_dir(path: &Path) -> Option<std::path::PathBuf> {
    let path = std::fs::canonicalize(path).ok()?;

    // If it's a directory, check for src/lib.tlang
    if path.is_dir() && path.join("src").join("lib.tlang").exists() {
        return Some(path);
    }

    // If it's src/lib.tlang, the project root is the parent of src/
    if path.is_file()
        && path.file_name().and_then(|n| n.to_str()) == Some("lib.tlang")
        && let Some(src_dir) = path.parent()
        && src_dir.file_name().and_then(|n| n.to_str()) == Some("src")
    {
        return src_dir.parent().map(|p| p.to_path_buf());
    }

    None
}

fn handle_run_project(project_dir: &Path) {
    let builtin_symbols = VM::builtin_symbols();
    let result = match compile_project_with_slots(project_dir, &builtin_symbols) {
        Ok(result) => result,
        Err(err) => {
            eprint!("{}", crate::commands::error::render_compile_error(&err));
            std::process::exit(1);
        }
    };

    let mut vm = VM::new();

    // Extend global slots for cross-module imports
    vm.extend_global_slots(
        result
            .import_slots
            .iter()
            .map(|((_, name), &slot)| (name.clone(), slot)),
    );

    // Register constant pools from all modules
    for compiled in result.modules.values() {
        vm.state_mut()
            .register_constant_pool_ids(compiled.constant_pool_ids.clone());
    }

    // Execute non-root modules first (register declarations)
    for (path, compiled) in &result.modules {
        if path == &ModulePath::root() {
            continue;
        }

        let mut hir_module = compiled.hir.clone();
        let mut optimizer = HirOptimizer::default();
        let mut ctx = compiled.lower_meta.clone().into();
        optimizer.optimize_hir(&mut hir_module, &mut ctx);

        vm.eval_module(&hir_module);

        // Copy exported function values to the global slots used by importing modules
        populate_import_slots(&mut vm, path, &result);
    }

    // Execute root module
    if let Some(compiled) = result.modules.get(&ModulePath::root()) {
        let mut hir_module = compiled.hir.clone();
        let mut optimizer = HirOptimizer::default();
        let mut ctx = compiled.lower_meta.clone().into();
        optimizer.optimize_hir(&mut hir_module, &mut ctx);

        // Copy exported function values for root module imports
        populate_import_slots(&mut vm, &ModulePath::root(), &result);

        let result = vm.eval_module(&hir_module);
        match result {
            TlangValue::Nil => {}
            _ => println!("{result}"),
        }
    }
}

/// After evaluating a source module, copy its exported values into the global
/// slots that importing modules expect.
fn populate_import_slots(
    vm: &mut VM,
    evaluated_module: &ModulePath,
    compile_result: &tlang_modules::MultiModuleCompileResult,
) {
    for (importing_module, resolved) in &compile_result.imports {
        for (local_name, sym) in &resolved.symbols {
            if &sym.source_module != evaluated_module {
                continue;
            }
            // Look up the value by its original name in the VM globals
            let value = vm.state().get_global(&sym.original_name);
            if let Some(value) = value
                && let Some(&slot) = compile_result
                    .import_slots
                    .get(&(importing_module.clone(), local_name.clone()))
            {
                vm.state_mut().set_global_slot(slot, value);
            }
        }
    }
}

fn handle_run_single_file(input_file: &str) {
    let (module, constant_pool_ids) = compile(input_file);

    let mut vm = VM::new();
    vm.state_mut().register_constant_pool_ids(constant_pool_ids);
    let result = vm.eval(&module);

    match result {
        TlangValue::Nil => {}
        _ => println!("{result}"),
    }
}

fn compile(input_file: &str) -> (hir::Module, std::collections::HashSet<tlang_hir::HirId>) {
    let path = Path::new(input_file);
    let mut file = match File::open(path) {
        Err(why) => panic!("couldn't open {}: {}", path.display(), why),
        Ok(file) => file,
    };
    let mut source = String::new();
    if let Err(why) = file.read_to_string(&mut source) {
        panic!("couldn't read {}: {}", path.display(), why)
    }

    let mut parser = tlang_parser::Parser::from_source(&source);
    let (mut ast, parse_meta) = match parser.parse() {
        Ok(result) => result,
        Err(err) => {
            eprint!(
                "{}",
                render_parse_issues(
                    &path.to_string_lossy(),
                    &source,
                    err.issues(),
                    std::io::stderr().is_terminal()
                )
            );
            std::process::exit(1);
        }
    };

    let mut semantic_analyzer = SemanticAnalyzer::default();
    semantic_analyzer.add_builtin_symbols_with_slots(&VM::builtin_symbols());
    if let Err(err) = semantic_analyzer.analyze(&mut ast) {
        eprint!(
            "{}",
            render_semantic_diagnostics(
                &path.to_string_lossy(),
                &source,
                &err,
                std::io::stderr().is_terminal()
            )
        );
        std::process::exit(1);
    }

    let warnings = semantic_analyzer
        .get_diagnostics()
        .into_iter()
        .filter(|d| d.is_warning())
        .collect::<Vec<_>>();
    if !warnings.is_empty() {
        eprint!(
            "{}",
            render_semantic_diagnostics(
                &path.to_string_lossy(),
                &source,
                &warnings,
                std::io::stderr().is_terminal()
            )
        );
    }

    let (mut module, meta) = lower_to_hir(
        &ast,
        &parse_meta.constant_pool_node_ids,
        semantic_analyzer.symbol_id_allocator(),
        semantic_analyzer.root_symbol_table(),
        semantic_analyzer.symbol_tables().clone(),
    );

    let mut optimizer = HirOptimizer::default();
    let constant_pool_ids = meta.constant_pool_ids.clone();
    let mut ctx = meta.into();
    optimizer.optimize_hir(&mut module, &mut ctx);

    (module, constant_pool_ids)
}
