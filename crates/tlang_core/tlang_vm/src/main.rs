use std::env;
use std::fs;
use std::io::IsTerminal;
use std::process;

use tlang_ast_lowering::lower_to_hir;
use tlang_diagnostics::{
    render_diagnostics, render_ice, render_parse_issues, render_semantic_diagnostics,
};
use tlang_hir_opt::{HirOptimizer, HirPass};
use tlang_semantics::SemanticAnalyzer;
use tlang_semantics::diagnostic::Diagnostic as SemanticDiagnostic;
use tlang_typeck::typecheck_module;
use tlang_vm::VM;

fn main() {
    let stress_gc = env::var("TLANG_STRESS_GC").is_ok();

    env_logger::builder()
        .filter_level(log::LevelFilter::Warn)
        .parse_default_env()
        .init();

    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file.tlang>", args[0]);
        process::exit(1);
    }

    let filename = &args[1];

    if !filename.ends_with(".tlang") {
        eprintln!("Error: The file must have a '.tlang' extension.");
        process::exit(1);
    }

    let code = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file '{}': {}", filename, err);
            process::exit(1);
        }
    };

    let mut parser = tlang_parser::Parser::from_source(&code);
    let (mut ast, parse_meta) = match parser.parse() {
        Ok(result) => result,
        Err(err) => {
            eprint!(
                "{}",
                render_parse_issues(
                    filename,
                    &code,
                    err.issues(),
                    std::io::stderr().is_terminal()
                )
            );
            process::exit(1);
        }
    };
    let mut analyzer = SemanticAnalyzer::default();
    analyzer.add_builtin_symbols_with_slots(&VM::builtin_symbols());

    match analyzer.analyze(&mut ast) {
        Ok(_) => {}
        Err(diagnostics) => {
            eprint!(
                "{}",
                render_semantic_diagnostics(
                    filename,
                    &code,
                    &diagnostics,
                    std::io::stderr().is_terminal()
                )
            );

            if diagnostics.iter().any(SemanticDiagnostic::is_error) {
                process::exit(1);
            }
        }
    }
    let (mut module, meta) = lower_to_hir(
        &ast,
        &parse_meta.constant_pool_node_ids,
        analyzer.symbol_id_allocator(),
        analyzer.root_symbol_table(),
        analyzer.symbol_tables().clone(),
    )
    .unwrap_or_else(|errs| {
        for err in &errs {
            eprintln!("error: {err}");
        }
        process::exit(1);
    });

    let mut optimizer = HirOptimizer::default();
    let constant_pool_ids = meta.constant_pool_ids.clone();
    let mut ctx = meta.into();
    if let Err(err) = optimizer.optimize_hir(&mut module, &mut ctx) {
        eprint!("{}", render_ice(&err));
        process::exit(1);
    }

    let diagnostics = match typecheck_module(&mut module, &mut ctx) {
        Ok(diagnostics) => diagnostics,
        Err(err) => {
            eprint!("{}", render_ice(&err));
            process::exit(1);
        }
    };

    if !diagnostics.warnings.is_empty() {
        eprint!(
            "{}",
            render_diagnostics(
                filename,
                &code,
                &diagnostics.warnings,
                std::io::stderr().is_terminal()
            )
        );
    }
    if diagnostics.has_errors() {
        eprint!(
            "{}",
            render_diagnostics(
                filename,
                &code,
                &diagnostics.errors,
                std::io::stderr().is_terminal()
            )
        );
        process::exit(1);
    }

    let mut vm = VM::new();
    vm.state_mut().set_stress_gc(stress_gc);
    vm.state_mut().register_constant_pool_ids(constant_pool_ids);
    vm.eval(&module);
}
