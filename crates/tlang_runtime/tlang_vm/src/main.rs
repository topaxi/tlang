use std::env;
use std::fs;
use std::process;

use tlang_ast_lowering::lower_to_hir;
use tlang_diagnostics::{render_parse_issues, render_semantic_diagnostics};
use tlang_hir_opt::HirOptimizer;
use tlang_semantics::SemanticAnalyzer;
use tlang_semantics::diagnostic::Diagnostic;
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
    let parse_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parser.parse()));
    let mut ast = match parse_result {
        Ok(Ok(ast)) => ast,
        Ok(Err(err)) => {
            eprint!("{}", render_parse_issues(filename, &code, err.issues()));
            process::exit(1);
        }
        Err(_) => {
            eprintln!("parse error");
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
                render_semantic_diagnostics(filename, &code, &diagnostics)
            );

            if diagnostics.iter().any(Diagnostic::is_error) {
                process::exit(1);
            }
        }
    }
    let (mut module, meta) = lower_to_hir(
        &ast,
        analyzer.symbol_id_allocator(),
        analyzer.root_symbol_table(),
        analyzer.symbol_tables().clone(),
    );

    let mut optimizer = HirOptimizer::default();
    let constant_pool_ids = meta.constant_pool_ids.clone();
    let mut ctx = meta.into();
    optimizer.optimize_hir(&mut module, &mut ctx);

    let mut vm = VM::new();
    vm.state_mut().set_stress_gc(stress_gc);
    vm.state_mut().register_constant_pool_ids(constant_pool_ids);
    vm.eval(&module);
}
