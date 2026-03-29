use std::io::IsTerminal;
use std::{env, fs, process};

use tlang_ast_lowering::lower_to_hir;
use tlang_diagnostics::{render_ice, render_parse_issues, render_semantic_diagnostics};
use tlang_hir_opt::HirOptimizer;
use tlang_octarine::stream::native::{
    NativeStreamRegistry, clear_stream_context, set_stream_context,
};
use tlang_semantics::SemanticAnalyzer;
use tlang_semantics::diagnostic::Diagnostic;
use tlang_vm::VM;

/// Additional builtin symbols provided by the octarine runtime.
///
/// These are registered alongside the standard `VM::builtin_symbols()` so
/// that the semantic analyzer recognizes `Streams::*` names.
fn octarine_extra_symbols() -> Vec<(String, tlang_defs::DefKind, Option<usize>)> {
    use tlang_defs::DefKind;
    vec![
        ("Streams".into(), DefKind::Module, None),
        ("Streams::ReadStream".into(), DefKind::Struct, None),
        ("Streams::WriteStream".into(), DefKind::Struct, None),
        ("Streams::DuplexStream".into(), DefKind::Struct, None),
    ]
}

fn main() {
    env_logger::builder()
        .filter_level(log::LevelFilter::Warn)
        .parse_default_env()
        .init();

    // Anchor octarine native definitions so inventory discovers them.
    tlang_octarine::init();

    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file.tlang>", args[0]);
        process::exit(1);
    }

    let filename = &args[1];
    if !filename.ends_with(".tlang") {
        eprintln!("Error: file must have a '.tlang' extension");
        process::exit(1);
    }

    let code = match fs::read_to_string(filename) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error reading '{}': {}", filename, e);
            process::exit(1);
        }
    };

    // ── Parse ──────────────────────────────────────────────────────────
    let mut parser = tlang_parser::Parser::from_source(&code);
    let (mut ast, parse_meta) = match parser.parse() {
        Ok(r) => r,
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

    // ── Semantic analysis ──────────────────────────────────────────────
    let mut analyzer = SemanticAnalyzer::default();
    analyzer.add_builtin_symbols_with_slots(&VM::builtin_symbols());
    analyzer.add_builtin_symbols_with_slots(&octarine_extra_symbols());

    match analyzer.analyze(&mut ast) {
        Ok(()) => {}
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
            if diagnostics.iter().any(Diagnostic::is_error) {
                process::exit(1);
            }
        }
    }

    // ── Lower to HIR ───────────────────────────────────────────────────
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

    // ── Optimize ───────────────────────────────────────────────────────
    let mut optimizer = HirOptimizer::default();
    let constant_pool_ids = meta.constant_pool_ids.clone();
    let mut ctx = meta.into();
    if let Err(err) = optimizer.optimize_hir(&mut module, &mut ctx) {
        eprint!("{}", render_ice(&err));
        process::exit(1);
    }

    // ── Evaluate ───────────────────────────────────────────────────────
    let mut vm = VM::new();
    vm.state_mut().register_constant_pool_ids(constant_pool_ids);

    // Set up the stream context so native stream functions can access I/O.
    let mut streams = NativeStreamRegistry::new();
    set_stream_context(&mut streams);

    vm.eval(&module);

    clear_stream_context();
}
