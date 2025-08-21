use std::env;
use std::fs;
use std::process;

use tlang_ast_lowering::lower_to_hir;
use tlang_hir_opt::HirOptimizer;
use tlang_interpreter::Interpreter;
pub use tlang_memory::NativeFnDef;
use tlang_semantics::SemanticAnalyzer;
use tlang_semantics::diagnostic::Diagnostic;

fn main() {
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
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(err) => {
            eprintln!("Error parsing file '{}': {}", filename, err);
            process::exit(1);
        }
    };
    let mut analyzer = SemanticAnalyzer::default();
    analyzer.add_builtin_symbols(&Interpreter::builtin_symbols());

    match analyzer.analyze(&ast) {
        Ok(_) => {}
        Err(diagnostics) => {
            for diagnostic in &diagnostics {
                eprintln!("{}", diagnostic);
            }

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
    optimizer.optimize_hir(&mut module, meta.into());

    let mut interp = Interpreter::default();
    interp.eval(&module);
}
