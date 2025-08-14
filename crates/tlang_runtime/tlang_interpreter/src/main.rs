use std::collections::HashSet;
use std::env;
use std::fs;
use std::process;

use tlang_ast::symbols::SymbolType;
use tlang_ast_lowering::lower_to_hir;
use tlang_hir_opt::HirOptimizer;
use tlang_interpreter::Interpreter;
pub use tlang_memory::NativeFnDef;
use tlang_semantics::SemanticAnalyzer;
use tlang_semantics::diagnostic::Diagnostic;

fn main() {
    env_logger::init();

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

    let mut module_names = HashSet::new();
    let builtin_module_symbols = inventory::iter::<NativeFnDef>
        .into_iter()
        .map(|def| def.module())
        .filter(|module_name| module_names.insert(module_name.to_string()))
        .map(|module_name| (module_name, SymbolType::Module))
        .collect::<Vec<_>>();

    let builtin_fn_symbols = inventory::iter::<NativeFnDef>
        .into_iter()
        .map(|def| (def.name(), SymbolType::Function(def.arity() as u16)))
        .collect::<Vec<_>>();

    analyzer.add_builtin_symbols(&builtin_module_symbols);
    analyzer.add_builtin_symbols(&builtin_fn_symbols);
    analyzer.add_builtin_symbols(&[("math::pi", SymbolType::Variable)]);

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
        analyzer.symbol_tables().clone(),
    );

    let mut optimizer = HirOptimizer::default();
    let mut optimizer_context = meta.into();
    optimizer.optimize_hir(&mut module, &mut optimizer_context);

    let mut interp = Interpreter::default();
    interp.eval(&module);
}
