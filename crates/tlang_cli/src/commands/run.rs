use std::{fs::File, io::Read, path::Path};

use tlang_ast_lowering::lower_to_hir;
use tlang_hir_opt::HirOptimizer;
use tlang_runtime::{interpreter::Interpreter, memory::TlangValue};
use tlang_semantics::SemanticAnalyzer;

pub fn handle_run(input_file: &str) {
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
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(err) => {
            eprintln!("{err:?}");
            std::process::exit(1);
        }
    };

    let mut semantic_analyzer = SemanticAnalyzer::default();
    semantic_analyzer.add_builtin_symbols_with_slots(&Interpreter::builtin_symbols());
    if let Err(err) = semantic_analyzer.analyze(&ast) {
        eprintln!("{err:?}");
        std::process::exit(1);
    }

    let (mut module, meta) = lower_to_hir(
        &ast,
        semantic_analyzer.symbol_id_allocator(),
        semantic_analyzer.root_symbol_table(),
        semantic_analyzer.symbol_tables().clone(),
    );

    let mut optimizer = HirOptimizer::default();
    optimizer.optimize_hir(&mut module, meta.into());

    let mut interpreter = Interpreter::default();
    let result = interpreter.eval(&module);

    match result {
        TlangValue::Nil => {}
        _ => println!("{}", result),
    }
}
