use std::{fs::File, io::Read, path::Path};

use tlang_ast_lowering::lower_to_hir;
use tlang_core::{memory::TlangValue, vm::VM};
use tlang_diagnostics::{render_parse_issues, render_semantic_diagnostics};
use tlang_hir as hir;
use tlang_hir_opt::HirOptimizer;
use tlang_semantics::SemanticAnalyzer;

pub fn handle_run(input_file: &str) {
    let (module, constant_pool_ids) = compile(input_file);

    let mut vm = VM::new();
    vm.state_mut().register_constant_pool_ids(constant_pool_ids);
    let result = vm.eval(&module);

    match result {
        TlangValue::Nil => {}
        _ => println!("{}", result),
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
    let prev_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(|_| {}));
    let parse_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parser.parse()));
    std::panic::set_hook(prev_hook);
    let (mut ast, parse_meta) = match parse_result {
        Ok(Ok(result)) => result,
        Ok(Err(err)) => {
            eprint!(
                "{}",
                render_parse_issues(&path.to_string_lossy(), &source, err.issues())
            );
            std::process::exit(1);
        }
        Err(payload) => {
            let issues = parser.errors();
            if issues.is_empty() {
                std::panic::resume_unwind(payload);
            }
            eprint!(
                "{}",
                render_parse_issues(&path.to_string_lossy(), &source, issues)
            );
            std::process::exit(1);
        }
    };

    let mut semantic_analyzer = SemanticAnalyzer::default();
    semantic_analyzer.add_builtin_symbols_with_slots(&VM::builtin_symbols());
    if let Err(err) = semantic_analyzer.analyze(&mut ast) {
        eprint!(
            "{}",
            render_semantic_diagnostics(&path.to_string_lossy(), &source, &err)
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
            render_semantic_diagnostics(&path.to_string_lossy(), &source, &warnings)
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
