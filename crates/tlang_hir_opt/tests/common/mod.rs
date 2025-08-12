use tlang_ast::symbols::SymbolType;
use tlang_ast_lowering::lower_to_hir;
use tlang_hir::hir;
use tlang_hir_opt::hir_opt::HirOptimizer;
use tlang_hir_pretty::HirPrettyOptions;
use tlang_parser::Parser;
use tlang_semantics::SemanticAnalyzer;

pub fn compile(source: &str) -> hir::LowerResult {
    let ast = Parser::from_source(source).parse().unwrap();
    let mut semantic_analyzer = SemanticAnalyzer::default();
    semantic_analyzer.add_builtin_symbols(&[("println", SymbolType::Function(u16::MAX))]);
    semantic_analyzer.analyze(&ast).unwrap();
    lower_to_hir(
        &ast,
        semantic_analyzer.symbol_id_allocator(),
        semantic_analyzer.symbol_tables().clone(),
    )
}

pub fn compile_and_optimize(source: &str) -> hir::Module {
    let mut hir = compile(source);
    let mut optimizer = HirOptimizer::default();
    optimizer.optimize_hir(&mut hir);
    hir.module
}

pub fn pretty_print(module: &hir::Module) -> String {
    let options = HirPrettyOptions {
        mark_unresolved: false,
        ..Default::default()
    };
    let mut prettier = tlang_hir_pretty::HirPretty::new(options);
    prettier.print_module(module);
    prettier.output().to_string()
}
