use tlang_ast::symbols::SymbolType;
use tlang_ast_lowering::{LowerResult, lower_to_hir};
use tlang_hir::hir;
use tlang_hir_opt::hir_opt::HirOptGroup;
use tlang_hir_opt::{
    HirPass, constant_folding::ConstantFolder, constant_propagation::ConstantPropagator,
    hir_opt::HirOptimizer,
};
use tlang_hir_pretty::HirPrettyOptions;
use tlang_parser::Parser;
use tlang_semantics::SemanticAnalyzer;

pub fn compile(source: &str) -> LowerResult {
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

pub fn optimize(module: &mut hir::Module, passes: Vec<Box<dyn HirPass>>) {
    let mut optimizer = HirOptimizer::new(HirOptGroup::new(passes));
    optimizer.optimize_module(module);
}

pub fn compile_and_optimize(source: &str) -> hir::Module {
    compile_with_passes(
        source,
        vec![
            Box::new(ConstantFolder::default()),
            Box::new(ConstantPropagator::default()),
        ],
    )
}

pub fn compile_with_passes(source: &str, passes: Vec<Box<dyn HirPass>>) -> hir::Module {
    let mut hir = compile(source);
    optimize(&mut hir.module, passes);
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
