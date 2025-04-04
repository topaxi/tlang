use tlang_ast_lowering::lower_to_hir;
use tlang_hir::hir;
use tlang_parser::Parser;
use tlang_hir_pretty::HirPrettyOptions;
use tlang_hir_opt::hir_opt::HirOptimizer;
use tlang_hir_opt::{constant_folding::ConstantFolder, constant_propagation::ConstantPropagator};

pub fn compile_to_hir(source: &str) -> hir::Module {
    let ast = Parser::from_source(source).parse().unwrap();
    lower_to_hir(&ast)
}

pub fn compile_and_optimize(source: &str) -> hir::Module {
    let mut module = compile_to_hir(source);
    let mut optimizer = HirOptimizer::new();
    optimizer.add_pass(Box::new(ConstantFolder::new()));
    optimizer.add_pass(Box::new(ConstantPropagator::new()));
    optimizer.optimize_module(&mut module);
    module
}

pub fn pretty_print(module: &hir::Module) -> String {
    let options = HirPrettyOptions {
        mark_unresolved: false,
        ..Default::default()
    };
    let mut prettier = tlang_hir_pretty::HirPretty::new(options);
    let mut output = String::from("fn main() -> unknown {\n");
    prettier.print_module(module);
    let inner = prettier.output().trim_end();
    if !inner.is_empty() {
        output.push_str("    ");
        output.push_str(&inner.replace("\n", "\n    "));
        output.push('\n');
    }
    output.push_str("};\n");
    output
}
