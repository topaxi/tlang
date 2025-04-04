use tlang_ast_lowering::lower_to_hir;
use tlang_hir::hir;
use tlang_parser::Parser;

pub fn compile_to_hir(source: &str) -> hir::Module {
    let ast = Parser::from_source(source).parse().unwrap();
    lower_to_hir(&ast)
}

pub fn compile_and_optimize(source: &str) -> hir::Module {
    let mut module = compile_to_hir(source);
    let mut folder = tlang_hir_opt::constant_folding::ConstantFolder::new();
    folder.optimize_module(&mut module);
    module
}
