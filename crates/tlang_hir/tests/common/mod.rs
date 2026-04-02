#![allow(dead_code)]

use tlang_ast as ast;

fn parse_str(input: &str) -> (ast::node::Module, tlang_parser::ParseMeta) {
    let mut parser = tlang_parser::Parser::from_source(input);
    parser.parse().unwrap()
}

pub fn hir_from_str(input: &str) -> tlang_hir::Module {
    let (ast, parse_meta) = parse_str(input);
    let (module, _) = tlang_ast_lowering::lower_to_hir(
        &ast,
        &parse_meta.constant_pool_node_ids,
        Default::default(),
        Default::default(),
        Default::default(),
    )
    .expect("lowering should succeed");
    module
}

pub fn hir_from_str_analyzed(input: &str) -> tlang_hir::Module {
    let (mut ast, parse_meta) = parse_str(input);
    let mut analyzer = tlang_semantics::SemanticAnalyzer::default();
    let _ = analyzer.analyze(&mut ast);
    let (module, _) = tlang_ast_lowering::lower_to_hir(
        &ast,
        &parse_meta.constant_pool_node_ids,
        analyzer.symbol_id_allocator(),
        analyzer.root_symbol_table(),
        analyzer.symbol_tables().clone(),
    )
    .expect("lowering should succeed");
    module
}
