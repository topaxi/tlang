fn main() {
    println!("Debug tool for analyzing HIR transformation issues");
}
    let source = r#"
        fn test_sum() {
            let sum = 0;
            for i in [1, 2, 3, 4, 5] {
                sum = sum + i;
            }
            sum
        }
    "#;

    let mut parser = Parser::from_source(source);
    let ast = parser.parse().unwrap();

    let mut semantic_analyzer = SemanticAnalyzer::default();
    semantic_analyzer.add_builtin_symbols(&CodegenJS::get_standard_library_symbols());
    semantic_analyzer.analyze(&ast).unwrap();

    let (mut module, meta) = tlang_ast_lowering::lower_to_hir(
        &ast,
        semantic_analyzer.symbol_id_allocator(),
        semantic_analyzer.root_symbol_table(),
        semantic_analyzer.symbol_tables().clone(),
    );

    println!("=== BEFORE OPTIMIZATION ===");
    println!("{:#?}", module);

    let mut optimizer = HirOptimizer::default();
    optimizer.add_pass(Box::new(create_hir_js_opt_group()));
    optimizer.optimize_hir(&mut module, meta.into());

    println!("\n=== AFTER OPTIMIZATION ===");
    println!("{:#?}", module);

    println!("\n=== ATTEMPTING CODEGEN ===");
    let mut codegen = CodegenJS::default();
    match std::panic::catch_unwind(|| {
        codegen.generate_code(&module);
        codegen.get_output().to_string()
    }) {
        Ok(output) => println!("SUCCESS:\n{}", output),
        Err(e) => println!("PANIC: {:?}", e),
    }
}