#[test]
fn test_enum_method_with_pattern_match() {
    use indoc::indoc;
    use pretty_assertions::assert_eq;
    use tlang_codegen_js::generator::CodegenJS;
    use tlang_codegen_js::js_hir_opt::JsHirOptimizer;
    use tlang_parser::Parser;
    use tlang_semantics::SemanticAnalyzer;

    let output = {
        let source = indoc! {"
            enum Tree {
                Empty,
                Node { value: int, left: Tree, right: Tree },
            }

            fn Tree.insert(Tree::Empty, value: int) -> Tree {
                Tree::Node { value: value, left: Tree::Empty, right: Tree::Empty }
            }

            fn Tree.insert(Tree::Node { value: node_val, left, right }, value: int) -> Tree {
                if (value < node_val) {
                    Tree::Node { value: node_val, left: left.insert(value), right: right }
                } else {
                    Tree::Node { value: node_val, left: left, right: right.insert(value) }
                }
            }
        "};

        let mut parser = Parser::from_source(source);
        let mut ast = parser.parse().unwrap();

        let mut semantic_analyzer = SemanticAnalyzer::default();
        semantic_analyzer.add_builtin_symbols(CodegenJS::get_standard_library_symbols());
        semantic_analyzer.analyze(&mut ast).unwrap();

        let (mut module, meta) = tlang_ast_lowering::lower_to_hir(
            &ast,
            semantic_analyzer.symbol_id_allocator(),
            semantic_analyzer.root_symbol_table(),
            semantic_analyzer.symbol_tables().clone(),
        );

        let mut optimizer = JsHirOptimizer::default();
        let mut ctx = meta.into();
        optimizer.optimize_hir(&mut module, &mut ctx);

        let mut codegen = CodegenJS::default();
        codegen.generate_code(&module);
        codegen.get_output().to_string()
    };

    println!("Generated code:\n{}", output);
}
