use tlang_ast::symbols::SymbolType;
use tlang_codegen_js::generator::CodegenJS;
use tlang_parser::parser::Parser;
use tlang_semantics::SemanticAnalyzer;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn parse_source(source: &str) -> String {
    let mut parser = Parser::from_source(source);
    let mut semantic_analyzer = SemanticAnalyzer::default();
    let mut codegen = CodegenJS::default();

    let mut ast = parser.parse();

    semantic_analyzer.add_builtin_symbols(&[
        ("log", SymbolType::Function),
        ("max", SymbolType::Function),
        ("min", SymbolType::Function),
    ]);
    semantic_analyzer.analyze(&mut ast);

    codegen.generate_code(&ast);
    codegen.get_output().to_string()
}
