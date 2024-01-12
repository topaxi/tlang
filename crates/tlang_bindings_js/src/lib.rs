use tlang_ast::{node::Node, symbols::SymbolType};
use tlang_codegen_js::generator::CodegenJS;
use tlang_parser::parser::Parser;
use tlang_semantics::SemanticAnalyzer;
use wasm_bindgen::prelude::*;

fn parse_source(source: &str) -> Result<Node, JsError> {
    match Parser::from_source(source).parse() {
        Ok(ast) => Ok(ast),
        Err(errors) => Err(JsError::new(&format!("{:#?}", errors))),
    }
}

#[wasm_bindgen]
pub fn parse_to_ast(source: &str) -> Result<String, JsError> {
    let ast = parse_source(source)?;
    Ok(format!("{:#?}", ast))
}

#[wasm_bindgen]
pub fn parse_and_analyze(source: &str) -> Result<String, JsError> {
    let mut ast = parse_source(source)?;
    let mut semantic_analyzer = SemanticAnalyzer::default();
    semantic_analyzer.add_builtin_symbols(&[
        ("log", SymbolType::Function),
        ("max", SymbolType::Function),
        ("min", SymbolType::Function),
        ("floor", SymbolType::Function),
        ("random", SymbolType::Function),
    ]);
    semantic_analyzer.analyze(&mut ast);
    Ok(format!("{:#?}", ast))
}

#[wasm_bindgen]
pub fn compile_to_js(source: &str) -> Result<String, JsError> {
    let mut semantic_analyzer = SemanticAnalyzer::default();
    let mut codegen = CodegenJS::default();

    let mut ast = parse_source(source)?;

    semantic_analyzer.add_builtin_symbols(&[
        ("log", SymbolType::Function),
        ("max", SymbolType::Function),
        ("min", SymbolType::Function),
        ("floor", SymbolType::Function),
        ("random", SymbolType::Function),
    ]);
    semantic_analyzer.analyze(&mut ast);

    codegen.generate_code(&ast);
    Ok(codegen.get_output().to_string())
}
