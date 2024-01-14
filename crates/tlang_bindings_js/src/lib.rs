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

fn analyze_ast(ast: &mut Node) -> Result<(), JsError> {
    let mut semantic_analyzer = SemanticAnalyzer::default();
    semantic_analyzer.add_builtin_symbols(&[
        ("Some", SymbolType::Function),
        ("None", SymbolType::Variable),
        ("Ok", SymbolType::Function),
        ("Err", SymbolType::Function),
        ("len", SymbolType::Function),
        ("log", SymbolType::Function),
        ("max", SymbolType::Function),
        ("min", SymbolType::Function),
        ("floor", SymbolType::Function),
        ("random", SymbolType::Function),
        ("random_int", SymbolType::Function),
        ("compose", SymbolType::Function),
        ("map", SymbolType::Function),
        ("filter", SymbolType::Function),
        ("filter_map", SymbolType::Function),
        ("partition", SymbolType::Function),
        ("foldl", SymbolType::Function),
        ("foldr", SymbolType::Function),
        ("sum", SymbolType::Function),
        ("zip", SymbolType::Function),
    ]);
    match semantic_analyzer.analyze(ast) {
        Ok(_) => Ok(()),
        Err(diagnostics) => Err(JsError::new(&format!("{:#?}", diagnostics))),
    }
}

#[wasm_bindgen]
pub fn get_standard_library_source() -> String {
    CodegenJS::get_standard_library_source()
}

#[wasm_bindgen]
pub fn parse_to_ast(source: &str) -> Result<String, JsError> {
    let ast = parse_source(source)?;
    Ok(format!("{:#?}", ast))
}

#[wasm_bindgen]
pub fn parse_and_analyze(source: &str) -> Result<String, JsError> {
    let mut ast = parse_source(source)?;
    analyze_ast(&mut ast)?;
    Ok(format!("{:#?}", ast))
}

#[wasm_bindgen]
pub fn compile_to_js(source: &str) -> Result<String, JsError> {
    let mut codegen = CodegenJS::default();
    let mut ast = parse_source(source)?;
    analyze_ast(&mut ast)?;
    codegen.generate_code(&ast);
    Ok(codegen.get_output().to_string())
}
