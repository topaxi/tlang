use tlang_ast::{
    node::{AstNode, Node},
    symbols::SymbolType,
};
use tlang_codegen_js::generator::CodegenJS;
use tlang_parser::{error::ParseError, parser::Parser};
use tlang_semantics::{diagnostic::Diagnostic, SemanticAnalyzer};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct TlangCompiler {
    source: String,
    codegen: CodegenJS,
    analyzer: SemanticAnalyzer,
    ast: Node,

    diagnostics: Vec<Diagnostic>,
    parse_errors: Vec<ParseError>,
}

#[wasm_bindgen]
impl TlangCompiler {
    #[wasm_bindgen(constructor)]
    pub fn new(source: &str) -> Self {
        TlangCompiler {
            source: source.to_string(),
            codegen: CodegenJS::default(),
            analyzer: SemanticAnalyzer::default(),
            ast: Node::new(AstNode::None),
            diagnostics: Vec::new(),
            parse_errors: Vec::new(),
        }
    }

    fn parse(&mut self) {
        match Parser::from_source(&self.source).parse() {
            Ok(ast) => self.ast = ast,
            Err(errors) => self.parse_errors = errors,
        }
    }

    fn analyze(&mut self) {
        self.analyzer.add_builtin_symbols(&[
            ("Option", SymbolType::Enum),
            ("Result", SymbolType::Enum),
            ("Option::Some", SymbolType::EnumVariant),
            ("Option::None", SymbolType::EnumVariant),
            ("Result::Ok", SymbolType::EnumVariant),
            ("Result::Err", SymbolType::EnumVariant),
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
        let _ = self.analyzer.analyze(&mut self.ast);
        self.diagnostics = self.analyzer.get_diagnostics().to_owned();
    }

    #[wasm_bindgen]
    pub fn compile(&mut self) {
        self.parse();
        self.analyze();
        self.codegen.generate_code(&self.ast);
    }

    #[wasm_bindgen(getter)]
    pub fn source(&self) -> String {
        self.source.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn ast(&self) -> String {
        format!("{:#?}", self.ast)
    }

    #[wasm_bindgen(getter)]
    pub fn diagnostics(&self) -> Vec<String> {
        self.diagnostics.iter().map(|d| d.to_string()).collect()
    }

    #[wasm_bindgen(getter, js_name = "parseErrors")]
    pub fn parse_errors(&self) -> Vec<String> {
        self.parse_errors.iter().map(|e| e.to_string()).collect()
    }

    #[wasm_bindgen(getter, js_name = "standardLibrarySource")]
    pub fn standard_library_source() -> String {
        CodegenJS::get_standard_library_source()
    }

    #[wasm_bindgen(getter)]
    pub fn output(&self) -> String {
        self.codegen.get_output().to_string()
    }
}
