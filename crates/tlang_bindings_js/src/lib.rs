extern crate console_error_panic_hook;

use gloo_utils::format::JsValueSerdeExt;
use tlang_ast::node::Module;
use tlang_codegen_js::generator::CodegenJS;
use tlang_parser::{error::ParseError, parser::Parser};
use tlang_semantics::{diagnostic::Diagnostic, SemanticAnalyzer};
use wasm_bindgen::prelude::*;

pub mod codemirror;

#[wasm_bindgen(js_name = "getStandardLibrarySource")]
pub fn get_standard_library_source() -> String {
    CodegenJS::get_standard_library_source()
}

#[wasm_bindgen]
pub struct TlangCompiler {
    source: String,
    codegen: CodegenJS,
    analyzer: SemanticAnalyzer,
    ast: Module,

    diagnostics: Vec<Diagnostic>,
    parse_errors: Vec<ParseError>,
}

#[wasm_bindgen]
impl TlangCompiler {
    #[wasm_bindgen(constructor)]
    pub fn new(source: &str) -> Self {
        console_error_panic_hook::set_once();

        TlangCompiler {
            source: source.to_string(),
            codegen: CodegenJS::default(),
            analyzer: SemanticAnalyzer::default(),
            ast: Module::new(vec![]),
            diagnostics: Vec::new(),
            parse_errors: Vec::new(),
        }
    }

    fn parse(&mut self) {
        match Parser::from_source(&self.source).parse() {
            Ok(ast) => self.ast = ast,
            Err(errors) => self.parse_errors = errors.to_vec(),
        }
    }

    fn analyze(&mut self) {
        self.analyzer
            .add_builtin_symbols(CodegenJS::get_standard_library_symbols());
        let _ = self.analyzer.analyze(&mut self.ast);
        self.analyzer
            .get_diagnostics()
            .clone_into(&mut self.diagnostics);
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
    pub fn ast_string(&self) -> Result<String, JsError> {
        Ok(ron::ser::to_string_pretty(
            &self.ast,
            ron::ser::PrettyConfig::default(),
        )?)
    }

    #[wasm_bindgen(getter)]
    pub fn ast_json_string(&self) -> Result<String, JsError> {
        Ok(serde_json::to_string_pretty(&self.ast)?)
    }

    #[wasm_bindgen(getter)]
    pub fn ast(&self) -> Result<JsValue, JsError> {
        Ok(JsValue::from_serde(&self.ast)?)
    }

    #[wasm_bindgen(getter, js_name = "diagnostics")]
    pub fn diagnostic_messages(&self) -> Vec<String> {
        self.diagnostics.iter().map(|d| d.to_string()).collect()
    }

    #[wasm_bindgen(getter, js_name = "parseErrors")]
    pub fn parse_error_messages(&self) -> Vec<String> {
        self.parse_errors.iter().map(|e| e.to_string()).collect()
    }

    #[wasm_bindgen(getter, js_name = "codemirrorDiagnostics")]
    pub fn codemirror_diagnostics(&self) -> Vec<codemirror::CodemirrorDiagnostic> {
        let parse_errors = self
            .parse_errors
            .iter()
            .map(|e| codemirror::from_parse_error(&self.source, e));
        let diagnostics = self
            .diagnostics
            .iter()
            .map(|d| codemirror::from_tlang_diagnostic(&self.source, d));

        parse_errors.chain(diagnostics).collect()
    }

    #[wasm_bindgen(getter)]
    pub fn output(&self) -> String {
        self.codegen.get_output().to_string()
    }
}
