use tlang_ast::node::{self as ast};
use tlang_codegen_js::generator::CodegenJS;
use tlang_hir::hir;
use tlang_hir_pretty::HirPretty;
use tlang_parser::Parser;
use tlang_parser::error::{ParseError, ParseIssue};
use tlang_semantics::SemanticAnalyzer;
use wasm_bindgen::prelude::*;

use crate::codemirror;

#[wasm_bindgen(js_name = "getStandardLibrarySource")]
pub fn get_standard_library_source() -> String {
    CodegenJS::get_standard_library_source()
}

#[derive(Default)]
pub struct BuildArtifacts {
    parse_result: Option<Result<ast::Module, ParseError>>,
    hir: Option<hir::Module>,
}

#[wasm_bindgen]
pub struct Tlang {
    source: String,
    build: BuildArtifacts,
    analyzer: SemanticAnalyzer,
    js: CodegenJS,
    interpreter: crate::interpreter::TlangInterpreter,
}

#[wasm_bindgen]
impl Tlang {
    #[wasm_bindgen(constructor)]
    pub fn new(source: String) -> Self {
        let mut analyzer = SemanticAnalyzer::default();

        analyzer.add_builtin_symbols(CodegenJS::get_standard_library_symbols());

        Self {
            source: source.to_string(),
            build: BuildArtifacts::default(),
            analyzer,
            js: CodegenJS::default(),
            interpreter: crate::interpreter::TlangInterpreter::new(),
        }
    }

    fn ast(&self) -> Option<&ast::Module> {
        self.build
            .parse_result
            .as_ref()
            .and_then(|r| r.as_ref().ok())
    }

    fn hir(&self) -> Option<&hir::Module> {
        self.build.hir.as_ref()
    }

    #[wasm_bindgen]
    pub fn eval(&mut self) -> Result<JsValue, JsError> {
        self.lower_to_hir();

        let hir = self
            .build
            .hir
            .as_ref()
            .ok_or_else(|| JsError::new("Failed to generate HIR"))?;

        Ok(self.interpreter.eval(hir))
    }

    #[wasm_bindgen]
    pub fn define_js_fn(&mut self, name: &str, f: js_sys::Function) {
        self.interpreter.define_js_fn(name, f);
        self.analyzer
            .add_builtin_symbols(&[(name, tlang_ast::symbols::SymbolType::Function)]);
    }

    fn parse(&mut self) -> Result<&ast::Module, &ParseError> {
        if self.build.parse_result.is_some() {
            return self.build.parse_result.as_ref().unwrap().as_ref();
        }

        let mut parser = Parser::from_source(&self.source);
        parser.set_recoverable(true);

        self.build.parse_result = Some(parser.parse());
        self.build.parse_result.as_ref().unwrap().as_ref()
    }

    fn parse_issues(&mut self) -> &[ParseIssue] {
        match self.parse() {
            Err(parse_error) => parse_error.issues(),
            Ok(_) => &[],
        }
    }

    #[wasm_bindgen(getter, js_name = "parseErrors")]
    pub fn parse_errors(&mut self) -> Result<JsValue, serde_wasm_bindgen::Error> {
        serde_wasm_bindgen::to_value(self.parse_issues())
    }

    #[wasm_bindgen]
    pub fn analyze(&mut self) {
        let _ = self.parse();

        if let Some(Ok(ast)) = &self.build.parse_result {
            let _ = self.analyzer.analyze(ast);
        }
    }

    fn lower_to_hir(&mut self) {
        let _ = self.parse();

        if self.hir().is_some() {
            return;
        }

        if let Some(ast) = self.ast() {
            self.build.hir = Some(tlang_ast_lowering::lower_to_hir(ast));
        }
    }

    #[wasm_bindgen(js_name = "compileToJS")]
    pub fn compile_to_js(&mut self) -> Result<(), JsError> {
        if self.js.get_output().is_empty() {
            let _ = self.parse();
            self.lower_to_hir();
            if let Some(hir) = self.build.hir.as_ref() {
                self.js.generate_code(hir);
            }
        }

        Ok(())
    }

    #[wasm_bindgen(getter)]
    pub fn hir_string(&mut self) -> String {
        self.lower_to_hir();

        if let Some(hir) = self.build.hir.as_ref() {
            ron::ser::to_string_pretty(hir, ron::ser::PrettyConfig::default()).unwrap_or_default()
        } else {
            String::new()
        }
    }

    #[wasm_bindgen(getter)]
    pub fn hir_pretty(&mut self) -> String {
        self.lower_to_hir();

        if let Some(hir) = self.build.hir.as_ref() {
            HirPretty::pretty_print(hir)
        } else {
            String::new()
        }
    }

    #[wasm_bindgen(getter)]
    pub fn ast_string(&mut self) -> String {
        let _ = self.parse();

        if let Some(ast) = self.ast() {
            ron::ser::to_string_pretty(ast, ron::ser::PrettyConfig::default()).unwrap_or_default()
        } else {
            String::new()
        }
    }

    #[wasm_bindgen(getter)]
    pub fn js(&mut self) -> String {
        let _ = self.compile_to_js();
        self.js.get_output().to_string()
    }

    #[wasm_bindgen(getter)]
    pub fn diagnostics(&mut self) -> Result<JsValue, serde_wasm_bindgen::Error> {
        serde_wasm_bindgen::to_value(self.analyzer.get_diagnostics())
    }

    #[wasm_bindgen(getter, js_name = "codemirrorDiagnostics")]
    pub fn codemirror_diagnostics(&mut self) -> Vec<codemirror::CodemirrorDiagnostic> {
        let source = self.source.clone();
        let diagnostics = self
            .analyzer
            .get_diagnostics()
            .iter()
            .map(|d| codemirror::from_tlang_diagnostic(&source, d))
            .collect::<Vec<_>>();
        let parse_errors = self
            .parse_issues()
            .iter()
            .map(|e| codemirror::from_parse_issue(&source, e));

        parse_errors.chain(diagnostics).collect()
    }
}
