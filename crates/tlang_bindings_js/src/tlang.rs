use serde::Deserialize;
use tlang_ast::node::{self as ast};
use tlang_codegen_js::generator::CodegenJS;
use tlang_hir::hir;
use tlang_hir_opt::HirOptimizer;
use tlang_hir_pretty::{HirPretty, HirPrettyOptions};
use tlang_parser::Parser;
use tlang_parser::error::{ParseError, ParseIssue};
use tlang_semantics::SemanticAnalyzer;
use wasm_bindgen::prelude::*;

use crate::codemirror;

#[wasm_bindgen(js_name = "getStandardLibrarySource")]
pub fn get_standard_library_source() -> String {
    CodegenJS::get_standard_library_source()
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct JsHirPrettyOptions {
    pub tab_indent: Option<bool>,
    pub indent_size: Option<usize>,
    pub mark_unresolved: Option<bool>,
    pub comments: Option<bool>,
}

impl JsHirPrettyOptions {
    fn into_hir_pretty_options(self) -> HirPrettyOptions {
        let mut options = HirPrettyOptions::default();

        if let Some(tab_indent) = self.tab_indent {
            options.tab_indent = tab_indent;
        }

        if let Some(indent_size) = self.indent_size {
            options.indent_size = indent_size;
        }

        if let Some(mark_unresolved) = self.mark_unresolved {
            options.mark_unresolved = mark_unresolved;
        }

        if let Some(comments) = self.comments {
            options.comments = comments;
        }

        options
    }
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
            source,
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

    #[wasm_bindgen(js_name = "defineFunction")]
    pub fn define_js_fn(&mut self, name: &str, f: js_sys::Function) {
        let arity = f.length() as u16;

        self.interpreter.define_js_fn(name, f);
        self.analyzer
            .add_builtin_symbols(&[(name, tlang_ast::symbols::SymbolType::Function(arity))]);
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

    #[wasm_bindgen(js_name = "getParseErrors")]
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
            let mut hir = tlang_ast_lowering::lower_to_hir(
                ast,
                self.analyzer.symbol_id_allocator(),
                self.analyzer.symbol_tables().clone(),
            );
            let mut optimizer = HirOptimizer::default();
            optimizer.optimize_module(&mut hir);
            self.build.hir = Some(hir);
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

    #[wasm_bindgen(js_name = "getHIRString")]
    pub fn hir_string(&mut self) -> String {
        self.lower_to_hir();

        if let Some(hir) = self.build.hir.as_ref() {
            ron::ser::to_string_pretty(hir, ron::ser::PrettyConfig::default()).unwrap_or_default()
        } else {
            String::new()
        }
    }

    #[wasm_bindgen(js_name = "getHIRPretty")]
    pub fn hir_pretty(&mut self, options: JsValue) -> Result<String, serde_wasm_bindgen::Error> {
        self.lower_to_hir();

        if let Some(hir) = self.build.hir.as_ref() {
            if options.is_undefined() {
                return Ok(HirPretty::pretty_print(hir));
            }

            let options: JsHirPrettyOptions = serde_wasm_bindgen::from_value(options)?;
            let options = options.into_hir_pretty_options();
            let mut pretty_pinter = HirPretty::new(options);

            pretty_pinter.print_module(hir);

            Ok(pretty_pinter.output().to_string())
        } else {
            Ok(String::new())
        }
    }

    #[wasm_bindgen(js_name = "getASTString")]
    pub fn ast_string(&mut self) -> String {
        let _ = self.parse();

        if let Some(ast) = self.ast() {
            ron::ser::to_string_pretty(ast, ron::ser::PrettyConfig::default()).unwrap_or_default()
        } else {
            String::new()
        }
    }

    #[wasm_bindgen(js_name = "getJavaScript")]
    pub fn js(&mut self) -> String {
        let _ = self.compile_to_js();
        self.js.get_output().to_string()
    }

    #[wasm_bindgen(js_name = "getDiagnostics")]
    pub fn diagnostics(&mut self) -> Result<JsValue, serde_wasm_bindgen::Error> {
        serde_wasm_bindgen::to_value(self.analyzer.get_diagnostics())
    }

    #[wasm_bindgen(js_name = "getCodemirrorDiagnostics")]
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
