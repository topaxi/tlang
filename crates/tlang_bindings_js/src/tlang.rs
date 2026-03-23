use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use serde::{Deserialize, Serialize};
use tlang_ast::node::{self as ast};
use tlang_codegen_js::generator::CodegenJS;
use tlang_codegen_js::js_anf_return_opt::JsAnfReturnOpt;
use tlang_codegen_js::js_anf_transform::JsAnfTransform;
use tlang_codegen_js::js_hir_opt::JsHirOptimizer;
use tlang_defs::{DefKind, DefScope};
use tlang_diagnostics::{render_parse_issues, render_semantic_diagnostics};
use tlang_hir as hir;
use tlang_hir_opt::HirPass;
use tlang_hir_opt::hir_opt::HirOptimizer;
use tlang_hir_pretty::{HirPretty, HirPrettyOptions};
use tlang_parser::Parser;
use tlang_parser::error::{ParseError, ParseIssue};
use tlang_semantics::SemanticAnalyzer;
use tlang_span::NodeId;
use tsify::Tsify;
use wasm_bindgen::prelude::*;

use crate::codemirror;
use crate::ts_types::{JsDiagnostic, JsParseIssue};

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(typescript_type = "JsDiagnostic[]")]
    pub type JsDiagnosticArray;

    #[wasm_bindgen(typescript_type = "JsParseIssue[]")]
    pub type JsParseIssueArray;

    #[wasm_bindgen(typescript_type = "CodemirrorDiagnostic[]")]
    pub type JsCodemirrorDiagnosticArray;

    #[wasm_bindgen(typescript_type = "unknown")]
    pub type JsUnknown;
}

#[wasm_bindgen(js_name = "getStandardLibrarySource")]
pub fn get_standard_library_source() -> String {
    CodegenJS::get_standard_library_source()
}

#[wasm_bindgen(js_name = "getStandardLibraryNativeJs")]
pub fn get_standard_library_native_js() -> String {
    CodegenJS::get_standard_library_native_js()
}

#[derive(Deserialize, Serialize, Tsify)]
#[tsify(into_wasm_abi, from_wasm_abi)]
#[serde(rename_all = "camelCase")]
pub struct JsHirPrettyOptions {
    pub tab_indent: Option<bool>,
    pub indent_size: Option<usize>,
    pub mark_unresolved: Option<bool>,
    pub print_ids: Option<bool>,
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

        if let Some(print_ids) = self.print_ids {
            options.print_ids = print_ids;
        }

        if let Some(comments) = self.comments {
            options.comments = comments;
        }

        options
    }
}

#[derive(Deserialize, Serialize, Tsify)]
#[tsify(into_wasm_abi, from_wasm_abi)]
#[serde(rename_all = "camelCase")]
pub struct JsOptimizationOptions {
    pub constant_folding: Option<bool>,
    /// ANF transform mode: `"off"`, `"minimal"` (JS-only lifting), or `"full"`.
    pub anf_transform: Option<String>,
    /// Replace ANF temporaries in return position with direct `return` statements.
    /// Defaults to `true` when the JS ANF transform is active.
    pub anf_return_opt: Option<bool>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RunnerKind {
    JavaScript,
    Interpreter,
}

impl From<Runner> for RunnerKind {
    fn from(runner: Runner) -> Self {
        match runner {
            Runner::JavaScript => RunnerKind::JavaScript,
            Runner::Interpreter => RunnerKind::Interpreter,
            _ => RunnerKind::Interpreter,
        }
    }
}

#[derive(Default)]
pub struct BuildArtifacts {
    parse_result: Option<Result<ast::Module, ParseError>>,
    constant_pool_node_ids: Vec<tlang_span::NodeId>,
    hir: Option<hir::Module>,
    analyzed: bool,
}

#[wasm_bindgen]
pub enum Runner {
    JavaScript = "JavaScript",
    Interpreter = "Interpreter",
}

#[wasm_bindgen]
pub struct Tlang {
    source: String,
    runner: RunnerKind,
    optimization_options: JsOptimizationOptions,
    build: BuildArtifacts,
    analyzer: SemanticAnalyzer,
    js: CodegenJS,
    interpreter: crate::interpreter::TlangInterpreter,
}

#[wasm_bindgen]
impl Tlang {
    #[wasm_bindgen(constructor)]
    #[allow(clippy::needless_pass_by_value)]
    pub fn new(source: String, runner: Runner) -> Self {
        let mut analyzer = SemanticAnalyzer::default();
        let runner_kind = RunnerKind::from(runner);

        match runner_kind {
            RunnerKind::JavaScript => {
                analyzer.add_builtin_symbols(CodegenJS::get_standard_library_symbols());
            }
            RunnerKind::Interpreter => {
                analyzer.add_builtin_symbols_with_slots(&tlang_core::vm::VM::builtin_symbols());
            }
        }

        Self {
            source,
            runner: runner_kind,
            optimization_options: JsOptimizationOptions {
                constant_folding: Some(true),
                anf_transform: None,
                anf_return_opt: Some(true),
            },
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
    pub fn eval(&mut self) -> Result<JsUnknown, JsError> {
        self.lower_to_hir();

        let hir = self
            .build
            .hir
            .as_ref()
            .ok_or_else(|| JsError::new("Failed to generate HIR"))?;

        Ok(self.interpreter.eval(hir).into())
    }

    #[wasm_bindgen(js_name = "defineFunction")]
    pub fn define_js_fn(&mut self, name: &str, f: js_sys::Function) {
        let arity = f.length() as u16;

        self.interpreter.define_js_fn(name, f);
        // Skip registration if the symbol is already a builtin (e.g. `log` for the
        // interpreter runner, which is registered with a global slot via
        // `add_builtin_symbols_with_slots`). Re-adding it without a slot would cause
        // the identifier resolver to pick up the slot-less duplicate and the
        // runtime to panic with "Function not found".
        if !self.analyzer.has_builtin_symbol(name) {
            self.analyzer
                .add_builtin_symbols(&[(name, DefKind::Function(arity))]);
        }
    }

    fn parse(&mut self) -> Result<&ast::Module, &ParseError> {
        if self.build.parse_result.is_none() {
            let mut parser = Parser::from_source(&self.source);
            match parser.parse() {
                Ok((module, parse_meta)) => {
                    self.build.constant_pool_node_ids = parse_meta.constant_pool_node_ids;
                    self.build.parse_result = Some(Ok(module));
                }
                Err(e) => {
                    self.build.parse_result = Some(Err(e));
                }
            }
        }

        self.build.parse_result.as_ref().unwrap().as_ref()
    }

    fn parse_issues(&mut self) -> &[ParseIssue] {
        match self.parse() {
            Err(parse_error) => parse_error.issues(),
            Ok(_) => &[],
        }
    }

    #[wasm_bindgen(js_name = "getParseErrors")]
    pub fn parse_errors(&mut self) -> Result<JsParseIssueArray, serde_wasm_bindgen::Error> {
        let issues: Vec<JsParseIssue> = self
            .parse_issues()
            .iter()
            .map(|issue| JsParseIssue::from(issue.clone()))
            .collect();
        Ok(serde_wasm_bindgen::to_value(&issues)?.unchecked_into())
    }

    #[wasm_bindgen]
    pub fn analyze(&mut self) {
        let _ = self.parse();

        if let Some(Ok(ast)) = &mut self.build.parse_result {
            let _ = self.analyzer.analyze(ast);
            self.build.analyzed = true;
        }
    }

    fn lower_to_hir(&mut self) {
        let _ = self.parse();

        if self.hir().is_some() {
            return;
        }

        if !self.build.analyzed {
            self.analyze();
        }

        if let Some(ast) = self.ast() {
            // Deep-clone the symbol tables so the lowering pass can mutate them
            // (e.g. via `DefScope::shift()`) without corrupting the analyzer's
            // state across multiple lowering invocations (e.g. when toggling
            // optimisation options).
            let symbol_tables: HashMap<NodeId, Rc<RefCell<DefScope>>> = self
                .analyzer
                .symbol_tables()
                .iter()
                .map(|(&k, v)| (k, Rc::new(RefCell::new(v.borrow().clone()))))
                .collect();
            let root_symbol_table = symbol_tables
                .get(&NodeId::new(1))
                .cloned()
                .unwrap_or_default();

            let (mut module, meta) = tlang_ast_lowering::lower_to_hir(
                ast,
                &self.build.constant_pool_node_ids,
                self.analyzer.symbol_id_allocator(),
                root_symbol_table,
                symbol_tables,
            );
            let constant_pool_ids = meta.constant_pool_ids.clone();
            let mut ctx = meta.into();

            match self.runner {
                RunnerKind::JavaScript => {
                    let anf_mode = self
                        .optimization_options
                        .anf_transform
                        .as_deref()
                        .unwrap_or("minimal");

                    let mut passes: Vec<Box<dyn HirPass>> = match anf_mode {
                        "full" => vec![Box::new(tlang_hir_opt::anf_transform::AnfTransform::<
                            tlang_hir_opt::anf_transform::FullAnfFilter,
                        >::default())],
                        // "minimal" or any other value: use JS-specific filter
                        _ => vec![Box::new(JsAnfTransform::default())],
                    };

                    if self.optimization_options.anf_return_opt.unwrap_or(true) {
                        passes.push(Box::new(JsAnfReturnOpt::default()));
                    }

                    passes.push(Box::new(
                        tlang_hir_opt::symbol_resolution::SymbolResolution::default(),
                    ));

                    if self.optimization_options.constant_folding.unwrap_or(true) {
                        passes.push(Box::new(
                            tlang_hir_opt::constant_folding::ConstantFolding::default(),
                        ));
                    }

                    let mut optimizer = JsHirOptimizer::new(passes);
                    optimizer
                        .optimize_hir(&mut module, &mut ctx)
                        .unwrap_or_else(|err| {
                            panic!("internal compiler error: {err}\n\nThis is a compiler bug. Please file an issue at https://github.com/topaxi/tlang/issues")
                        });
                }
                RunnerKind::Interpreter => {
                    let mut passes: Vec<Box<dyn HirPass>> = Vec::new();

                    // Optional general ANF transform — off by default; reserved for
                    // future bytecode compilation work.
                    if self
                        .optimization_options
                        .anf_transform
                        .as_deref()
                        .unwrap_or("off")
                        == "full"
                    {
                        passes.push(Box::new(tlang_hir_opt::anf_transform::AnfTransform::<
                            tlang_hir_opt::anf_transform::FullAnfFilter,
                        >::default()));
                    }

                    passes.push(Box::new(
                        tlang_hir_opt::symbol_resolution::SymbolResolution::default(),
                    ));

                    if self.optimization_options.constant_folding.unwrap_or(true) {
                        passes.push(Box::new(
                            tlang_hir_opt::constant_folding::ConstantFolding::default(),
                        ));
                    }

                    passes.push(Box::new(
                        tlang_hir_opt::slot_allocation::SlotAllocation::default(),
                    ));

                    let mut optimizer = HirOptimizer::new(passes);
                    optimizer
                        .optimize_hir(&mut module, &mut ctx)
                        .unwrap_or_else(|err| {
                            panic!("internal compiler error: {err}\n\nThis is a compiler bug. Please file an issue at https://github.com/topaxi/tlang/issues")
                        });
                }
            }

            self.interpreter
                .register_constant_pool_ids(constant_pool_ids);
            self.build.hir = Some(module);
        }
    }

    #[wasm_bindgen(js_name = "setOptimizations")]
    pub fn set_optimizations(&mut self, options: JsOptimizationOptions) {
        self.optimization_options = options;
        self.build.hir = None;
        self.js = CodegenJS::default();
    }

    #[wasm_bindgen(js_name = "compileToJS")]
    pub fn compile_to_js(&mut self) -> Result<(), JsError> {
        if self.js.get_output().is_empty() {
            let _ = self.parse();
            self.lower_to_hir();
            if let Some(hir) = self.build.hir.as_ref() {
                self.js
                    .generate_code_with_source_map(hir, "playground.tlang", &self.source);
            }
        }

        Ok(())
    }

    #[wasm_bindgen(js_name = "getSourceMap")]
    pub fn get_source_map(&mut self) -> Option<String> {
        let _ = self.compile_to_js();
        self.js.get_source_map_json()
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
    pub fn hir_pretty(
        &mut self,
        options: Option<JsHirPrettyOptions>,
    ) -> Result<String, serde_wasm_bindgen::Error> {
        self.lower_to_hir();

        if let Some(hir) = self.build.hir.as_ref() {
            let options = if let Some(options) = options {
                options.into_hir_pretty_options()
            } else {
                HirPrettyOptions::default()
            };

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

    #[wasm_bindgen(js_name = "getJSASTString")]
    pub fn js_ast_string(&mut self) -> String {
        let _ = self.compile_to_js();
        self.js.get_js_ast_json().to_string()
    }

    #[wasm_bindgen(js_name = "renderDiagnostics")]
    pub fn render_diagnostics(&mut self) -> String {
        let diagnostics = self.analyzer.get_diagnostics();
        if diagnostics.is_empty() {
            return String::new();
        }
        render_semantic_diagnostics("playground.tlang", &self.source, &diagnostics, true)
    }

    #[wasm_bindgen(js_name = "renderErrorDiagnostics")]
    pub fn render_error_diagnostics(&mut self) -> String {
        let diagnostics: Vec<_> = self
            .analyzer
            .get_diagnostics()
            .into_iter()
            .filter(|d| d.is_error())
            .collect();
        if diagnostics.is_empty() {
            return String::new();
        }
        render_semantic_diagnostics("playground.tlang", &self.source, &diagnostics, true)
    }

    #[wasm_bindgen(js_name = "renderWarningDiagnostics")]
    pub fn render_warning_diagnostics(&mut self) -> String {
        let diagnostics: Vec<_> = self
            .analyzer
            .get_diagnostics()
            .into_iter()
            .filter(|d| d.is_warning())
            .collect();
        if diagnostics.is_empty() {
            return String::new();
        }
        render_semantic_diagnostics("playground.tlang", &self.source, &diagnostics, true)
    }

    #[wasm_bindgen(js_name = "renderParseErrors")]
    pub fn render_parse_errors(&mut self) -> String {
        let source = self.source.clone();
        let issues = self.parse_issues();
        if issues.is_empty() {
            return String::new();
        }
        render_parse_issues("playground.tlang", &source, issues, true)
    }

    #[wasm_bindgen(js_name = "getDiagnostics")]
    pub fn diagnostics(&mut self) -> Result<JsDiagnosticArray, serde_wasm_bindgen::Error> {
        let diagnostics: Vec<JsDiagnostic> = self
            .analyzer
            .get_diagnostics()
            .iter()
            .map(|diagnostic| JsDiagnostic::from(diagnostic.clone()))
            .collect();
        Ok(serde_wasm_bindgen::to_value(&diagnostics)?.unchecked_into())
    }

    #[wasm_bindgen(js_name = "getCodemirrorDiagnostics")]
    pub fn codemirror_diagnostics(
        &mut self,
    ) -> Result<JsCodemirrorDiagnosticArray, serde_wasm_bindgen::Error> {
        let diagnostics = self
            .analyzer
            .get_diagnostics()
            .iter()
            .map(codemirror::from_tlang_diagnostic)
            .collect::<Vec<_>>();
        let parse_errors = self.parse_issues().iter().map(codemirror::from_parse_issue);

        let all: Vec<_> = parse_errors.chain(diagnostics).collect();
        Ok(serde_wasm_bindgen::to_value(&all)?.unchecked_into())
    }
}
