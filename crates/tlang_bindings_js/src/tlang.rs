use std::collections::HashMap;
use std::sync::{Arc, RwLock};

use serde::{Deserialize, Serialize};
use tlang_analysis::symbol_index::SymbolIndex;
use tlang_ast::node::{self as ast};
use tlang_codegen_js::generator::CodegenJS;
use tlang_codegen_js::{
    JsAnfReturnOpt, JsAnfTransform, JsHirOptimizer, TailCallSelfReferenceValidation,
};
use tlang_core::vm::VM;
use tlang_defs::{DefKind, DefScope};
use tlang_diagnostics::{render_ice, render_parse_issues, render_semantic_diagnostics};
use tlang_hir as hir;
use tlang_hir_opt::HirPass;
use tlang_hir_opt::hir_opt::HirOptimizer;
use tlang_hir_pretty::{HirPretty, HirPrettyOptions};
use tlang_parser::Parser;
use tlang_parser::error::{ParseError, ParseIssue};
use tlang_semantics::SemanticAnalyzer;
use tlang_span::NodeId;
use tlang_typeck::TypeChecker;
use tsify::Tsify;
use wasm_bindgen::prelude::*;

use crate::codemirror;
use crate::codemirror::{
    CodemirrorDefinitionLocation, CodemirrorHoverInfo, CodemirrorParameterInformation,
    CodemirrorSignatureHelp, CodemirrorSignatureInformation,
};
use crate::ts_types::{JsDiagnostic, JsParseIssue};

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(typescript_type = "JsDiagnostic[]")]
    pub type JsDiagnosticArray;

    #[wasm_bindgen(typescript_type = "JsParseIssue[]")]
    pub type JsParseIssueArray;

    #[wasm_bindgen(typescript_type = "CodemirrorDiagnostic[]")]
    pub type JsCodemirrorDiagnosticArray;

    #[wasm_bindgen(typescript_type = "CodemirrorCompletion[]")]
    pub type JsCodemirrorCompletionArray;

    #[wasm_bindgen(typescript_type = "CodemirrorInlayHint[]")]
    pub type JsCodemirrorInlayHintArray;

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
    /// Remove unreferenced bindings and declarations. Defaults to `true`.
    pub dead_code_elimination: Option<bool>,
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
    hir_opt_diagnostics: Vec<tlang_diagnostics::Diagnostic>,
    analyzed: bool,
    /// Symbol index built from the semantic analyzer after [`Tlang::analyze`]
    /// is called.  `None` before analysis or when parsing fails.
    symbol_index: Option<SymbolIndex>,
    /// Cached typed HIR for editor features that need inferred callable
    /// signatures. `None` means typed HIR is not available yet, either because
    /// analysis has not run or because lowering/type checking did not succeed.
    typed_hir: Option<tlang_analysis::typed_hir::TypedHir>,
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
                tlang_analysis::configure_js_analyzer(&mut analyzer);
            }
            RunnerKind::Interpreter => {
                let vm_symbols = VM::builtin_symbols();
                analyzer.add_builtin_symbols_with_slots(&vm_symbols);
            }
        }

        Self {
            source,
            runner: runner_kind,
            optimization_options: JsOptimizationOptions {
                constant_folding: Some(true),
                anf_transform: None,
                anf_return_opt: Some(true),
                dead_code_elimination: Some(true),
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

    fn build_failure_message(&mut self) -> Option<String> {
        let parse_errors = self.render_parse_errors();
        if !parse_errors.is_empty() {
            return Some(parse_errors);
        }

        let error_diagnostics = self.render_error_diagnostics();
        if !error_diagnostics.is_empty() {
            return Some(error_diagnostics);
        }

        None
    }

    #[wasm_bindgen]
    pub fn eval(&mut self) -> Result<JsUnknown, JsError> {
        self.lower_to_hir();

        let Some(hir) = self.build.hir.as_ref() else {
            return Err(JsError::new(
                self.build_failure_message()
                    .as_deref()
                    .unwrap_or("Failed to generate HIR"),
            ));
        };

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
            let mut symbol_index = SymbolIndex::from_analyzer(&self.analyzer);
            self.build.typed_hir = if self
                .analyzer
                .get_diagnostics()
                .iter()
                .any(|diagnostic| diagnostic.is_error())
            {
                None
            } else {
                tlang_analysis::typed_hir::lower_and_typecheck_parts(
                    ast,
                    &self.build.constant_pool_node_ids,
                    &self.analyzer,
                )
            };
            if let Some(typed_hir) = self.build.typed_hir.as_ref() {
                symbol_index.populate_type_info(typed_hir);
            }
            self.build.symbol_index = Some(symbol_index);
        }
    }

    /// Get hover information for the symbol at the given UTF-16 position.
    ///
    /// Returns `None` when analysis has not been run, the position is not on
    /// an identifier, or the identifier cannot be resolved.
    #[wasm_bindgen(js_name = "getHoverInfo")]
    pub fn get_hover_info(&self, pos: u32) -> Result<JsValue, serde_wasm_bindgen::Error> {
        let ast = match self.ast() {
            Some(ast) => ast,
            None => return Ok(JsValue::NULL),
        };
        let index = match &self.build.symbol_index {
            Some(idx) => idx,
            None => return Ok(JsValue::NULL),
        };

        let byte_pos = codemirror::utf16_to_byte_offset(&self.source, pos);
        let (line, column) = codemirror::byte_offset_to_line_column(&self.source, byte_pos);

        let resolved = tlang_analysis::query::resolve_symbol(ast, index, line, column);
        match resolved {
            Some(mut sym) => {
                tlang_analysis::query::enrich_hover_symbol(
                    ast,
                    self.build.typed_hir.as_ref(),
                    &mut sym,
                );
                let info = CodemirrorHoverInfo {
                    text: sym.hover_text(),
                    from: codemirror::byte_offset_to_utf16(&self.source, sym.ident_span.start),
                    to: codemirror::byte_offset_to_utf16(&self.source, sym.ident_span.end),
                };
                serde_wasm_bindgen::to_value(&info)
            }
            None => Ok(JsValue::NULL),
        }
    }

    /// Get the inferred type information for the value at the given UTF-16 position.
    #[wasm_bindgen(js_name = "getTypeAtPosition")]
    pub fn get_type_at_position(&self, pos: u32) -> Result<JsValue, serde_wasm_bindgen::Error> {
        let ast = match self.ast() {
            Some(ast) => ast,
            None => return Ok(JsValue::NULL),
        };
        let index = match &self.build.symbol_index {
            Some(idx) => idx,
            None => return Ok(JsValue::NULL),
        };

        let byte_pos = codemirror::utf16_to_byte_offset(&self.source, pos);
        let (line, column) = codemirror::byte_offset_to_line_column(&self.source, byte_pos);

        match tlang_analysis::query::type_at_position(
            &self.source,
            ast,
            index,
            self.build.typed_hir.as_ref(),
            line,
            column,
        ) {
            Some(ty) => serde_wasm_bindgen::to_value(&ty),
            None => Ok(JsValue::NULL),
        }
    }

    /// Get the definition location for the symbol at the given UTF-16 position.
    ///
    /// Returns `None` when the symbol is a builtin (no source location),
    /// analysis has not been run, or the position is not on a resolvable
    /// identifier.
    #[wasm_bindgen(js_name = "getDefinitionLocation")]
    pub fn get_definition_location(&self, pos: u32) -> Result<JsValue, serde_wasm_bindgen::Error> {
        let ast = match self.ast() {
            Some(ast) => ast,
            None => return Ok(JsValue::NULL),
        };
        let index = match &self.build.symbol_index {
            Some(idx) => idx,
            None => return Ok(JsValue::NULL),
        };

        let byte_pos = codemirror::utf16_to_byte_offset(&self.source, pos);
        let (line, column) = codemirror::byte_offset_to_line_column(&self.source, byte_pos);

        let resolved = tlang_analysis::query::resolve_symbol(ast, index, line, column);
        match resolved {
            Some(sym) if !sym.builtin => {
                let loc = CodemirrorDefinitionLocation {
                    from: codemirror::byte_offset_to_utf16(&self.source, sym.def_span.start),
                    to: codemirror::byte_offset_to_utf16(&self.source, sym.def_span.end),
                };
                serde_wasm_bindgen::to_value(&loc)
            }
            _ => Ok(JsValue::NULL),
        }
    }

    /// Get signature help for the call at the given UTF-16 position.
    #[wasm_bindgen(js_name = "getSignatureHelp")]
    pub fn get_signature_help(&mut self, pos: u32) -> Result<JsValue, serde_wasm_bindgen::Error> {
        let target = match self.runner {
            RunnerKind::JavaScript => tlang_analysis::CompilationTarget::Js,
            RunnerKind::Interpreter => tlang_analysis::CompilationTarget::Interpreter,
        };

        let result = tlang_analysis::analyze_for_target(&self.source, target);
        let Some(typed_hir) = tlang_analysis::inlay_hints::lower_and_typecheck(&result) else {
            return Ok(JsValue::NULL);
        };

        let byte_pos = codemirror::utf16_to_byte_offset(&self.source, pos);
        let (line, column) = codemirror::byte_offset_to_line_column(&self.source, byte_pos);

        let Some(help) = tlang_analysis::signature_help::signature_help_at(
            &self.source,
            &typed_hir,
            line,
            column,
        ) else {
            return Ok(JsValue::NULL);
        };

        let help = CodemirrorSignatureHelp {
            signatures: help
                .signatures
                .into_iter()
                .map(|signature| CodemirrorSignatureInformation {
                    label: signature.label,
                    parameters: signature
                        .parameters
                        .into_iter()
                        .map(|param| CodemirrorParameterInformation { label: param.label })
                        .collect(),
                })
                .collect(),
            active_signature: help.active_signature,
            active_parameter: help.active_parameter,
        };

        serde_wasm_bindgen::to_value(&help)
    }

    #[allow(clippy::too_many_lines)]
    fn lower_to_hir(&mut self) {
        let _ = self.parse();

        if self.hir().is_some() {
            return;
        }

        self.build.hir = None;
        self.build.hir_opt_diagnostics.clear();

        if !self.build.analyzed {
            self.analyze();
        }

        if self
            .analyzer
            .get_diagnostics()
            .iter()
            .any(|diagnostic| diagnostic.is_error())
        {
            return;
        }

        if let Some(ast) = self.ast() {
            // Deep-clone the symbol tables so the lowering pass can mutate them
            // (e.g. via `DefScope::shift()`) without corrupting the analyzer's
            // state across multiple lowering invocations (e.g. when toggling
            // optimisation options).
            let symbol_tables: HashMap<NodeId, Arc<RwLock<DefScope>>> = self
                .analyzer
                .symbol_tables()
                .iter()
                .map(|(&k, v)| (k, Arc::new(RwLock::new(v.read().unwrap().clone()))))
                .collect();
            let root_symbol_table = symbol_tables
                .get(&NodeId::new(1))
                .cloned()
                .unwrap_or_default();

            let (mut module, meta) = match tlang_ast_lowering::lower_to_hir(
                ast,
                &self.build.constant_pool_node_ids,
                self.analyzer.symbol_id_allocator(),
                root_symbol_table,
                symbol_tables,
            ) {
                Ok(result) => result,
                Err(errs) => {
                    let diagnostics: Vec<_> = errs
                        .iter()
                        .map(|e| tlang_diagnostics::Diagnostic::error(&e.to_string(), e.span()))
                        .collect();
                    self.build.hir_opt_diagnostics = diagnostics;
                    return;
                }
            };
            let constant_pool_ids = meta.constant_pool_ids.clone();
            let mut ctx = meta.into();

            match self.runner {
                RunnerKind::JavaScript => {
                    let anf_mode = self
                        .optimization_options
                        .anf_transform
                        .as_deref()
                        .unwrap_or("minimal");

                    // TailPositionAnalysis must run first to annotate rec call sites.
                    let mut passes: Vec<Box<dyn HirPass>> = vec![Box::new(
                        tlang_hir_opt::tail_call_validation::TailPositionAnalysis::default(),
                    )];

                    // SymbolResolution must run before ANF so that callee paths have
                    // their `res.hir_id()` set — the ANF pass uses HirId identity to
                    // detect self-referencing tail calls.
                    passes.push(Box::new(
                        tlang_hir_opt::symbol_resolution::SymbolResolution::default(),
                    ));

                    // Warn about non-self-referencing `rec` calls that the JS backend
                    // cannot compile into loops. Must run after SymbolResolution.
                    passes.push(Box::new(TailCallSelfReferenceValidation::default()));

                    match anf_mode {
                        "full" => {
                            passes.push(Box::new(tlang_hir_opt::anf_transform::AnfTransform::<
                                tlang_hir_opt::anf_transform::FullAnfFilter,
                            >::default()));
                        }
                        // "minimal" or any other value: use JS-specific filter
                        _ => passes.push(Box::new(JsAnfTransform::default())),
                    }

                    if self.optimization_options.anf_return_opt.unwrap_or(true) {
                        passes.push(Box::new(JsAnfReturnOpt::default()));
                    }

                    if self.optimization_options.constant_folding.unwrap_or(true) {
                        passes.push(Box::new(
                            tlang_hir_opt::constant_folding::ConstantFolding::default(),
                        ));
                    }

                    if self
                        .optimization_options
                        .dead_code_elimination
                        .unwrap_or(true)
                    {
                        passes.push(Box::new(
                            tlang_hir_opt::dead_code_elimination::DeadCodeElimination::default(),
                        ));
                    }

                    let mut optimizer = JsHirOptimizer::new(passes);
                    optimizer
                        .optimize_hir(&mut module, &mut ctx)
                        .unwrap_or_else(|err| panic!("{}", render_ice(&err)));
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

                    if self
                        .optimization_options
                        .dead_code_elimination
                        .unwrap_or(true)
                    {
                        passes.push(Box::new(
                            tlang_hir_opt::dead_code_elimination::DeadCodeElimination::default(),
                        ));
                    }

                    passes.push(Box::new(
                        tlang_hir_opt::slot_allocation::SlotAllocation::default(),
                    ));

                    passes.push(Box::new(
                        tlang_hir_opt::free_variable_analysis::FreeVariableAnalysis,
                    ));

                    let mut optimizer = HirOptimizer::new(passes);
                    optimizer
                        .optimize_hir(&mut module, &mut ctx)
                        .unwrap_or_else(|err| panic!("{}", render_ice(&err)));
                }
            }

            let mut type_checker = TypeChecker::new();
            type_checker
                .optimize_hir(&mut module, &mut ctx)
                .unwrap_or_else(|err| panic!("{}", render_ice(&err)));

            // ExhaustiveEnumMatch must run after type checking — it uses
            // type-checked pattern types to verify catch-all arms.
            let mut exhaustive_enum = tlang_hir_opt::ExhaustiveEnumMatch::default();
            exhaustive_enum
                .optimize_hir(&mut module, &mut ctx)
                .unwrap_or_else(|err| panic!("{}", render_ice(&err)));

            let has_type_errors = ctx
                .diagnostics
                .iter()
                .any(|diagnostic| diagnostic.is_error());
            self.build.hir_opt_diagnostics = ctx.diagnostics;

            if has_type_errors {
                return;
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
        self.build.hir_opt_diagnostics.clear();
        self.js = CodegenJS::default();
    }

    #[wasm_bindgen(js_name = "compileToJS")]
    pub fn compile_to_js(&mut self) -> Result<(), JsError> {
        if self.js.get_output().is_empty() {
            let _ = self.parse();
            self.lower_to_hir();
            let Some(hir) = self.build.hir.as_ref() else {
                return Err(JsError::new(
                    self.build_failure_message()
                        .as_deref()
                        .unwrap_or("Failed to generate HIR"),
                ));
            };
            self.js
                .generate_code_with_source_map(hir, "playground.tlang", &self.source)
                .map_err(|errors| {
                    let msgs: Vec<String> = errors.iter().map(|e| e.to_string()).collect();
                    JsError::new(&msgs.join("\n"))
                })?;
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
        let diagnostics: Vec<_> = self
            .analyzer
            .get_diagnostics()
            .into_iter()
            .chain(self.build.hir_opt_diagnostics.iter().cloned())
            .collect();
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
            .chain(self.build.hir_opt_diagnostics.iter().cloned())
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
            .chain(self.build.hir_opt_diagnostics.iter().cloned())
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
            .chain(self.build.hir_opt_diagnostics.iter())
            .map(|diagnostic| JsDiagnostic::from(diagnostic.clone()))
            .collect();
        Ok(serde_wasm_bindgen::to_value(&diagnostics)?.unchecked_into())
    }

    #[wasm_bindgen(js_name = "getCodemirrorDiagnostics")]
    pub fn codemirror_diagnostics(
        &mut self,
    ) -> Result<JsCodemirrorDiagnosticArray, serde_wasm_bindgen::Error> {
        // Collect parse issues first so we can borrow `self.source` afterwards.
        let parse_issues = self.parse_issues().to_vec();

        let source = &self.source;
        let diagnostics = self
            .analyzer
            .get_diagnostics()
            .iter()
            .chain(self.build.hir_opt_diagnostics.iter())
            .map(|d| codemirror::from_tlang_diagnostic(d, source))
            .collect::<Vec<_>>();
        let parse_errors = parse_issues
            .iter()
            .map(|issue| codemirror::from_parse_issue(issue, source));

        let all: Vec<_> = parse_errors.chain(diagnostics).collect();
        Ok(serde_wasm_bindgen::to_value(&all)?.unchecked_into())
    }

    /// Return completion items derived from the semantic analyzer's symbol tables.
    ///
    /// This delegates to [`SymbolIndex::collect_completion_items`] — the
    /// canonical implementation shared with the LSP server — and converts
    /// the protocol-agnostic items into CodeMirror-compatible format.
    #[wasm_bindgen(js_name = "getCompletionItems")]
    #[allow(clippy::missing_panics_doc)]
    pub fn completion_items(
        &mut self,
    ) -> Result<JsCodemirrorCompletionArray, serde_wasm_bindgen::Error> {
        if !self.build.analyzed {
            self.analyze();
        }

        let analysis_items = match &self.build.symbol_index {
            Some(index) => index.collect_completion_items(),
            None => vec![],
        };

        let items: Vec<codemirror::CodemirrorCompletion> = analysis_items
            .into_iter()
            .map(|item| codemirror::CodemirrorCompletion {
                label: item.label,
                completion_type: codemirror::completion_type_from_def_kind(item.kind),
                detail: item.detail,
            })
            .collect();

        Ok(serde_wasm_bindgen::to_value(&items)?.unchecked_into())
    }

    /// Return inlay hints for the current source.
    ///
    /// Runs the full type-checking pipeline (analysis → HIR lowering →
    /// optimisation → type checking) and collects hints for variable bindings,
    /// function return types, and function parameters.
    ///
    /// Positions in the returned hints are UTF-16 code unit offsets suitable
    /// for use with CodeMirror 6's text model.
    #[wasm_bindgen(js_name = "getInlayHints")]
    #[allow(clippy::missing_panics_doc)]
    pub fn get_inlay_hints(
        &mut self,
    ) -> Result<JsCodemirrorInlayHintArray, serde_wasm_bindgen::Error> {
        if !self.build.analyzed {
            self.analyze();
        }

        let target = match self.runner {
            RunnerKind::JavaScript => tlang_analysis::CompilationTarget::Js,
            RunnerKind::Interpreter => tlang_analysis::CompilationTarget::Interpreter,
        };

        let result = tlang_analysis::analyze_for_target(&self.source, target);
        let hints = match tlang_analysis::inlay_hints::lower_and_typecheck(&result) {
            Some(typed_hir) => {
                let analysis_hints =
                    tlang_analysis::inlay_hints::collect_inlay_hints(&typed_hir, None);
                analysis_hints
                    .into_iter()
                    .map(|h| {
                        let byte_offset = codemirror::line_column_to_byte_offset(
                            &self.source,
                            h.line,
                            h.character,
                        );
                        let utf16_pos = codemirror::byte_offset_to_utf16(&self.source, byte_offset);
                        codemirror::CodemirrorInlayHint {
                            position: utf16_pos,
                            label: h.label,
                            kind: match h.kind {
                                tlang_analysis::inlay_hints::InlayHintKind::Type => {
                                    "type".to_string()
                                }
                                tlang_analysis::inlay_hints::InlayHintKind::ReturnType => {
                                    "returnType".to_string()
                                }
                                tlang_analysis::inlay_hints::InlayHintKind::ChainedPipeline => {
                                    "chainedPipeline".to_string()
                                }
                            },
                        }
                    })
                    .collect()
            }
            None => vec![],
        };

        Ok(serde_wasm_bindgen::to_value(&hints)?.unchecked_into())
    }
}
