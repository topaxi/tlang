use std::{cell::RefCell, collections::HashMap, rc::Rc};

use log::debug;
use tlang_ast::{
    node::{
        BinaryOpKind, Expr, ExprKind, FunctionDeclaration, LetDeclaration, Module, Pat, PatKind,
        Path, Stmt, StmtKind, StructDeclaration,
    },
    token::Literal,
    visit::{Visitor, walk_expr, walk_pat, walk_stmt},
};
use tlang_span::{NodeId, Span};
use tlang_symbols::{SymbolIdAllocator, SymbolInfo, SymbolTable, SymbolType};

use crate::{
    declarations::{DeclarationAnalyzer, DeclarationContext},
    diagnostic::{self, Diagnostic},
};

/**
 * Context for semantic analysis, containing only shared state needed
 * during the visitor traversal (similar to DeclarationContext).
 */
pub struct SemanticAnalysisContext {
    pub declaration_context: DeclarationContext,
    pub struct_declarations: HashMap<String, StructDeclaration>,
}

impl SemanticAnalysisContext {
    pub fn new(declaration_context: DeclarationContext) -> Self {
        SemanticAnalysisContext {
            declaration_context,
            struct_declarations: HashMap::new(),
        }
    }

    pub fn get_symbol_table(&self, id: NodeId) -> Option<Rc<RefCell<SymbolTable>>> {
        self.declaration_context.symbol_tables().get(&id).cloned()
    }
}

pub struct SemanticAnalyzer {
    declaration_analyzer: DeclarationAnalyzer,
    context: Option<SemanticAnalysisContext>,
    symbol_table_stack: Vec<Rc<RefCell<SymbolTable>>>,
    diagnostics: Vec<Diagnostic>,
    in_pipeline_rhs: bool, // Track if we're analyzing the RHS of a pipeline
}

impl Default for SemanticAnalyzer {
    fn default() -> Self {
        Self::new(DeclarationAnalyzer::default())
    }
}

impl SemanticAnalyzer {
    pub fn new(declaration_analyzer: DeclarationAnalyzer) -> Self {
        SemanticAnalyzer {
            declaration_analyzer,
            context: None,
            symbol_table_stack: vec![],
            diagnostics: vec![],
            in_pipeline_rhs: false,
        }
    }

    fn current_symbol_table(&self) -> Rc<RefCell<SymbolTable>> {
        self.symbol_table_stack.last().cloned().unwrap()
    }

    fn push_symbol_table(&mut self, symbol_table: &Rc<RefCell<SymbolTable>>) {
        self.symbol_table_stack.push(symbol_table.clone());
    }

    fn pop_symbol_table(&mut self) -> Rc<RefCell<SymbolTable>> {
        self.symbol_table_stack.pop().unwrap()
    }

    #[inline(always)]
    /// # Panics
    /// Panics if analysis has not been run first.
    pub fn symbol_tables(&self) -> &HashMap<NodeId, Rc<RefCell<SymbolTable>>> {
        let ctx = self.context.as_ref().expect("Analysis must be run first");
        ctx.declaration_context.symbol_tables()
    }

    #[inline(always)]
    pub fn get_symbol_table(&self, id: NodeId) -> Option<Rc<RefCell<SymbolTable>>> {
        self.symbol_tables().get(&id).cloned()
    }

    /// # Panics
    /// Panics if analysis has not been run first.
    pub fn root_symbol_table(&self) -> Rc<RefCell<SymbolTable>> {
        let ctx = self.context.as_ref().expect("Analysis must be run first");
        ctx.declaration_context.root_symbol_table().clone()
    }

    /// # Panics
    /// Panics if analysis has not been run first.
    pub fn symbol_id_allocator(&self) -> SymbolIdAllocator {
        let ctx = self.context.as_ref().expect("Analysis must be run first");
        ctx.declaration_context.symbol_id_allocator()
    }

    pub fn get_diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    pub fn get_errors(&self) -> Vec<Diagnostic> {
        self.diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.is_error())
            .cloned()
            .collect()
    }

    pub fn get_struct_declaration(&self, name: &str) -> Option<&StructDeclaration> {
        self.context.as_ref()?.struct_declarations.get(name)
    }

    pub fn add_builtin_symbols<'a, S, I>(&mut self, symbols: I)
    where
        S: AsRef<str> + 'a,
        I: IntoIterator<Item = &'a (S, SymbolType)>,
    {
        // Initialize context if it doesn't exist
        if self.context.is_none() {
            let declaration_context = DeclarationContext::new();
            self.context = Some(SemanticAnalysisContext::new(declaration_context));
        }

        if let Some(ref mut ctx) = self.context {
            ctx.declaration_context.add_builtin_symbols(symbols);
        }
    }

    pub fn analyze(&mut self, module: &Module) -> Result<(), Vec<Diagnostic>> {
        self.analyze_root_module(module, false)
    }

    pub fn analyze_as_root_module(&mut self, module: &Module) -> Result<(), Vec<Diagnostic>> {
        self.analyze_root_module(module, true)
    }

    fn analyze_root_module(
        &mut self,
        module: &Module,
        is_root: bool,
    ) -> Result<(), Vec<Diagnostic>> {
        // First run declaration analysis to collect all declarations
        let declaration_context = if let Some(ref mut existing_ctx) = self.context {
            // Reuse existing context with builtin symbols
            self.declaration_analyzer.analyze_with_context(
                module,
                is_root,
                &mut existing_ctx.declaration_context,
            );
            std::mem::take(&mut existing_ctx.declaration_context)
        } else {
            // Create new context
            self.declaration_analyzer.analyze(module, is_root)
        };

        // Create analysis context and reset analyzer state
        let mut context = SemanticAnalysisContext::new(declaration_context);
        self.symbol_table_stack.clear();
        self.diagnostics.clear();
        self.in_pipeline_rhs = false;

        // Initialize symbol table stack with root table
        let root_table = context.declaration_context.root_symbol_table().clone();
        self.push_symbol_table(&root_table);

        // Perform semantic analysis using visitor pattern
        self.visit_module(module, &mut context);

        // Pop root symbol table (with unused symbol reporting)
        let root_table = self.pop_symbol_table();
        self.report_unused_symbols(&root_table, &mut context);

        // Store context
        self.context = Some(context);

        if self.get_errors().is_empty() {
            Ok(())
        } else {
            Err(self.get_errors())
        }
    }

    fn mark_as_used_by_name(&mut self, name: &str, span: Span) {
        let symbol_info = self
            .current_symbol_table()
            .borrow()
            .get_closest_by_name(name, span);
        if let Some(symbol_info) = symbol_info {
            self.current_symbol_table()
                .borrow_mut()
                .mark_as_used(symbol_info.id);
        } else {
            self.report_undeclared_variable(name, span);
        }
    }

    fn mark_as_used_by_name_and_arity(&mut self, name: &str, arity: usize, span: Span) {
        let symbol_info_ids: Vec<_> = self
            .current_symbol_table()
            .borrow()
            .get_by_name_and_arity(name, arity)
            .iter()
            .map(|s| s.id)
            .collect();

        if symbol_info_ids.is_empty() {
            self.report_undeclared_function(name, arity, span);
            return;
        }

        for symbol_id in symbol_info_ids {
            // TODO: This only marks the fn by it's name and arity, not by it's id.
            //       Maybe we should mark it in the collection pass?
            //       Or keep track of the current id in case the name is being shadowed?
            self.current_symbol_table()
                .borrow_mut()
                .mark_as_used(symbol_id);
        }
    }

    fn report_undeclared_variable(&mut self, name: &str, span: Span) {
        let did_you_mean = did_you_mean(
            name,
            &self
                .current_symbol_table()
                .borrow()
                .get_all_declared_symbols(),
        );

        if let Some(suggestion) = did_you_mean {
            self.diagnostics.push(diagnostic::error_at!(
                span,
                "Use of undeclared variable `{name}`, did you mean the {} `{}`?",
                suggestion.symbol_type,
                suggestion.name,
            ));
        } else {
            self.diagnostics.push(diagnostic::error_at!(
                span,
                "Use of undeclared variable `{name}`",
            ));
        }
    }

    fn report_undeclared_function(&mut self, name: &str, arity: usize, span: Span) {
        let did_you_mean = did_you_mean(
            name,
            &self
                .current_symbol_table()
                .borrow()
                .get_all_declared_symbols(),
        );

        if let Some(suggestion) = did_you_mean {
            self.diagnostics.push(diagnostic::error_at!(
                span,
                "Use of undeclared function `{name}` with arity {arity}, did you mean the {} `{}`?",
                suggestion.symbol_type,
                suggestion.name
            ));
        } else {
            self.diagnostics.push(diagnostic::error_at!(
                span,
                "Use of undeclared function `{name}` with arity {arity}",
            ));
        }
    }

    fn analyze_path(&mut self, path: &Path, span: Span) {
        debug!("Analyzing path: {}", path);

        let mut path_str = String::new();

        for segment in &path.segments {
            path_str.push_str(segment.as_str());
            self.mark_as_used_by_name(&path_str, span);
            path_str.push_str("::");
        }
    }

    fn analyze_path_with_known_arity(&mut self, path: &Path, arity: usize, span: Span) {
        debug!(
            "Analyzing path with known arity: {}, arity: {}",
            path, arity
        );

        let mut path_str = String::new();

        for segment in path.segments.iter().take(path.segments.len() - 1) {
            path_str.push_str(segment.as_str());
            self.mark_as_used_by_name(&path_str, span);
            path_str.push_str("::");
        }

        self.mark_as_used_by_name_and_arity(&path.to_string(), arity, span);
    }

    fn analyze_variable_declaration(
        &mut self,
        decl: &LetDeclaration,
        ctx: &mut SemanticAnalysisContext,
    ) {
        self.visit_pat(&decl.pattern, ctx);

        // When declaring a variable, we can only reference symbols that were declared before.
        // This includes our own variable name.
        // E.g. `let a = a;` is not allowed. But `let a = 1; let a = a;` is.
        // By marking the symbol as undeclared from the table while analyzing the expression,
        // we can check whether the expression references any symbols that were not declared before.
        let pattern_symbols = decl
            .pattern
            .get_all_node_ids()
            .iter()
            .filter_map(|id| {
                self.current_symbol_table()
                    .borrow_mut()
                    .get_local(|s| s.node_id == Some(*id))
                    .map(|s| s.id)
            })
            .collect::<Vec<_>>();

        for symbol_id in &pattern_symbols {
            self.current_symbol_table()
                .borrow_mut()
                .set_declared(*symbol_id, false);
        }

        self.visit_expr(&decl.expression, ctx);

        for symbol_id in &pattern_symbols {
            self.current_symbol_table()
                .borrow_mut()
                .set_declared(*symbol_id, true);
        }
    }

    fn report_unused_symbols(
        &mut self,
        symbol_table: &Rc<RefCell<SymbolTable>>,
        _ctx: &mut SemanticAnalysisContext,
    ) {
        let symbol_table = symbol_table.borrow();
        let unused_symbols = symbol_table
            .get_all_declared_local_symbols()
            .filter(|symbol| !symbol.used)
            .filter(|symbol| !symbol.is_builtin())
            // Do not report the binding introduced within function bodies to reference to
            // themselves.
            .filter(|symbol| !symbol.is_fn_self_binding())
            .filter(|symbol| !symbol.name.starts_with('_'))
            // TODO: We currently do not track member methods, as we do not have any type
            //       information yet.
            .filter(|symbol| !symbol.name.contains('.'))
            .collect::<Vec<_>>();

        for unused_symbol in &unused_symbols {
            if unused_symbol.is_any_fn() {
                self.diagnostics.push(diagnostic::warn_at!(
                    unused_symbol.defined_at,
                    "Unused {} `{}/{}`",
                    unused_symbol.symbol_type,
                    unused_symbol.name,
                    unused_symbol.symbol_type.arity().unwrap(),
                ));
            } else {
                self.diagnostics.push(diagnostic::warn_at!(
                    unused_symbol.defined_at,
                    "Unused {} `{}`, if this is intentional, prefix the name with an underscore: `_{}`",
                    unused_symbol.symbol_type, unused_symbol.name, unused_symbol.name
                ));
            }
        }
    }

    fn validate_escape_sequences(&mut self, string_content: &str, span: Span) {
        let mut chars = string_content.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch == '\\'
                && let Some(&next_ch) = chars.peek()
            {
                // Check if this is a valid escape sequence
                // Valid escape sequences in tlang: \", \', \\, \n, \t, \r, \0, \u{...}
                match next_ch {
                    '"' | '\'' | '\\' | 'n' | 't' | 'r' | '0' => {
                        // Valid escape sequence, no warning needed
                        chars.next(); // consume the character after backslash
                    }
                    'u' => {
                        // Check for Unicode escape sequence \u{...}
                        chars.next(); // consume 'u'
                        if let Some(&'{') = chars.peek() {
                            chars.next(); // consume '{'
                            let mut hex_digits = String::new();
                            let mut found_closing_brace = false;

                            // Collect hex digits until we find '}'
                            while let Some(&hex_ch) = chars.peek() {
                                if hex_ch == '}' {
                                    chars.next(); // consume '}'
                                    found_closing_brace = true;
                                    break;
                                } else if hex_ch.is_ascii_hexdigit() && hex_digits.len() < 6 {
                                    hex_digits.push(hex_ch);
                                    chars.next();
                                } else {
                                    // Invalid character or too many digits
                                    break;
                                }
                            }

                            // Validate the Unicode escape sequence
                            if !found_closing_brace {
                                self.diagnostics.push(diagnostic::warn_at!(
                                    span,
                                    "Unterminated Unicode escape sequence in string literal",
                                ));
                            } else if hex_digits.is_empty() {
                                self.diagnostics.push(diagnostic::warn_at!(
                                    span,
                                    "Empty Unicode escape sequence in string literal",
                                ));
                            } else if let Ok(code_point) = u32::from_str_radix(&hex_digits, 16) {
                                if char::from_u32(code_point).is_none() {
                                    self.diagnostics.push(diagnostic::warn_at!(
                                        span,
                                        "Invalid Unicode code point in string literal",
                                    ));
                                }
                                // Valid Unicode escape sequence, no warning needed
                            } else {
                                self.diagnostics.push(diagnostic::warn_at!(
                                    span,
                                    "Invalid hexadecimal in Unicode escape sequence",
                                ));
                            }
                        } else {
                            // \u not followed by {, treat as unknown escape sequence
                            self.diagnostics.push(diagnostic::warn_at!(
                                span,
                                "Unknown escape sequence '\\{}' in string literal",
                                next_ch
                            ));
                        }
                    }
                    _ => {
                        // Unknown escape sequence, emit warning
                        self.diagnostics.push(diagnostic::warn_at!(
                            span,
                            "Unknown escape sequence '\\{}' in string literal",
                            next_ch
                        ));
                        chars.next(); // consume the character after backslash
                    }
                }
            }
        }
    }
}

impl SemanticAnalyzer {
    fn visit_expr_in_pipeline_context(&mut self, expr: &Expr, ctx: &mut SemanticAnalysisContext) {
        match &expr.kind {
            ExprKind::Call(call_expr) | ExprKind::RecursiveCall(call_expr) => {
                // For direct calls in pipeline context, add +1 to arity
                for argument in &call_expr.arguments {
                    self.visit_expr(argument, ctx);
                }

                if let ExprKind::Path(path) = &call_expr.callee.kind {
                    let arity = call_expr.arguments.len() + 1; // +1 for the pipeline operator
                    self.analyze_path_with_known_arity(path, arity, call_expr.callee.span);
                } else {
                    self.visit_expr(&call_expr.callee, ctx);
                }
            }
            _ => self.visit_expr(expr, ctx),
        }
    }
}

impl<'ast> Visitor<'ast> for SemanticAnalyzer {
    type Context = SemanticAnalysisContext;

    fn enter_scope(&mut self, node_id: NodeId, ctx: &mut Self::Context) {
        if let Some(symbol_table) = ctx.get_symbol_table(node_id) {
            self.push_symbol_table(&symbol_table);
        }
    }

    fn leave_scope(&mut self, node_id: NodeId, ctx: &mut Self::Context) {
        if ctx.get_symbol_table(node_id).is_some() {
            let symbol_table = self.pop_symbol_table();
            self.report_unused_symbols(&symbol_table, ctx);
        }
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt, ctx: &mut Self::Context) {
        match &stmt.kind {
            StmtKind::Let(decl) => {
                self.analyze_variable_declaration(decl, ctx);
            }
            _ => walk_stmt(self, stmt, ctx),
        }
    }

    fn visit_struct_decl(
        &mut self,
        decl: &'ast tlang_ast::node::StructDeclaration,
        ctx: &mut Self::Context,
    ) {
        ctx.struct_declarations
            .insert(decl.name.to_string(), decl.clone());
    }

    fn visit_fn_decl(&mut self, decl: &'ast FunctionDeclaration, ctx: &mut Self::Context) {
        // Don't visit the function name expression as that would mark it as used
        // The function declaration itself is handled by the declaration analyzer

        self.enter_scope(decl.id, ctx);

        for parameter in &decl.parameters {
            self.visit_fn_param(parameter, ctx);
        }

        if let Some(guard) = &decl.guard {
            self.visit_expr(guard, ctx);
        }

        if let Some(return_type_annotation) = &decl.return_type_annotation {
            self.visit_fn_ret_ty(return_type_annotation, ctx);
        }

        self.visit_fn_body(&decl.body, ctx);

        self.leave_scope(decl.id, ctx);
    }

    fn visit_expr(&mut self, expr: &'ast Expr, ctx: &mut Self::Context) {
        match &expr.kind {
            ExprKind::BinaryOp(binary_expr) => {
                self.visit_expr(&binary_expr.lhs, ctx);

                // Set pipeline context for RHS if this is a pipeline operator
                if binary_expr.op == BinaryOpKind::Pipeline {
                    // For pipeline operators, we need to handle the RHS specially
                    // but only set the pipeline context for direct function calls
                    self.visit_expr_in_pipeline_context(&binary_expr.rhs, ctx);
                } else {
                    self.visit_expr(&binary_expr.rhs, ctx);
                }
            }
            ExprKind::Call(call_expr) | ExprKind::RecursiveCall(call_expr) => {
                for argument in &call_expr.arguments {
                    self.visit_expr(argument, ctx);
                }

                if let ExprKind::Path(path) = &call_expr.callee.kind {
                    let arity = call_expr.arguments.len(); // Normal arity for non-pipeline calls
                    self.analyze_path_with_known_arity(path, arity, call_expr.callee.span);
                } else {
                    self.visit_expr(&call_expr.callee, ctx);
                }
            }
            ExprKind::Path(path) => {
                self.analyze_path(path, expr.span);
            }
            ExprKind::Dict(kvs) => {
                for (_key, value) in kvs {
                    // TODO: Keys are currently not properly analyzed
                    // Don't analyze keys as they are field names, not variable references
                    self.visit_expr(value, ctx);
                }
            }
            ExprKind::FieldExpression(field_expr) => {
                self.visit_expr(&field_expr.base, ctx);
                // TODO: We are checking for unused variables, this should be refactored into
                //       it's own pass. Skipping analyzing field of variable as we do not have
                //       any type information yet.
                // Don't analyze the field name as it's not a variable reference
            }
            _ => walk_expr(self, expr, ctx),
        }
    }

    fn visit_pat(&mut self, pat: &'ast Pat, ctx: &mut Self::Context) {
        match &pat.kind {
            PatKind::Enum(enum_pattern) => {
                self.analyze_path(&enum_pattern.path, pat.span);

                for (_ident, pat) in &enum_pattern.elements {
                    // Don't analyze the field name as it's not a variable reference,
                    // just visit the pattern
                    self.visit_pat(pat, ctx);
                }
            }
            _ => walk_pat(self, pat, ctx),
        }
    }

    fn visit_literal(&mut self, literal: &'ast Literal, span: Span, _ctx: &mut Self::Context) {
        match literal {
            Literal::String(string_content) | Literal::Char(string_content) => {
                self.validate_escape_sequences(string_content, span);
            }
            _ => {
                // No validation needed for other literal types
            }
        }
    }
}

fn did_you_mean(name: &str, candidates: &[SymbolInfo]) -> Option<SymbolInfo> {
    let mut best_distance = usize::MAX;
    let mut best_candidate = None;
    for candidate in candidates {
        let distance = levenshtein_distance(name, &candidate.name);
        if distance < best_distance {
            best_distance = distance;
            best_candidate = Some(candidate);
        }
    }
    if best_distance < 3 {
        Some(best_candidate.unwrap().clone())
    } else {
        None
    }
}

fn levenshtein_distance(a: &str, b: &str) -> usize {
    let mut matrix = vec![vec![0; b.len() + 1]; a.len() + 1];
    #[allow(clippy::needless_range_loop)]
    for i in 0..=a.len() {
        matrix[i][0] = i;
    }
    for j in 0..=b.len() {
        matrix[0][j] = j;
    }
    for j in 1..=b.len() {
        for i in 1..=a.len() {
            let substitution_cost = if a.chars().nth(i - 1) == b.chars().nth(j - 1) {
                0
            } else {
                1
            };
            matrix[i][j] = *[
                matrix[i - 1][j] + 1,
                matrix[i][j - 1] + 1,
                matrix[i - 1][j - 1] + substitution_cost,
            ]
            .iter()
            .min()
            .unwrap();
        }
    }
    matrix[a.len()][b.len()]
}
