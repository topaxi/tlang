use std::{cell::RefCell, collections::HashMap, rc::Rc};

use log::debug;
use tlang_ast::{
    NodeId,
    node::{
        BinaryOpKind, Block, Expr, ExprKind, FunctionDeclaration, FunctionParameter,
        LetDeclaration, Module, Pat, PatKind, Path, Stmt, StmtKind, StructDeclaration,
    },
    symbols::{SymbolIdAllocator, SymbolInfo, SymbolTable, SymbolType},
};
use tlang_span::Span;

use crate::{
    declarations::DeclarationAnalyzer,
    diagnostic::{self, Diagnostic},
};

pub struct SemanticAnalyzer {
    declaration_analyzer: DeclarationAnalyzer,
    symbol_table_stack: Vec<Rc<RefCell<SymbolTable>>>,
    diagnostics: Vec<Diagnostic>,
    struct_declarations: HashMap<String, StructDeclaration>,
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
            symbol_table_stack: vec![],
            diagnostics: vec![],
            struct_declarations: HashMap::new(),
        }
    }

    #[inline(always)]
    pub fn symbol_tables(&self) -> &HashMap<NodeId, Rc<RefCell<SymbolTable>>> {
        self.declaration_analyzer.symbol_tables()
    }

    #[inline(always)]
    pub fn get_symbol_table(&self, id: NodeId) -> Option<Rc<RefCell<SymbolTable>>> {
        self.symbol_tables().get(&id).cloned()
    }

    pub fn symbol_id_allocator(&self) -> SymbolIdAllocator {
        self.declaration_analyzer.symbol_id_allocator()
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
        self.struct_declarations.get(name)
    }

    fn current_symbol_table(&self) -> Rc<RefCell<SymbolTable>> {
        Rc::clone(self.symbol_table_stack.last().unwrap())
    }

    fn push_symbol_table(&mut self, symbol_table: &Rc<RefCell<SymbolTable>>) {
        self.symbol_table_stack.push(Rc::clone(symbol_table));
    }

    fn pop_symbol_table(&mut self) -> Rc<RefCell<SymbolTable>> {
        self.symbol_table_stack.pop().unwrap()
    }

    pub fn add_builtin_symbols<'a, S, I>(&mut self, symbols: I)
    where
        S: AsRef<str> + 'a,
        I: IntoIterator<Item = &'a (S, SymbolType)>,
    {
        self.declaration_analyzer.add_builtin_symbols(symbols);
    }

    pub fn analyze(&mut self, module: &Module) -> Result<(), Vec<Diagnostic>> {
        self.collect_declarations(module);
        // self.collect_initializations(ast);
        self.analyze_module(module);

        if self.get_errors().is_empty() {
            Ok(())
        } else {
            Err(self.get_errors())
        }
    }

    fn collect_declarations(&mut self, module: &Module) {
        self.declaration_analyzer.analyze(module);
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

    #[inline(always)]
    fn analyze_optional_expr(&mut self, expr: &Option<Expr>) {
        if let Some(expr) = expr {
            self.analyze_expr(expr, false);
        }
    }

    fn analyze_block(&mut self, block: &Block) {
        if let Some(symbol_table) = &self.get_symbol_table(block.id) {
            self.push_symbol_table(symbol_table);
        }

        for stmt in &block.statements {
            self.analyze_stmt(stmt);
        }

        self.analyze_optional_expr(&block.expression);

        if let Some(symbol_table) = &self.get_symbol_table(block.id) {
            self.report_unused_symbols(symbol_table);
            self.pop_symbol_table();
        }
    }

    fn analyze_stmt(&mut self, stmt: &Stmt) {
        if let Some(symbol_table) = &self.get_symbol_table(stmt.id) {
            self.push_symbol_table(symbol_table);
        }

        match &stmt.kind {
            StmtKind::None => {}
            StmtKind::Expr(expr) => self.analyze_expr(expr, false),
            StmtKind::Let(decl) => self.analyze_variable_declaration(decl),
            StmtKind::FunctionDeclaration(decl) => self.analyze_fn_decl(decl),
            StmtKind::FunctionDeclarations(decls) => {
                for decl in decls {
                    self.analyze_fn_decl(decl);
                }
            }
            StmtKind::Return(Some(expr)) => self.analyze_expr(expr, false),
            StmtKind::Return(_) => {}
            StmtKind::EnumDeclaration(_decl) => {
                // TODO
            }
            StmtKind::StructDeclaration(decl) => {
                self.struct_declarations
                    .insert(decl.name.to_string(), *decl.clone());
            }
        }

        if let Some(symbol_table) = &self.get_symbol_table(stmt.id) {
            self.report_unused_symbols(symbol_table);
            self.pop_symbol_table();
        }
    }

    fn analyze_fn_param(&mut self, param: &FunctionParameter) {
        self.analyze_pat(&param.pattern);
    }

    fn analyze_fn_decl(&mut self, decl: &FunctionDeclaration) {
        if let Some(symbol_table) = &self.get_symbol_table(decl.id) {
            self.push_symbol_table(symbol_table);
        }

        for parameter in &decl.parameters {
            self.analyze_fn_param(parameter);
        }

        if let Some(guard) = &decl.guard {
            self.analyze_expr(guard, false);
        }

        self.analyze_block(&decl.body);

        if let Some(symbol_table) = &self.get_symbol_table(decl.id) {
            self.report_unused_symbols(symbol_table);
            self.pop_symbol_table();
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

    fn analyze_expr(&mut self, expr: &Expr, parent_pipeline: bool) {
        if let Some(symbol_table) = &self.get_symbol_table(expr.id) {
            self.push_symbol_table(symbol_table);
        }

        match &expr.kind {
            ExprKind::BinaryOp(expr) => {
                self.analyze_expr(&expr.lhs, false);
                self.analyze_expr(&expr.rhs, expr.op == BinaryOpKind::Pipeline);
            }
            ExprKind::Block(block) | ExprKind::Loop(block) => self.analyze_block(block),
            ExprKind::ForLoop(for_loop) => {
                self.analyze_expr(&for_loop.iter, false);
                self.analyze_pat(&for_loop.pat);

                if let Some((pat, expr)) = &for_loop.acc {
                    self.analyze_pat(pat);
                    self.analyze_expr(expr, false);
                }

                self.analyze_block(&for_loop.block);

                if let Some(else_block) = &for_loop.else_block {
                    self.analyze_block(else_block);
                }
            }
            ExprKind::UnaryOp(_, node) => {
                self.analyze_expr(node, false);
            }
            ExprKind::Break(expr) => {
                if let Some(expr) = expr {
                    self.analyze_expr(expr, false);
                }
            }
            ExprKind::Call(expr) | ExprKind::RecursiveCall(expr) => {
                for argument in &expr.arguments {
                    self.analyze_expr(argument, false);
                }

                if let ExprKind::Path(path) = &expr.callee.kind {
                    let arity = if parent_pipeline {
                        expr.arguments.len() + 1 // +1 for the pipeline operator
                    } else {
                        expr.arguments.len()
                    };
                    self.analyze_path_with_known_arity(path, arity, expr.callee.span);
                } else {
                    self.analyze_expr(&expr.callee, false);
                }
            }
            ExprKind::Cast(expr, _) => {
                self.analyze_expr(expr, false);
            }
            ExprKind::IfElse(expr) => {
                self.analyze_expr(&expr.condition, false);
                self.analyze_block(&expr.then_branch);

                for else_branch in &expr.else_branches {
                    self.analyze_optional_expr(&else_branch.condition);
                    self.analyze_block(&else_branch.consequence);
                }
            }
            ExprKind::Let(pat, expr) => {
                self.analyze_pat(pat);
                self.analyze_expr(expr, false);
            }
            ExprKind::List(values) => {
                for value in values {
                    self.analyze_expr(value, false);
                }
            }
            ExprKind::Dict(kvs) => {
                for (_key, value) in kvs {
                    // TODO: Keys are currently not properly analyzed
                    // self.analyze_expr(key, false);
                    self.analyze_expr(value, false);
                }
            }
            ExprKind::IndexExpression(expr) => {
                self.analyze_expr(&expr.index, false);
                self.analyze_expr(&expr.base, false);
            }
            ExprKind::FieldExpression(expr) => {
                self.analyze_expr(&expr.base, false);
                // TODO: We are checking for unused variables, this should be refactored into
                //       it's own pass. Skipping analyzing field of variable as we do not have
                //       any type information yet.
                // self.analyze_node(&expr.field);
            }
            ExprKind::FunctionExpression(decl) => {
                self.analyze_fn_decl(decl);
            }
            ExprKind::Path(path) => {
                self.analyze_path(path, expr.span);
            }
            ExprKind::Match(expr) => {
                self.analyze_expr(&expr.expression, false);

                for arm in &expr.arms {
                    if let Some(symbol_table) = &self.get_symbol_table(arm.id) {
                        self.push_symbol_table(symbol_table);
                    }

                    self.analyze_pat(&arm.pattern);
                    self.analyze_expr(&arm.expression, false);

                    if let Some(symbol_table) = &self.get_symbol_table(arm.id) {
                        self.report_unused_symbols(symbol_table);
                        self.pop_symbol_table();
                    }
                }
            }
            ExprKind::Range(expr) => {
                self.analyze_expr(&expr.start, false);
                self.analyze_expr(&expr.end, false);
            }
            ExprKind::None | ExprKind::Continue | ExprKind::Literal(_) | ExprKind::Wildcard => {}
        }

        if let Some(symbol_table) = &self.get_symbol_table(expr.id) {
            self.report_unused_symbols(symbol_table);
            self.pop_symbol_table();
        }
    }

    fn analyze_pat(&mut self, pat: &Pat) {
        match &pat.kind {
            PatKind::List(patterns) => {
                for pattern in patterns {
                    self.analyze_pat(pattern);
                }
            }
            PatKind::Rest(pattern) => self.analyze_pat(pattern),
            PatKind::Enum(enum_pattern) => {
                self.analyze_path(&enum_pattern.path, pat.span);

                for (_ident, pat) in &enum_pattern.elements {
                    self.analyze_pat(pat);
                }
            }
            _ => {}
        }
    }

    fn analyze_module(&mut self, module: &Module) {
        if let Some(symbol_table) = &self.get_symbol_table(module.id) {
            self.push_symbol_table(symbol_table);
        }

        for stmt in &module.statements {
            self.analyze_stmt(stmt);
        }

        if let Some(symbol_table) = &self.get_symbol_table(module.id) {
            self.report_unused_symbols(symbol_table);
            self.pop_symbol_table();
        }
    }

    fn report_unused_symbols(&mut self, symbol_table: &Rc<RefCell<SymbolTable>>) {
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

    fn analyze_variable_declaration(&mut self, decl: &LetDeclaration) {
        self.analyze_pat(&decl.pattern);

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

        self.analyze_expr(&decl.expression, false);

        for symbol_id in &pattern_symbols {
            self.current_symbol_table()
                .borrow_mut()
                .set_declared(*symbol_id, true);
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
