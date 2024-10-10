use std::{cell::RefCell, rc::Rc};

use tlang_ast::{
    node::{
        Block, Expr, ExprKind, FunctionDeclaration, FunctionParameter, LetDeclaration, Module,
        Pattern, PatternKind, Stmt, StmtKind,
    },
    span::Span,
    symbols::{SymbolId, SymbolInfo, SymbolTable, SymbolType},
};

use crate::{
    declarations::DeclarationAnalyzer,
    diagnostic::{Diagnostic, Severity},
};

pub struct SemanticAnalyzer {
    declaration_analyzer: DeclarationAnalyzer,
    symbol_table_stack: Vec<Rc<RefCell<SymbolTable>>>,
    diagnostics: Vec<Diagnostic>,
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
        }
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

    fn get_last_symbol_table(&self) -> Rc<RefCell<SymbolTable>> {
        Rc::clone(self.symbol_table_stack.last().unwrap())
    }

    fn push_symbol_table(&mut self, symbol_table: &Rc<RefCell<SymbolTable>>) {
        self.symbol_table_stack.push(Rc::clone(symbol_table));
    }

    fn pop_symbol_table(&mut self) -> Rc<RefCell<SymbolTable>> {
        self.symbol_table_stack.pop().unwrap()
    }

    pub fn add_builtin_symbols(&mut self, symbols: &[(&str, SymbolType)]) {
        self.declaration_analyzer.add_builtin_symbols(symbols);
    }

    pub fn analyze(&mut self, module: &mut Module) -> Result<(), Vec<Diagnostic>> {
        self.collect_declarations(module);
        // self.collect_initializations(ast);
        self.analyze_module(module);

        if self.get_errors().is_empty() {
            Ok(())
        } else {
            Err(self.get_errors())
        }
    }

    fn collect_declarations(&mut self, module: &mut Module) {
        self.declaration_analyzer.analyze(module);
    }

    fn mark_as_used_by_name(&mut self, name: &str, span: &Span) {
        let symbol_info = self.get_last_symbol_table().borrow().get_by_name(name);

        if let Some(symbol_info) = symbol_info {
            let symbol_table = self.get_last_symbol_table();
            // TODO: This only marks the by it's name, not by it's id. Maybe we should
            //       mark it in the collection pass? Or keep track of the current id in
            //       case the name is being shadowed?
            symbol_table.borrow_mut().mark_as_used(symbol_info.id);
        } else {
            let did_you_mean = did_you_mean(
                name,
                &self.get_last_symbol_table().borrow().get_all_symbols(),
            );

            if let Some(suggestion) = did_you_mean {
                self.diagnostics.push(Diagnostic::new(
                    format!(
                        "Use of undeclared variable `{}`, did you mean the {} `{}`",
                        name, suggestion.symbol_type, suggestion.name
                    ),
                    Severity::Error,
                    *span,
                ));
            } else {
                self.diagnostics.push(Diagnostic::new(
                    format!("Use of undeclared variable `{name}`"),
                    Severity::Error,
                    *span,
                ));
            }
        }
    }

    #[inline(always)]
    fn analyze_optional_expr(&mut self, expr: &mut Option<Expr>) {
        if let Some(expr) = expr {
            self.analyze_expr(expr);
        }
    }

    fn analyze_block(&mut self, block: &mut Block) {
        if let Some(symbol_table) = &block.symbol_table {
            self.push_symbol_table(symbol_table);
        }

        for stmt in &mut block.statements {
            self.analyze_stmt(stmt);
        }

        self.analyze_optional_expr(&mut block.expression);

        if let Some(symbol_table) = &block.symbol_table {
            self.report_unused_symbols(symbol_table, &block.span);
            self.pop_symbol_table();
        }
    }

    fn analyze_stmt(&mut self, stmt: &mut Stmt) {
        if let Some(symbol_table) = &stmt.symbol_table {
            self.push_symbol_table(symbol_table);
        }

        match &mut stmt.kind {
            StmtKind::None => {}
            StmtKind::Expr(expr) => self.analyze_expr(expr),
            StmtKind::Let(decl) => self.analyze_variable_declaration(decl),
            StmtKind::FunctionDeclaration(decl) => self.analyze_fn_decl(decl),
            StmtKind::FunctionDeclarations(decls) => {
                for decl in decls {
                    self.analyze_fn_decl(decl);
                }
            }
            StmtKind::Return(expr) => self.analyze_optional_expr(expr),
            StmtKind::EnumDeclaration(_decl) => {
                // TODO
            }
            StmtKind::StructDeclaration(_decl) => {
                // TODO
            }
        }

        if let Some(symbol_table) = &stmt.symbol_table {
            self.report_unused_symbols(symbol_table, &stmt.span);
            self.pop_symbol_table();
        }
    }

    fn analyze_fn_param(&mut self, param: &mut FunctionParameter) {
        self.analyze_pat(&mut param.pattern);
    }

    fn analyze_fn_decl(&mut self, decl: &mut FunctionDeclaration) {
        if let Some(symbol_table) = &decl.symbol_table {
            self.push_symbol_table(symbol_table);
        }

        for parameter in &mut decl.parameters {
            self.analyze_fn_param(parameter);
        }

        if let Some(ref mut guard) = decl.guard {
            self.analyze_expr(guard);
        }

        self.analyze_block(&mut decl.body);

        if let Some(symbol_table) = &decl.symbol_table {
            self.report_unused_symbols(symbol_table, &decl.span);
            self.pop_symbol_table();
        }
    }

    fn analyze_expr(&mut self, expr: &mut Expr) {
        if let Some(symbol_table) = &expr.symbol_table {
            self.push_symbol_table(symbol_table);
        }

        let mut kind = std::mem::take(&mut expr.kind);

        match &mut kind {
            ExprKind::BinaryOp(expr) => {
                self.analyze_expr(&mut expr.lhs);
                self.analyze_expr(&mut expr.rhs);
            }
            ExprKind::Block(block) => self.analyze_block(block),
            ExprKind::UnaryOp(_, node) => {
                self.analyze_expr(node);
            }
            ExprKind::Call(expr) | ExprKind::RecursiveCall(expr) => {
                self.analyze_expr(&mut expr.callee);

                for argument in &mut expr.arguments {
                    self.analyze_expr(argument);
                }
            }
            ExprKind::IfElse(expr) => {
                self.analyze_expr(&mut expr.condition);
                self.analyze_expr(&mut expr.then_branch);

                for else_branch in &mut expr.else_branches {
                    self.analyze_optional_expr(&mut else_branch.condition);
                    self.analyze_expr(&mut else_branch.consequence);
                }
            }
            ExprKind::Let(pat, expr) => {
                self.analyze_pat(pat);
                self.analyze_expr(expr);
            }
            ExprKind::List(values) => {
                for value in values {
                    self.analyze_expr(value);
                }
            }
            ExprKind::Dict(kvs) => {
                for (_key, value) in kvs {
                    // TODO: Keys are currently not properly analyzed
                    // self.analyze_expr(key);
                    self.analyze_expr(value);
                }
            }
            ExprKind::IndexExpression(expr) => {
                self.analyze_expr(&mut expr.base);
                self.analyze_expr(&mut expr.index);
            }
            ExprKind::FieldExpression(expr) => {
                self.analyze_expr(&mut expr.base);
                // TODO: We are checking for unused variables, this should be refactored into
                //       it's own pass. Skipping analyzing field of variable as we do not have
                //       any type information yet.
                // self.analyze_node(&mut expr.field);
            }
            ExprKind::FunctionExpression(decl) => {
                self.analyze_fn_decl(decl);
            }
            ExprKind::Path(path) => {
                if let Some(first_ident) = path.segments.first() {
                    self.mark_as_used_by_name(first_ident.as_str(), &expr.span);
                }

                // TODO: Handle nested identifiers
            }
            ExprKind::Match(expr) => {
                self.analyze_expr(&mut expr.expression);

                for arm in &mut expr.arms {
                    self.analyze_pat(&mut arm.pattern);
                    self.analyze_expr(&mut arm.expression);
                }
            }
            ExprKind::Range(expr) => {
                self.analyze_expr(&mut expr.start);
                self.analyze_expr(&mut expr.end);
            }
            ExprKind::None | ExprKind::Literal(_) | ExprKind::Wildcard => {}
        }

        expr.kind = kind;

        if let Some(symbol_table) = &expr.symbol_table {
            self.report_unused_symbols(symbol_table, &expr.span);
            self.pop_symbol_table();
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn analyze_pat(&self, pat: &mut Pattern) {
        match &mut pat.kind {
            PatternKind::List(patterns) => {
                for pattern in patterns {
                    self.analyze_pat(pattern);
                }
            }
            PatternKind::Rest(pattern) => self.analyze_pat(pattern),
            PatternKind::Enum(enum_pattern) => {
                // self.mark_as_used_by_name(enum_pattern.identifier.as_str(), &pat.span);

                for element in &mut enum_pattern.elements {
                    self.analyze_pat(element);
                }
            }
            PatternKind::Wildcard | PatternKind::Identifier { .. } | PatternKind::Literal(_) => {}
        }
    }

    fn analyze_module(&mut self, module: &mut Module) {
        if let Some(symbol_table) = &module.symbol_table {
            self.push_symbol_table(symbol_table);
        }

        for stmt in &mut module.statements {
            self.analyze_stmt(stmt);
        }

        if let Some(symbol_table) = &module.symbol_table {
            self.report_unused_symbols(symbol_table, &module.span);
            self.pop_symbol_table();
        }
    }

    fn report_unused_symbols(&mut self, symbol_table: &Rc<RefCell<SymbolTable>>, span: &Span) {
        let symbol_table = symbol_table.borrow();
        let local_symbols = symbol_table.get_all_local_symbols();
        let mut unused_symbols = local_symbols
            .iter()
            .filter(|symbol| symbol.id != SymbolId::new(0))
            .filter(|symbol| !symbol.used)
            .filter(|symbol| !symbol.name.starts_with('_'))
            .collect::<Vec<_>>();

        for unused_symbol in &mut unused_symbols {
            self.diagnostics.push(Diagnostic::new(
                    format!(
                        "Unused {} `{}`, if this is intentional, prefix the name with an underscore: `_{}`",
                        unused_symbol.symbol_type, unused_symbol.name, unused_symbol.name
                    ),
                    Severity::Warning,
                    unused_symbol.defined_at.unwrap_or(*span)
                ));
        }
    }

    fn analyze_variable_declaration(&mut self, decl: &mut LetDeclaration) {
        self.analyze_pat(&mut decl.pattern);

        // When declaring a variable, we can only reference symbols that were declared before.
        // This includes our own variable name.
        // E.g. `let a = a;` is not allowed. But `let a = 1; let a = a;` is.
        // By removing the symbol from the table while analyzing the expression, we can check
        // whether the expression references any symbols that were not declared before.
        let pattern_symbols = decl
            .pattern
            .get_all_symbol_ids()
            .iter()
            .filter_map(|id| self.get_last_symbol_table().borrow_mut().remove(*id))
            .collect::<Vec<_>>();

        self.analyze_expr(&mut decl.expression);

        for symbol in pattern_symbols {
            self.get_last_symbol_table()
                .borrow_mut()
                .insert_beginning(symbol);
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
