use std::{cell::RefCell, collections::HashMap, rc::Rc};

use tlang_ast::{
    node::{
        Block, Expr, ExprKind, FunctionDeclaration, FunctionParameter, LetDeclaration, Module,
        Path, Pattern, PatternKind, Stmt, StmtKind, StructDeclaration,
    },
    node_id::NodeId,
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
    fn symbol_tables(&self) -> &HashMap<NodeId, Rc<RefCell<SymbolTable>>> {
        self.declaration_analyzer.get_symbol_tables()
    }

    #[inline(always)]
    pub fn get_symbol_table(&self, id: NodeId) -> Option<Rc<RefCell<SymbolTable>>> {
        self.symbol_tables().get(&id).cloned()
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
        let symbol_info = self.get_last_symbol_table().borrow().get_by_name(name);

        if let Some(symbol_info) = symbol_info {
            // TODO: This only marks the by it's name, not by it's id. Maybe we should
            //       mark it in the collection pass? Or keep track of the current id in
            //       case the name is being shadowed?
            self.get_last_symbol_table()
                .borrow_mut()
                .mark_as_used(symbol_info.id);
        } else {
            self.report_undeclared_variable(name, span);
        }
    }

    fn report_undeclared_variable(&mut self, name: &str, span: Span) {
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
                span,
            ));
        } else {
            self.diagnostics.push(Diagnostic::new(
                format!("Use of undeclared variable `{name}`"),
                Severity::Error,
                span,
            ));
        }
    }

    #[inline(always)]
    fn analyze_optional_expr(&mut self, expr: &Option<Expr>) {
        if let Some(expr) = expr {
            self.analyze_expr(expr);
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

        if let Some(ref guard) = decl.guard {
            self.analyze_expr(guard);
        }

        self.analyze_block(&decl.body);

        if let Some(symbol_table) = &self.get_symbol_table(decl.id) {
            self.report_unused_symbols(symbol_table);
            self.pop_symbol_table();
        }
    }

    fn analyze_path(&mut self, path: &Path, span: Span) {
        let mut segments = Vec::with_capacity(path.segments.len());
        for segment in &path.segments {
            segments.push(segment.to_string());
            self.mark_as_used_by_name(&segments.join("::"), span);
        }
    }

    fn analyze_expr(&mut self, expr: &Expr) {
        if let Some(symbol_table) = &self.get_symbol_table(expr.id) {
            self.push_symbol_table(symbol_table);
        }

        match &expr.kind {
            ExprKind::BinaryOp(expr) => {
                self.analyze_expr(&expr.lhs);
                self.analyze_expr(&expr.rhs);
            }
            ExprKind::Block(block) => self.analyze_block(block),
            ExprKind::UnaryOp(_, node) => {
                self.analyze_expr(node);
            }
            ExprKind::Call(expr) | ExprKind::RecursiveCall(expr) => {
                for argument in &expr.arguments {
                    self.analyze_expr(argument);
                }

                self.analyze_expr(&expr.callee);
            }
            ExprKind::IfElse(expr) => {
                self.analyze_expr(&expr.condition);
                self.analyze_expr(&expr.then_branch);

                for else_branch in &expr.else_branches {
                    self.analyze_optional_expr(&else_branch.condition);
                    self.analyze_expr(&else_branch.consequence);
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
                self.analyze_expr(&expr.index);
                self.analyze_expr(&expr.base);
            }
            ExprKind::FieldExpression(expr) => {
                self.analyze_expr(&expr.base);
                // TODO: We are checking for unused variables, this should be refactored into
                //       it's own pass. Skipping analyzing field of variable as we do not have
                //       any type information yet.
                // self.analyze_node(&expr.field);
            }
            ExprKind::FunctionExpression(decl) => {
                self.analyze_fn_decl(decl);
            }
            ExprKind::Path(path) => {
                let mut segments = Vec::with_capacity(path.segments.len());

                for segment in &path.segments {
                    segments.push(segment.to_string());

                    self.mark_as_used_by_name(&segments.join("::"), expr.span);
                }
            }
            ExprKind::Match(expr) => {
                self.analyze_expr(&expr.expression);

                for arm in &expr.arms {
                    self.analyze_pat(&arm.pattern);
                    self.analyze_expr(&arm.expression);
                }
            }
            ExprKind::Range(expr) => {
                self.analyze_expr(&expr.start);
                self.analyze_expr(&expr.end);
            }
            ExprKind::None | ExprKind::Literal(_) | ExprKind::Wildcard => {}
        }

        if let Some(symbol_table) = &self.get_symbol_table(expr.id) {
            self.report_unused_symbols(symbol_table);
            self.pop_symbol_table();
        }
    }

    fn analyze_pat(&mut self, pat: &Pattern) {
        match &pat.kind {
            PatternKind::List(patterns) => {
                for pattern in patterns {
                    self.analyze_pat(pattern);
                }
            }
            PatternKind::Rest(pattern) => self.analyze_pat(pattern),
            PatternKind::Enum(enum_pattern) => {
                self.analyze_path(&enum_pattern.path, pat.span);

                for element in &enum_pattern.elements {
                    self.analyze_pat(element);
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
            .get_all_local_symbols()
            .iter()
            // Internal symbols are currently represented with id 0 and internal symbols are not
            // reported as unused.
            .filter(|symbol| symbol.id != SymbolId::new(0))
            .filter(|symbol| !symbol.used)
            .filter(|symbol| !symbol.name.starts_with('_'))
            // TODO: We currently do not track member methods, as we do not have any type
            //       information yet.
            .filter(|symbol| !symbol.name.contains('.'))
            .collect::<Vec<_>>();

        for unused_symbol in &unused_symbols {
            self.diagnostics.push(Diagnostic::new(
                    format!(
                        "Unused {} `{}`, if this is intentional, prefix the name with an underscore: `_{}`",
                        unused_symbol.symbol_type, unused_symbol.name, unused_symbol.name
                    ),
                    Severity::Warning,
                    unused_symbol.defined_at
                ));
        }
    }

    fn analyze_variable_declaration(&mut self, decl: &LetDeclaration) {
        self.analyze_pat(&decl.pattern);

        // When declaring a variable, we can only reference symbols that were declared before.
        // This includes our own variable name.
        // E.g. `let a = a;` is not allowed. But `let a = 1; let a = a;` is.
        // By removing the symbol from the table while analyzing the expression, we can check
        // whether the expression references any symbols that were not declared before.
        let pattern_symbols = decl
            .pattern
            .get_all_node_ids()
            .iter()
            .filter_map(|id| {
                self.get_last_symbol_table()
                    .borrow()
                    .get_local_by_node_id(*id)
                    .map(|symbol_info| symbol_info.id)
            })
            .filter_map(|id| self.get_last_symbol_table().borrow_mut().remove(id))
            .collect::<Vec<_>>();

        self.analyze_expr(&decl.expression);

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
