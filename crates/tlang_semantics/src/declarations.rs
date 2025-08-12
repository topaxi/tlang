use std::rc::Rc;
use std::{cell::RefCell, collections::HashMap};
use tlang_ast::keyword::kw;
use tlang_ast::node::MatchExpression;
use tlang_ast::node_id::NodeId;
use tlang_ast::span::Span;
use tlang_ast::symbols::SymbolIdAllocator;
use tlang_ast::{
    node::{
        Block, Expr, ExprKind, FunctionDeclaration, FunctionParameter, LetDeclaration, Module, Pat,
        PatKind, Stmt, StmtKind,
    },
    symbols::{SymbolId, SymbolInfo, SymbolTable, SymbolType},
};

/**
 * The declaration analyzer is responsible for collecting all the declarations in a module.
 */
pub struct DeclarationAnalyzer {
    symbol_id_allocator: SymbolIdAllocator,
    symbol_tables: HashMap<NodeId, Rc<RefCell<SymbolTable>>>,
    symbol_table_stack: Vec<Rc<RefCell<SymbolTable>>>,
    symbol_type_context: Vec<SymbolType>,
}

impl Default for DeclarationAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

impl DeclarationAnalyzer {
    pub fn new() -> Self {
        DeclarationAnalyzer {
            symbol_id_allocator: Default::default(),
            symbol_tables: HashMap::new(),
            symbol_table_stack: vec![Rc::new(RefCell::new(SymbolTable::default()))],
            symbol_type_context: vec![],
        }
    }

    #[inline(always)]
    pub fn symbol_tables(&self) -> &HashMap<NodeId, Rc<RefCell<SymbolTable>>> {
        &self.symbol_tables
    }

    pub(crate) fn symbol_id_allocator(&self) -> SymbolIdAllocator {
        self.symbol_id_allocator
    }

    fn unique_id(&mut self) -> SymbolId {
        self.symbol_id_allocator.next_id()
    }

    fn root_symbol_table(&self) -> &Rc<RefCell<SymbolTable>> {
        &self.symbol_table_stack[0]
    }

    fn current_symbol_table(&self) -> &Rc<RefCell<SymbolTable>> {
        self.symbol_table_stack.last().unwrap()
    }

    fn push_symbol_table(&mut self, node_id: NodeId) -> Rc<RefCell<SymbolTable>> {
        let parent = Rc::clone(self.current_symbol_table());
        let new_symbol_table = Rc::new(RefCell::new(SymbolTable::new(parent)));
        self.symbol_tables
            .insert(node_id, Rc::clone(&new_symbol_table));
        self.symbol_table_stack.push(Rc::clone(&new_symbol_table));

        new_symbol_table
    }

    fn pop_symbol_table(&mut self) -> Rc<RefCell<SymbolTable>> {
        self.symbol_table_stack.pop().unwrap()
    }

    #[inline(always)]
    fn declare_symbol(&mut self, node_id: NodeId, name: &str, symbol_type: SymbolType, span: Span) {
        let symbol_info = SymbolInfo::new(node_id, self.unique_id(), name, symbol_type, span);
        let symbol_table = self.current_symbol_table();

        symbol_table.borrow_mut().insert(symbol_info);
    }

    pub fn add_builtin_symbols<'a, S, I>(&mut self, symbols: I)
    where
        S: AsRef<str> + 'a,
        I: IntoIterator<Item = &'a (S, SymbolType)>,
    {
        for (name, symbol_type) in symbols {
            self.root_symbol_table()
                .borrow_mut()
                .insert(SymbolInfo::new_builtin(name.as_ref(), *symbol_type));
        }
    }

    pub fn analyze(&mut self, module: &Module) {
        self.collect_module_declarations(module);

        // After collecting all declarations, we should be left with the root symbol table on the
        // symbol table stack.
        debug_assert_eq!(self.symbol_table_stack.len(), 1);
    }

    #[inline(always)]
    fn collect_optional_declarations_expr(&mut self, node: &Option<Expr>) {
        if let Some(node) = node {
            self.collect_declarations_expr(node);
        }
    }

    fn collect_declarations_stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Expr(expr) => self.collect_declarations_expr(expr),
            StmtKind::Let(decl) => self.collect_variable_declaration(decl),
            StmtKind::FunctionDeclaration(declaration) => {
                self.collect_function_declaration(declaration);
            }
            StmtKind::FunctionDeclarations(declarations) => {
                for declaration in declarations {
                    self.collect_function_declaration(declaration);
                }
            }
            StmtKind::Return(Some(expr)) => self.collect_declarations_expr(expr),
            StmtKind::Return(_) => {}
            StmtKind::EnumDeclaration(decl) => {
                self.declare_symbol(stmt.id, decl.name.as_str(), SymbolType::Enum, stmt.span);

                for element in &decl.variants {
                    self.declare_symbol(
                        element.id,
                        &(decl.name.to_string() + "::" + element.name.as_str()),
                        SymbolType::EnumVariant,
                        element.span,
                    );
                }
            }
            StmtKind::StructDeclaration(decl) => {
                self.declare_symbol(stmt.id, decl.name.as_str(), SymbolType::Struct, stmt.span);
            }
            StmtKind::None => {
                // Nothing to do here
            }
        }
    }

    fn collect_declarations_from_fn(&mut self, function_decl: &FunctionDeclaration) {
        self.symbol_type_context.push(SymbolType::Parameter);
        for param in &function_decl.parameters {
            self.collect_declarations_from_fn_param(param);
        }
        self.symbol_type_context.pop();
        self.collect_optional_declarations_expr(&function_decl.guard);
        self.collect_declarations_block(&function_decl.body);
    }

    fn collect_declarations_from_fn_param(&mut self, param: &FunctionParameter) {
        self.collect_pattern(&param.pattern);
    }

    fn collect_declarations_block(&mut self, block: &Block) {
        self.push_symbol_table(block.id);
        for stmt in &block.statements {
            self.collect_declarations_stmt(stmt);
        }

        self.collect_optional_declarations_expr(&block.expression);
        self.pop_symbol_table();
    }

    fn collect_declarations_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Block(block) | ExprKind::Loop(block) => {
                self.collect_declarations_block(block);
            }
            ExprKind::ForLoop(for_loop) => {
                self.push_symbol_table(expr.id);
                self.collect_pattern(&for_loop.pat);
                self.collect_declarations_expr(&for_loop.iter);

                if let Some((pat, expr)) = &for_loop.acc {
                    self.collect_pattern(pat);
                    self.collect_declarations_expr(expr);
                }

                self.collect_declarations_block(&for_loop.block);

                if let Some(else_block) = &for_loop.else_block {
                    self.collect_declarations_block(else_block);
                }

                self.pop_symbol_table();
            }
            ExprKind::Break(expr) => {
                if let Some(expr) = expr {
                    self.collect_declarations_expr(expr);
                }
            }
            ExprKind::FunctionExpression(decl) => {
                self.collect_function_expression(decl);
            }
            ExprKind::Call(expr) | ExprKind::RecursiveCall(expr) => {
                self.collect_declarations_expr(&expr.callee);
                for argument in &expr.arguments {
                    self.collect_declarations_expr(argument);
                }
            }
            ExprKind::Cast(expr, _) => {
                self.collect_declarations_expr(expr);
            }
            ExprKind::UnaryOp(_, node) => {
                self.collect_declarations_expr(node);
            }
            ExprKind::BinaryOp(expr) => {
                self.collect_declarations_expr(&expr.lhs);
                self.collect_declarations_expr(&expr.rhs);
            }
            ExprKind::List(values) => {
                for value in values {
                    self.collect_declarations_expr(value);
                }
            }
            ExprKind::Dict(kvs) => {
                for (key, value) in kvs {
                    self.collect_declarations_expr(key);
                    self.collect_declarations_expr(value);
                }
            }
            ExprKind::Let(pattern, expr) => {
                self.collect_declarations_expr(expr);
                self.collect_pattern(pattern);
            }
            ExprKind::IfElse(expr) => {
                self.collect_declarations_expr(&expr.condition);
                self.collect_declarations_block(&expr.then_branch);

                for else_branch in &expr.else_branches {
                    self.collect_optional_declarations_expr(&else_branch.condition);
                    self.collect_declarations_block(&else_branch.consequence);
                }
            }
            ExprKind::FieldExpression(expr) => {
                self.collect_declarations_expr(&expr.base);
            }
            ExprKind::IndexExpression(expr) => {
                self.collect_declarations_expr(&expr.base);
                self.collect_declarations_expr(&expr.index);
            }
            ExprKind::Match(expr) => {
                self.collect_match_expr(expr);
            }
            ExprKind::Range(expr) => {
                self.collect_declarations_expr(&expr.start);
                self.collect_declarations_expr(&expr.end);
            }
            ExprKind::Path(_)
            | ExprKind::Literal(_)
            | ExprKind::Wildcard
            | ExprKind::Continue
            | ExprKind::None => {
                // Nothing to do here
            }
        }
    }

    fn collect_match_expr(&mut self, match_expr: &MatchExpression) {
        self.collect_declarations_expr(&match_expr.expression);

        for arm in &match_expr.arms {
            self.push_symbol_table(arm.id);
            self.collect_pattern(&arm.pattern);
            self.collect_optional_declarations_expr(&arm.guard);
            self.collect_declarations_expr(&arm.expression);
            self.pop_symbol_table();
        }
    }

    fn collect_module_declarations(&mut self, module: &Module) {
        self.push_symbol_table(module.id);

        for stmt in &module.statements {
            self.collect_declarations_stmt(stmt);
        }

        self.pop_symbol_table();
    }

    fn collect_variable_declaration(&mut self, decl: &LetDeclaration) {
        self.collect_declarations_expr(&decl.expression);
        self.collect_pattern(&decl.pattern);
    }

    /// TODO: This is a temporary solution. We need to find a better way to handle this.
    #[allow(clippy::only_used_in_recursion)]
    fn fn_identifier_to_string(&self, identifier: &Expr) -> String {
        match identifier.kind {
            ExprKind::Path(ref path) => path.to_string(),
            ExprKind::FieldExpression(ref expr) => {
                let base_name = self.fn_identifier_to_string(&expr.base);

                format!("{base_name}.{}", expr.field)
            }
            _ => panic!("Expected identifier, found {:?}", identifier.kind),
        }
    }

    fn collect_function_declaration(&mut self, declaration: &FunctionDeclaration) {
        let name_as_str = self.fn_identifier_to_string(&declaration.name);

        self.declare_symbol(
            declaration.id,
            &name_as_str,
            SymbolType::Function(declaration.parameters.len() as u16),
            declaration.name.span,
        );

        self.push_symbol_table(declaration.id);

        self.symbol_type_context.push(SymbolType::Parameter);
        for param in &declaration.parameters {
            self.collect_declarations_from_fn_param(param);
        }
        self.symbol_type_context.pop();

        self.collect_optional_declarations_expr(&declaration.guard);
        self.collect_declarations_block(&declaration.body);

        self.pop_symbol_table();
    }

    fn collect_function_expression(&mut self, decl: &FunctionDeclaration) {
        self.push_symbol_table(decl.id);
        let name_as_str = self.fn_identifier_to_string(&decl.name);

        if name_as_str != "anonymous" {
            self.declare_symbol(
                decl.id,
                &name_as_str,
                SymbolType::Function(decl.parameters.len() as u16),
                decl.name.span,
            );
        }

        self.collect_declarations_from_fn(decl);
        self.pop_symbol_table();
    }

    fn collect_pattern(&mut self, pattern: &Pat) {
        match &pattern.kind {
            PatKind::Identifier(ident) => {
                self.declare_symbol(
                    pattern.id,
                    ident.as_str(),
                    *self
                        .symbol_type_context
                        .last()
                        .unwrap_or(&SymbolType::Variable),
                    pattern.span,
                );
            }
            PatKind::_Self => {
                self.declare_symbol(pattern.id, kw::_Self, SymbolType::Variable, pattern.span);
            }
            PatKind::List(patterns) => {
                for pattern in patterns {
                    self.collect_pattern(pattern);
                }
            }
            PatKind::Rest(pattern) => {
                self.collect_pattern(pattern);
            }
            PatKind::Enum(enum_pattern) => {
                for (_ident, pat) in &enum_pattern.elements {
                    self.collect_pattern(pat);
                }
            }
            PatKind::Wildcard => {} // Wildcard discards values, nothing to do here.
            PatKind::Literal(_) => {} // Literal patterns don't need to be declared.
            _ => {}
        }
    }
}
