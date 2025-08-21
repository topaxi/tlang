use log::debug;
use std::rc::Rc;
use std::{cell::RefCell, collections::HashMap};
use tlang_ast::NodeId;
use tlang_ast::keyword::kw;
use tlang_ast::node::MatchExpression;
use tlang_ast::symbols::SymbolIdAllocator;
use tlang_ast::{
    node::{
        Block, Expr, ExprKind, FunctionDeclaration, FunctionParameter, Module, Pat, PatKind, Stmt,
        StmtKind,
    },
    symbols::{SymbolId, SymbolInfo, SymbolTable, SymbolType},
};
use tlang_span::{LineColumn, Span};

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
        let root_symbol_table = Rc::new(RefCell::new(SymbolTable::default()));

        DeclarationAnalyzer {
            symbol_id_allocator: SymbolIdAllocator::default(),
            symbol_tables: HashMap::from([(NodeId::new(1), root_symbol_table.clone())]),
            symbol_table_stack: vec![root_symbol_table],
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

    pub fn root_symbol_table(&self) -> &Rc<RefCell<SymbolTable>> {
        &self.symbol_table_stack[0]
    }

    fn current_symbol_table(&self) -> &Rc<RefCell<SymbolTable>> {
        self.symbol_table_stack.last().unwrap()
    }

    fn push_symbol_table(&mut self, node_id: NodeId) -> Rc<RefCell<SymbolTable>> {
        debug!("Entering new scope for node: {}", node_id);

        let parent = self.current_symbol_table().clone();
        let new_symbol_table = Rc::new(RefCell::new(SymbolTable::new(parent)));
        self.symbol_tables.insert(node_id, new_symbol_table.clone());
        self.symbol_table_stack.push(new_symbol_table.clone());

        new_symbol_table
    }

    fn pop_symbol_table(&mut self) -> Rc<RefCell<SymbolTable>> {
        debug!("Leaving scope");

        self.symbol_table_stack.pop().unwrap()
    }

    #[inline(always)]
    fn declare_symbol(
        &mut self,
        node_id: NodeId,
        name: &str,
        symbol_type: SymbolType,
        defined_at: Span,
        scope_start: LineColumn,
    ) {
        let id = self.unique_id();
        let symbol_info =
            SymbolInfo::new(id, name, symbol_type, defined_at, scope_start).with_node_id(node_id);

        debug!("Declaring symbol: {:#?}", symbol_info);

        self.current_symbol_table().borrow_mut().insert(symbol_info);
    }

    pub fn add_builtin_symbols<'a, S, I>(&mut self, symbols: I)
    where
        S: AsRef<str> + 'a,
        I: IntoIterator<Item = &'a (S, SymbolType)>,
    {
        for (name, symbol_type) in symbols {
            let symbol_info =
                SymbolInfo::new_builtin(self.unique_id(), name.as_ref(), *symbol_type);

            self.root_symbol_table().borrow_mut().insert(symbol_info);
        }
    }

    pub fn analyze(&mut self, module: &Module, is_root: bool) {
        self.collect_module_declarations(module, is_root);

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
            StmtKind::Let(decl) => {
                self.collect_declarations_expr(&decl.expression);
                self.collect_pattern(&decl.pattern, stmt.span.end);
            }
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
                self.declare_symbol(
                    stmt.id,
                    decl.name.as_str(),
                    SymbolType::Enum,
                    stmt.span,
                    stmt.span.end,
                );

                for element in &decl.variants {
                    self.declare_symbol(
                        element.id,
                        &(decl.name.to_string() + "::" + element.name.as_str()),
                        SymbolType::EnumVariant,
                        element.span,
                        element.span.end,
                    );
                }
            }
            StmtKind::StructDeclaration(decl) => {
                self.declare_symbol(
                    stmt.id,
                    decl.name.as_str(),
                    SymbolType::Struct,
                    stmt.span,
                    stmt.span.end,
                );
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
        self.collect_declarations_block_scopeless(&function_decl.body);
    }

    fn collect_declarations_from_fn_param(&mut self, param: &FunctionParameter) {
        self.collect_pattern(&param.pattern, param.span.end);
    }

    fn collect_declarations_block(&mut self, block: &Block) {
        self.push_symbol_table(block.id);
        self.collect_declarations_block_scopeless(block);
        self.pop_symbol_table();
    }

    fn collect_declarations_block_scopeless(&mut self, block: &Block) {
        for stmt in &block.statements {
            self.collect_declarations_stmt(stmt);
        }

        self.collect_optional_declarations_expr(&block.expression);
    }

    fn collect_declarations_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Block(block) | ExprKind::Loop(block) => {
                self.collect_declarations_block(block);
            }
            ExprKind::ForLoop(for_loop) => {
                self.push_symbol_table(expr.id);
                self.collect_pattern(&for_loop.pat, for_loop.pat.span.end);
                self.collect_declarations_expr(&for_loop.iter);

                if let Some((pat, expr)) = &for_loop.acc {
                    self.collect_pattern(pat, expr.span.end);
                    self.collect_declarations_expr(expr);
                }

                self.collect_declarations_block(&for_loop.block);
                self.pop_symbol_table();

                if let Some(else_block) = &for_loop.else_block {
                    self.collect_declarations_block(else_block);
                }
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
                for argument in &expr.arguments {
                    self.collect_declarations_expr(argument);
                }

                self.collect_declarations_expr(&expr.callee);
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
                self.collect_pattern(pattern, expr.span.end);
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
            self.collect_pattern(&arm.pattern, arm.pattern.span.end);
            self.collect_optional_declarations_expr(&arm.guard);

            match &arm.expression.kind {
                ExprKind::Block(block) => self.collect_declarations_block_scopeless(block),
                _ => self.collect_declarations_expr(&arm.expression),
            }

            self.pop_symbol_table();
        }
    }

    fn collect_module_declarations(&mut self, module: &Module, is_root: bool) {
        if !is_root {
            self.push_symbol_table(module.id);
        }

        for stmt in &module.statements {
            self.collect_declarations_stmt(stmt);
        }

        if !is_root {
            self.pop_symbol_table();
        }
    }

    fn collect_function_declaration(&mut self, declaration: &FunctionDeclaration) {
        let name_as_str = declaration.name();

        self.declare_symbol(
            declaration.id,
            &name_as_str,
            SymbolType::Function(declaration.parameters.len() as u16),
            declaration.name.span,
            declaration.span.end,
        );

        self.push_symbol_table(declaration.id);

        // The function name is also declared and bound within the function itself.
        // Similar to what JS does.
        self.declare_symbol(
            declaration.id,
            &name_as_str,
            SymbolType::FunctionSelfRef(declaration.parameters.len() as u16),
            declaration.name.span,
            declaration.name.span.end,
        );

        self.symbol_type_context.push(SymbolType::Parameter);
        for param in &declaration.parameters {
            self.collect_declarations_from_fn_param(param);
        }
        self.symbol_type_context.pop();

        self.collect_optional_declarations_expr(&declaration.guard);
        self.collect_declarations_block_scopeless(&declaration.body);

        self.pop_symbol_table();
    }

    fn collect_function_expression(&mut self, decl: &FunctionDeclaration) {
        self.push_symbol_table(decl.id);
        let name_as_str = decl.name();

        self.declare_symbol(
            decl.id,
            &name_as_str,
            SymbolType::FunctionSelfRef(decl.parameters.len() as u16),
            decl.name.span,
            decl.name.span.end,
        );

        self.collect_declarations_from_fn(decl);
        self.pop_symbol_table();
    }

    fn collect_pattern(&mut self, pattern: &Pat, scope_start: LineColumn) {
        match &pattern.kind {
            PatKind::Identifier(ident) => {
                self.declare_symbol(
                    pattern.id,
                    ident.as_str(),
                    self.symbol_type_context
                        .last()
                        .copied()
                        .unwrap_or(SymbolType::Variable),
                    pattern.span,
                    scope_start,
                );
            }
            PatKind::_Self => {
                self.declare_symbol(
                    pattern.id,
                    kw::_Self,
                    SymbolType::Variable,
                    pattern.span,
                    scope_start,
                );
            }
            PatKind::List(patterns) => {
                for pattern in patterns {
                    self.collect_pattern(pattern, scope_start);
                }
            }
            PatKind::Rest(pattern) => {
                self.collect_pattern(pattern, scope_start);
            }
            PatKind::Enum(enum_pattern) => {
                for (_ident, pat) in &enum_pattern.elements {
                    self.collect_pattern(pat, scope_start);
                }
            }
            PatKind::Wildcard => {} // Wildcard discards values, nothing to do here.
            PatKind::Literal(_) => {} // Literal patterns don't need to be declared.
            _ => {}
        }
    }
}
