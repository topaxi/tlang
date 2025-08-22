use log::debug;
use std::rc::Rc;
use std::{cell::RefCell, collections::HashMap};
use tlang_ast::keyword::kw;
use tlang_ast::symbols::SymbolIdAllocator;
use tlang_ast::visit::{Visitor, walk_stmt, walk_expr};
use tlang_ast::{
    node::{
        Expr, ExprKind, FunctionDeclaration, FunctionParameter, Module, Pat, PatKind, Stmt,
        StmtKind,
    },
    symbols::{SymbolId, SymbolInfo, SymbolTable, SymbolType},
};
use tlang_span::{LineColumn, NodeId, Span};

/**
 * Context for declaration analysis, containing all the state needed
 * during the visitor traversal.
 */
pub struct DeclarationContext {
    symbol_id_allocator: SymbolIdAllocator,
    symbol_tables: HashMap<NodeId, Rc<RefCell<SymbolTable>>>,
    root_symbol_table: Rc<RefCell<SymbolTable>>,
}

impl DeclarationContext {
    pub fn new() -> Self {
        let root_symbol_table = Rc::new(RefCell::new(SymbolTable::default()));
        DeclarationContext {
            symbol_id_allocator: SymbolIdAllocator::default(),
            symbol_tables: HashMap::from([(NodeId::new(1), root_symbol_table.clone())]),
            root_symbol_table,
        }
    }

    pub fn symbol_tables(&self) -> &HashMap<NodeId, Rc<RefCell<SymbolTable>>> {
        &self.symbol_tables
    }

    pub fn symbol_id_allocator(&self) -> SymbolIdAllocator {
        self.symbol_id_allocator
    }

    pub fn root_symbol_table(&self) -> &Rc<RefCell<SymbolTable>> {
        &self.root_symbol_table
    }

    fn unique_id(&mut self) -> SymbolId {
        self.symbol_id_allocator.next_id()
    }

    pub fn add_builtin_symbols<'a, S, I>(&mut self, symbols: I)
    where
        S: AsRef<str> + 'a,
        I: IntoIterator<Item = &'a (S, SymbolType)>,
    {
        for (name, symbol_type) in symbols {
            let symbol_info =
                SymbolInfo::new_builtin(self.unique_id(), name.as_ref(), *symbol_type);

            self.root_symbol_table.borrow_mut().insert(symbol_info);
        }
    }
}

/**
 * The declaration analyzer is responsible for collecting all the declarations in a module.
 */
pub struct DeclarationAnalyzer {
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
            symbol_table_stack: vec![],
            symbol_type_context: vec![],
        }
    }

    pub fn analyze(&mut self, module: &Module, is_root: bool) -> DeclarationContext {
        let mut ctx = DeclarationContext::new();
        self.analyze_with_context(module, is_root, &mut ctx);
        ctx
    }

    pub fn analyze_with_context(&mut self, module: &Module, is_root: bool, ctx: &mut DeclarationContext) {
        // Initialize symbol table stack with root table
        self.symbol_table_stack = vec![ctx.root_symbol_table().clone()];

        if !is_root {
            self.push_symbol_table(module.id, ctx);
        }

        self.visit_module(module, ctx);

        if !is_root {
            self.pop_symbol_table();
        }

        // After collecting all declarations, we should be left with the root symbol table on the
        // symbol table stack.
        debug_assert_eq!(self.symbol_table_stack.len(), 1);
    }

    fn current_symbol_table(&self) -> &Rc<RefCell<SymbolTable>> {
        self.symbol_table_stack.last().unwrap()
    }

    fn push_symbol_table(&mut self, node_id: NodeId, ctx: &mut DeclarationContext) -> Rc<RefCell<SymbolTable>> {
        debug!("Entering new scope for node: {}", node_id);

        let parent = self.current_symbol_table().clone();
        let new_symbol_table = Rc::new(RefCell::new(SymbolTable::new(parent)));
        ctx.symbol_tables.insert(node_id, new_symbol_table.clone());
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
        ctx: &mut DeclarationContext,
        node_id: NodeId,
        name: &str,
        symbol_type: SymbolType,
        defined_at: Span,
        scope_start: LineColumn,
    ) {
        let id = ctx.unique_id();
        let symbol_info =
            SymbolInfo::new(id, name, symbol_type, defined_at, scope_start).with_node_id(node_id);

        debug!("Declaring symbol: {:#?}", symbol_info);

        self.current_symbol_table().borrow_mut().insert(symbol_info);
    }
}

impl<'ast> Visitor<'ast> for DeclarationAnalyzer {
    type Context = DeclarationContext;

    fn enter_scope(&mut self, node_id: NodeId, ctx: &mut Self::Context) {
        self.push_symbol_table(node_id, ctx);
    }

    fn leave_scope(&mut self, _node_id: NodeId, _ctx: &mut Self::Context) {
        self.pop_symbol_table();
    }

    fn visit_module(&mut self, module: &'ast Module, ctx: &mut Self::Context) {
        // Don't use walk_module as it includes scope management that conflicts
        // with our explicit scope management in analyze_with_context
        for statement in &module.statements {
            self.visit_stmt(statement, ctx);
        }
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt, ctx: &mut Self::Context) {
        match &stmt.kind {
            StmtKind::FunctionDeclaration(decl) => {
                self.visit_fn_decl(decl, ctx);
            }
            StmtKind::FunctionDeclarations(decls) => {
                for decl in decls {
                    self.visit_fn_decl(decl, ctx);
                }
            }
            StmtKind::EnumDeclaration(decl) => {
                self.declare_symbol(
                    ctx,
                    stmt.id,
                    decl.name.as_str(),
                    SymbolType::Enum,
                    stmt.span,
                    stmt.span.end,
                );

                for element in &decl.variants {
                    self.declare_symbol(
                        ctx,
                        element.id,
                        &(decl.name.to_string() + "::" + element.name.as_str()),
                        SymbolType::EnumVariant(element.parameters.len() as u16),
                        element.span,
                        element.span.end,
                    );
                }
            }
            StmtKind::StructDeclaration(decl) => {
                self.declare_symbol(
                    ctx,
                    stmt.id,
                    decl.name.as_str(),
                    SymbolType::Struct,
                    stmt.span,
                    stmt.span.end,
                );
            }
            StmtKind::Let(decl) => {
                self.visit_expr(&decl.expression, ctx);
                self.collect_pattern(&decl.pattern, stmt.span.end, ctx);
                return; // Don't walk the statement again
            }
            _ => {}
        }
        
        // For other statement types, use the default walker
        if !matches!(&stmt.kind, 
                    StmtKind::FunctionDeclaration(_) | 
                    StmtKind::FunctionDeclarations(_) | 
                    StmtKind::EnumDeclaration(_) | 
                    StmtKind::StructDeclaration(_) | 
                    StmtKind::Let(_)) {
            walk_stmt(self, stmt, ctx);
        }
    }

    fn visit_fn_decl(&mut self, declaration: &'ast FunctionDeclaration, ctx: &mut Self::Context) {
        let name_as_str = declaration.name();

        self.declare_symbol(
            ctx,
            declaration.id,
            &name_as_str,
            SymbolType::Function(declaration.parameters.len() as u16),
            declaration.name.span,
            declaration.span.end,
        );

        // Enter function scope
        self.enter_scope(declaration.id, ctx);

        // The function name is also declared and bound within the function itself.
        // Similar to what JS does.
        self.declare_symbol(
            ctx,
            declaration.id,
            &name_as_str,
            SymbolType::FunctionSelfRef(declaration.parameters.len() as u16),
            declaration.name.span,
            declaration.name.span.end,
        );

        // Handle parameters
        self.symbol_type_context.push(SymbolType::Parameter);
        for param in &declaration.parameters {
            self.visit_fn_param(param, ctx);
        }
        self.symbol_type_context.pop();

        // Handle guard and body
        if let Some(ref guard) = declaration.guard {
            self.visit_expr(guard, ctx);
        }
        
        self.visit_block(&declaration.body.statements, &declaration.body.expression, ctx);

        // Leave function scope
        self.leave_scope(declaration.id, ctx);
    }

    fn visit_expr(&mut self, expr: &'ast Expr, ctx: &mut Self::Context) {
        match &expr.kind {
            ExprKind::Block(block) | ExprKind::Loop(block) => {
                self.enter_scope(block.id, ctx);
                self.visit_block(&block.statements, &block.expression, ctx);
                self.leave_scope(block.id, ctx);
            }
            ExprKind::ForLoop(for_loop) => {
                // Using the expr.id here, as the whole expression will be lowered into a block
                // expression, refering to this expr.id.
                self.enter_scope(expr.id, ctx);
                self.visit_expr(&for_loop.iter, ctx);

                self.enter_scope(for_loop.id, ctx);
                if let Some((pat, expr)) = &for_loop.acc {
                    self.collect_pattern(pat, expr.span.end, ctx);
                    self.visit_expr(expr, ctx);
                }

                self.enter_scope(for_loop.block.id, ctx);
                self.collect_pattern(&for_loop.pat, for_loop.pat.span.end, ctx);
                self.visit_block(&for_loop.block.statements, &for_loop.block.expression, ctx);
                self.leave_scope(for_loop.block.id, ctx);
                self.leave_scope(for_loop.id, ctx);
                self.leave_scope(expr.id, ctx);

                if let Some(else_block) = &for_loop.else_block {
                    self.enter_scope(else_block.id, ctx);
                    self.visit_block(&else_block.statements, &else_block.expression, ctx);
                    self.leave_scope(else_block.id, ctx);
                }
            }
            ExprKind::FunctionExpression(decl) => {
                self.enter_scope(decl.id, ctx);
                let name_as_str = decl.name();

                self.declare_symbol(
                    ctx,
                    decl.id,
                    &name_as_str,
                    SymbolType::FunctionSelfRef(decl.parameters.len() as u16),
                    decl.name.span,
                    decl.name.span.end,
                );

                // Handle parameters
                self.symbol_type_context.push(SymbolType::Parameter);
                for param in &decl.parameters {
                    self.visit_fn_param(param, ctx);
                }
                self.symbol_type_context.pop();

                // Handle guard and body
                if let Some(ref guard) = decl.guard {
                    self.visit_expr(guard, ctx);
                }
                
                self.visit_block(&decl.body.statements, &decl.body.expression, ctx);
                self.leave_scope(decl.id, ctx);
            }
            ExprKind::Let(pattern, expr) => {
                self.visit_expr(expr, ctx);
                self.collect_pattern(pattern, expr.span.end, ctx);
            }
            ExprKind::Match(expr) => {
                self.visit_expr(&expr.expression, ctx);

                for arm in &expr.arms {
                    self.enter_scope(arm.id, ctx);
                    self.collect_pattern(&arm.pattern, arm.pattern.span.end, ctx);
                    if let Some(ref guard) = arm.guard {
                        self.visit_expr(guard, ctx);
                    }

                    match &arm.expression.kind {
                        ExprKind::Block(block) => self.visit_block(&block.statements, &block.expression, ctx),
                        _ => self.visit_expr(&arm.expression, ctx),
                    }

                    self.leave_scope(arm.id, ctx);
                }
            }
            ExprKind::IfElse(if_else_expr) => {
                self.visit_expr(&if_else_expr.condition, ctx);
                
                self.enter_scope(if_else_expr.then_branch.id, ctx);
                self.visit_block(&if_else_expr.then_branch.statements, &if_else_expr.then_branch.expression, ctx);
                self.leave_scope(if_else_expr.then_branch.id, ctx);

                for else_branch in &if_else_expr.else_branches {
                    if let Some(ref condition) = else_branch.condition {
                        self.visit_expr(condition, ctx);
                    }
                    
                    self.enter_scope(else_branch.consequence.id, ctx);
                    self.visit_block(&else_branch.consequence.statements, &else_branch.consequence.expression, ctx);
                    self.leave_scope(else_branch.consequence.id, ctx);
                }
            }
            _ => {
                // For all other expressions, use the default walker
                walk_expr(self, expr, ctx);
            }
        }
    }

    fn visit_fn_param(&mut self, parameter: &'ast FunctionParameter, ctx: &mut Self::Context) {
        self.collect_pattern(&parameter.pattern, parameter.span.end, ctx);
        // Don't visit type annotation as it doesn't contain declarations
    }
}

impl DeclarationAnalyzer {
    fn collect_pattern(&mut self, pattern: &Pat, scope_start: LineColumn, ctx: &mut DeclarationContext) {
        match &pattern.kind {
            PatKind::Identifier(ident) => {
                self.declare_symbol(
                    ctx,
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
                    ctx,
                    pattern.id,
                    kw::_Self,
                    SymbolType::Variable,
                    pattern.span,
                    scope_start,
                );
            }
            PatKind::List(patterns) => {
                for pattern in patterns {
                    self.collect_pattern(pattern, scope_start, ctx);
                }
            }
            PatKind::Rest(pattern) => {
                self.collect_pattern(pattern, scope_start, ctx);
            }
            PatKind::Enum(enum_pattern) => {
                for (_ident, pat) in &enum_pattern.elements {
                    self.collect_pattern(pat, scope_start, ctx);
                }
            }
            PatKind::Wildcard => {} // Wildcard discards values, nothing to do here.
            PatKind::Literal(_) => {} // Literal patterns don't need to be declared.
            _ => {}
        }
    }
}

// Provide public API methods that maintain compatibility with the original interface
impl DeclarationAnalyzer {
    /// Get symbol tables from the analysis context
    pub fn symbol_tables<'a>(&self, ctx: &'a DeclarationContext) -> &'a HashMap<NodeId, Rc<RefCell<SymbolTable>>> {
        ctx.symbol_tables()
    }

    /// Get the symbol ID allocator from the analysis context
    pub fn symbol_id_allocator(&self, ctx: &DeclarationContext) -> SymbolIdAllocator {
        ctx.symbol_id_allocator()
    }

    /// Get the root symbol table from the analysis context
    pub fn root_symbol_table<'a>(&self, ctx: &'a DeclarationContext) -> &'a Rc<RefCell<SymbolTable>> {
        ctx.root_symbol_table()
    }

    /// Add builtin symbols to the analysis context - delegate to context method
    pub fn add_builtin_symbols<'a, S, I>(&mut self, ctx: &mut DeclarationContext, symbols: I)
    where
        S: AsRef<str> + 'a,
        I: IntoIterator<Item = &'a (S, SymbolType)>,
    {
        ctx.add_builtin_symbols(symbols);
    }
}
