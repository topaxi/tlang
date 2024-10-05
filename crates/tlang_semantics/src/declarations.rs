use std::cell::RefCell;
use std::rc::Rc;
use tlang_ast::{
    node::{
        Block, Expr, ExprKind, FunctionDeclaration, FunctionParameter, LetDeclaration, Module,
        Pattern, PatternKind, Stmt, StmtKind,
    },
    symbols::{SymbolId, SymbolInfo, SymbolTable, SymbolType},
};

pub struct DeclarationAnalyzer {
    symbol_table_stack: Vec<Rc<RefCell<SymbolTable>>>,
    symbol_type: Vec<SymbolType>,
}

impl Default for DeclarationAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

impl DeclarationAnalyzer {
    pub fn new() -> Self {
        DeclarationAnalyzer {
            symbol_table_stack: vec![Rc::new(RefCell::new(SymbolTable::default()))],
            symbol_type: vec![],
        }
    }

    fn root_symbol_table(&self) -> &Rc<RefCell<SymbolTable>> {
        &self.symbol_table_stack[0]
    }

    fn get_last_symbol_table(&self) -> &Rc<RefCell<SymbolTable>> {
        self.symbol_table_stack.last().unwrap()
    }

    fn push_symbol_table(&mut self) -> Rc<RefCell<SymbolTable>> {
        let parent = Rc::clone(self.get_last_symbol_table());
        let new_symbol_table = Rc::new(RefCell::new(SymbolTable::new(parent)));
        self.symbol_table_stack.push(Rc::clone(&new_symbol_table));

        new_symbol_table
    }

    fn pop_symbol_table(&mut self) -> Rc<RefCell<SymbolTable>> {
        self.symbol_table_stack.pop().unwrap()
    }

    #[inline(always)]
    fn declare_symbol(&mut self, symbol_info: SymbolInfo) {
        let symbol_table = self.get_last_symbol_table();

        // Multiple function declarations with the same name result in SymbolInfo with the same ID.
        if symbol_table.borrow().get(symbol_info.id).is_none() {
            symbol_table.borrow_mut().insert(symbol_info);
        }
    }

    pub fn add_builtin_symbols(&mut self, symbols: &[(&str, SymbolType)]) {
        for (name, symbol_type) in symbols {
            self.root_symbol_table()
                .borrow_mut()
                .insert(SymbolInfo::new(
                    SymbolId::new(0), // Builtins have ID 0 for now.
                    name,
                    *symbol_type,
                    None,
                ));
        }
    }

    pub fn analyze(&mut self, module: &mut Module) {
        self.collect_module_declarations(module);
    }

    #[inline(always)]
    fn collect_optional_declarations_expr(&mut self, node: &mut Option<Expr>) {
        if let Some(node) = node {
            self.collect_declarations_expr(node);
        }
    }

    fn collect_declarations_stmt(&mut self, stmt: &mut Stmt) {
        let mut kind = std::mem::take(&mut stmt.kind);

        match &mut kind {
            StmtKind::None => {
                // Nothing to do here
            }
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
            StmtKind::Return(expr) => self.collect_optional_declarations_expr(expr),
            StmtKind::EnumDeclaration(decl) => {
                self.declare_symbol(SymbolInfo::new(
                    decl.id,
                    decl.name.as_str(),
                    SymbolType::Enum,
                    Some(stmt.span),
                ));
            }
            StmtKind::StructDeclaration(decl) => {
                self.declare_symbol(SymbolInfo::new(
                    decl.id,
                    decl.name.as_str(),
                    SymbolType::Struct,
                    Some(stmt.span),
                ));
            }
        }

        stmt.kind = kind;
    }

    fn collect_declarations_from_fn(&mut self, function_decl: &mut FunctionDeclaration) {
        self.symbol_type.push(SymbolType::Parameter);
        for param in &mut function_decl.parameters {
            self.collect_declarations_from_fn_param(param);
        }
        self.symbol_type.pop();
        self.collect_optional_declarations_expr(&mut function_decl.guard);
        self.collect_declarations_block(&mut function_decl.body);
    }

    fn collect_declarations_from_fn_param(&mut self, param: &mut FunctionParameter) {
        self.collect_pattern(&mut param.pattern);
    }

    fn collect_declarations_block(&mut self, block: &mut Block) {
        block.symbol_table = Some(Rc::clone(&self.push_symbol_table()));
        for stmt in &mut block.statements {
            self.collect_declarations_stmt(stmt);
        }

        self.collect_optional_declarations_expr(&mut block.expression);
        self.pop_symbol_table();
    }

    fn collect_declarations_expr(&mut self, expr: &mut Expr) {
        match &mut expr.kind {
            ExprKind::Block(block) => self.collect_declarations_block(block),
            ExprKind::FunctionExpression(decl) => {
                expr.symbol_table = Some(Rc::clone(&self.push_symbol_table()));
                let name_as_str = self.fn_identifier_to_string(&decl.name);

                if name_as_str != "anonymous" {
                    self.declare_symbol(SymbolInfo::new(
                        decl.id,
                        &name_as_str,
                        SymbolType::Function,
                        Some(decl.name.span),
                    ));
                }

                self.collect_declarations_from_fn(decl);
                self.pop_symbol_table();
            }
            ExprKind::Call(expr) | ExprKind::RecursiveCall(expr) => {
                self.collect_declarations_expr(&mut expr.callee);
                for argument in &mut expr.arguments {
                    self.collect_declarations_expr(argument);
                }
            }
            ExprKind::UnaryOp(_, node) => {
                self.collect_declarations_expr(node);
            }
            ExprKind::BinaryOp(expr) => {
                self.collect_declarations_expr(&mut expr.lhs);
                self.collect_declarations_expr(&mut expr.rhs);
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
                self.collect_declarations_expr(&mut expr.condition);
                self.collect_declarations_expr(&mut expr.then_branch);

                for else_branch in &mut expr.else_branches {
                    self.collect_optional_declarations_expr(&mut else_branch.condition);
                    self.collect_declarations_expr(&mut else_branch.consequence);
                }
            }
            ExprKind::FieldExpression(expr) => {
                self.collect_declarations_expr(&mut expr.base);
            }
            ExprKind::IndexExpression(expr) => {
                self.collect_declarations_expr(&mut expr.base);
                self.collect_declarations_expr(&mut expr.index);
            }
            ExprKind::Match(expr) => {
                self.collect_declarations_expr(&mut expr.expression);
                for arm in &mut expr.arms {
                    self.collect_pattern(&mut arm.pattern);
                    self.collect_declarations_expr(&mut arm.expression);
                }
            }
            ExprKind::Range(expr) => {
                self.collect_declarations_expr(&mut expr.start);
                self.collect_declarations_expr(&mut expr.end);
            }
            ExprKind::Path(_) | ExprKind::Literal(_) | ExprKind::Wildcard | ExprKind::None => {
                // Nothing to do here
            }
        }
    }

    fn collect_module_declarations(&mut self, module: &mut Module) {
        module.symbol_table = Some(Rc::clone(&self.push_symbol_table()));

        for stmt in &mut module.statements {
            self.collect_declarations_stmt(stmt);
        }

        self.pop_symbol_table();
    }

    fn collect_variable_declaration(&mut self, decl: &mut LetDeclaration) {
        self.collect_declarations_expr(&mut decl.expression);
        self.collect_pattern(&mut decl.pattern);
    }

    /// TODO: This is a temporary solution. We need to find a better way to handle this.
    #[allow(clippy::only_used_in_recursion)]
    fn fn_identifier_to_string(&self, identifier: &Expr) -> String {
        match identifier.kind {
            ExprKind::Path(ref path) => path.join("::"),
            ExprKind::FieldExpression(ref expr) => {
                let base_name = self.fn_identifier_to_string(&expr.base);

                format!("{base_name}.{}", expr.field)
            }
            _ => panic!("Expected identifier, found {:?}", identifier.kind),
        }
    }

    fn collect_function_declaration(&mut self, declaration: &mut FunctionDeclaration) {
        let name_as_str = self.fn_identifier_to_string(&declaration.name);

        self.declare_symbol(SymbolInfo::new(
            declaration.id,
            &name_as_str,
            SymbolType::Function,
            Some(declaration.name.span),
        ));

        declaration.symbol_table = Some(Rc::clone(&self.push_symbol_table()));

        self.symbol_type.push(SymbolType::Parameter);
        for param in &mut declaration.parameters {
            self.collect_declarations_from_fn_param(param);
        }
        self.symbol_type.pop();

        self.collect_optional_declarations_expr(&mut declaration.guard);
        self.collect_declarations_block(&mut declaration.body);

        self.pop_symbol_table();
    }

    fn collect_pattern(&mut self, pattern: &mut Pattern) {
        match &mut pattern.kind {
            PatternKind::Identifier { id, name } => {
                self.declare_symbol(SymbolInfo::new(
                    *id,
                    name.as_str(),
                    *self.symbol_type.last().unwrap_or(&SymbolType::Variable),
                    Some(pattern.span),
                ));
            }
            PatternKind::List(patterns) => {
                for pattern in patterns {
                    self.collect_pattern(pattern);
                }
            }
            PatternKind::Rest(pattern) => {
                self.collect_pattern(pattern);
            }
            PatternKind::Enum { elements, .. } => {
                for element in elements.iter_mut() {
                    self.collect_pattern(element);
                }
            }
            PatternKind::Wildcard => {} // Wildcard discards values, nothing to do here.
            PatternKind::Literal(_) => {} // Literal patterns don't need to be declared.
        }
    }
}
