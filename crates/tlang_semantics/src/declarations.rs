use std::cell::RefCell;
use std::rc::Rc;
use tlang_ast::{
    node::{AstNode, Expr, ExprKind, FunctionDeclaration, Node, NodeKind, Pattern, PatternKind},
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

    fn root_symbol_table(&self) -> Rc<RefCell<SymbolTable>> {
        Rc::clone(&self.symbol_table_stack[0])
    }

    fn get_last_symbol_table(&self) -> Rc<RefCell<SymbolTable>> {
        Rc::clone(self.symbol_table_stack.last().unwrap())
    }

    fn push_symbol_table(&mut self) -> Rc<RefCell<SymbolTable>> {
        let new_symbol_table = Rc::new(RefCell::new(SymbolTable::new(Rc::clone(
            &self.get_last_symbol_table(),
        ))));
        self.symbol_table_stack.push(Rc::clone(&new_symbol_table));

        new_symbol_table
    }

    fn pop_symbol_table(&mut self) -> Rc<RefCell<SymbolTable>> {
        self.symbol_table_stack.pop().unwrap()
    }

    #[inline(always)]
    fn declare_symbol(&mut self, symbol_info: SymbolInfo) {
        self.symbol_table_stack
            .last_mut()
            .unwrap()
            .borrow_mut()
            .insert(symbol_info);
    }

    pub fn add_builtin_symbols(&mut self, symbols: &[(&str, SymbolType)]) {
        for (name, symbol_type) in symbols {
            self.root_symbol_table()
                .borrow_mut()
                .insert(SymbolInfo::new(
                    SymbolId::new(0), // Builtins have ID 0 for now.
                    name,
                    symbol_type.clone(),
                    None,
                ));
        }
    }

    pub fn analyze(&mut self, ast: &mut Node) {
        self.collect_declarations(ast);
    }

    #[inline(always)]
    fn collect_optional_declarations_expr(&mut self, node: &mut Option<Expr>) {
        if let Some(node) = node {
            self.collect_declarations_expr(node);
        }
    }

    fn collect_declarations_expr(&mut self, expr: &mut Expr) {
        match &mut expr.kind {
            ExprKind::Block(stmts, cexpr) => {
                expr.symbol_table = Some(Rc::clone(&self.push_symbol_table()));
                for stmt in stmts {
                    self.collect_declarations(stmt);
                }
                self.collect_optional_declarations_expr(cexpr);
                self.pop_symbol_table();
            }
            ExprKind::FunctionExpression {
                id,
                name,
                declaration,
            } => {
                expr.symbol_table = Some(Rc::clone(&self.push_symbol_table()));
                if let Some(name) = name.as_ref() {
                    let name_as_str = self.fn_identifier_to_string(name);
                    self.declare_symbol(SymbolInfo::new(
                        *id,
                        &name_as_str,
                        SymbolType::Function,
                        Some(name.span.clone()),
                    ));
                }

                self.collect_declarations(declaration);
                self.pop_symbol_table();
            }
            ExprKind::Call {
                function,
                arguments,
            } => {
                self.collect_declarations_expr(function);
                for argument in arguments {
                    self.collect_declarations_expr(argument);
                }
            }
            ExprKind::RecursiveCall(expr) => {
                self.collect_declarations_expr(expr);
            }
            ExprKind::UnaryOp(_, node) => {
                self.collect_declarations_expr(node);
            }
            ExprKind::BinaryOp { lhs, rhs, .. } => {
                self.collect_declarations_expr(lhs);
                self.collect_declarations_expr(rhs);
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
            ExprKind::IfElse {
                condition,
                then_branch,
                else_branch,
            } => {
                self.collect_declarations_expr(condition);
                self.collect_declarations_expr(then_branch);
                self.collect_optional_declarations_expr(else_branch);
            }
            ExprKind::FieldExpression { base, field } => {
                self.collect_declarations_expr(base);
                self.collect_declarations_expr(field);
            }
            ExprKind::IndexExpression { base, index } => {
                self.collect_declarations_expr(base);
                self.collect_declarations_expr(index);
            }
            ExprKind::Match { expression, arms } => {
                self.collect_declarations_expr(expression);
                for arm in arms {
                    self.collect_declarations(arm);
                }
            }
            ExprKind::Range { start, end, .. } => {
                self.collect_declarations_expr(start);
                self.collect_declarations_expr(end);
            }
            ExprKind::Identifier(_) | ExprKind::NestedIdentifier(_) => {
                // Nothing to do here
            }
            ExprKind::Literal(_) => {
                // Nothing to do here
            }
            _ => panic!("Unexpected expression {:?}", expr.kind),
        }
    }

    fn collect_declarations(&mut self, node: &mut Node) {
        let mut ast_node = std::mem::take(&mut node.ast_node);

        match &mut ast_node {
            NodeKind::Expr(expr) => self.collect_declarations_expr(expr),
            NodeKind::Legacy(AstNode::Module(nodes)) => {
                self.collect_module_declarations(node, nodes)
            }
            NodeKind::Legacy(AstNode::ExpressionStatement(node)) => {
                self.collect_declarations_expr(node)
            }
            NodeKind::Legacy(AstNode::VariableDeclaration {
                id,
                pattern,
                expression,
                type_annotation: _,
            }) => self.collect_variable_declaration(*id, pattern, expression),
            NodeKind::Legacy(AstNode::FunctionDeclaration(declaration)) => {
                self.collect_function_declaration(declaration);
            }
            NodeKind::Legacy(AstNode::FunctionSingleDeclaration {
                id,
                name,
                declaration,
            }) => self.collect_function_single_declaration(node, *id, name, declaration),
            NodeKind::Legacy(AstNode::FunctionDeclarations {
                id,
                name,
                declarations,
            }) => self.collect_function_declarations(node, *id, name, declarations),
            NodeKind::Legacy(AstNode::FunctionParameter {
                id: _, // TODO: Do we still need an id for fn params, now that they are on the
                // pattern?
                pattern,
                type_annotation: _,
            }) => self.collect_pattern(pattern),
            NodeKind::Legacy(AstNode::ReturnStatement(expr)) => {
                self.collect_optional_declarations_expr(expr);
            }
            NodeKind::Legacy(AstNode::EnumDeclaration { id, name, variants }) => {
                self.declare_symbol(SymbolInfo::new(
                    *id,
                    &name.to_string(),
                    SymbolType::Enum,
                    Some(node.span.clone()),
                ));

                for variant in variants {
                    self.collect_declarations(variant);
                }
            }
            NodeKind::Legacy(AstNode::EnumVariant {
                name: _,
                parameters,
                ..
            }) => {
                // TODO: We need the parent enum name here.
                // self.collect_declarations(name)

                for param in parameters {
                    self.collect_declarations_expr(param);
                }
            }
            NodeKind::Legacy(AstNode::MatchArm { .. } | AstNode::TypeAnnotation { .. }) => {
                // TODO
            }
            NodeKind::None
            | NodeKind::Legacy(
                AstNode::None
                | AstNode::Wildcard
                | AstNode::SingleLineComment(_)
                | AstNode::MultiLineComment(_),
            ) => {
                // Nothing to do here
            }
        }

        node.ast_node = ast_node;
    }

    fn collect_module_declarations(&mut self, node: &mut Node, nodes: &mut [Node]) {
        node.symbol_table = Some(Rc::clone(&self.push_symbol_table()));

        for node in nodes {
            self.collect_declarations(node);
        }

        self.pop_symbol_table();
    }

    fn collect_variable_declaration(
        &mut self,
        _id: SymbolId,
        pattern: &mut Pattern,
        expr: &mut Expr,
    ) {
        self.collect_declarations_expr(expr);
        self.collect_pattern(pattern);
    }

    /// TODO: This is a temporary solution. We need to find a better way to handle this.
    fn fn_identifier_to_string(&self, identifier: &Expr) -> String {
        match identifier.kind {
            ExprKind::Identifier(ref ident) => ident.to_string(),
            ExprKind::NestedIdentifier(ref idents) => idents
                .iter()
                .map(|ident| ident.to_string())
                .collect::<Vec<_>>()
                .join("::"),
            ExprKind::FieldExpression {
                ref base,
                ref field,
            } => {
                let base_name = match base.kind {
                    ExprKind::Identifier(ref ident) => ident.to_string(),
                    _ => panic!("Expected identifier, found {:?}", base),
                };

                let field_name = match field.kind {
                    ExprKind::Identifier(ref ident) => ident.to_string(),
                    _ => panic!("Expected identifier, found {:?}", field),
                };

                format!("{}.{}", base_name, field_name)
            }
            _ => panic!("Expected identifier, found {:?}", identifier.kind),
        }
    }

    fn collect_function_declaration(&mut self, declaration: &mut FunctionDeclaration) {
        self.symbol_type.push(SymbolType::Parameter);
        for param in &mut declaration.parameters {
            self.collect_declarations(param);
        }
        self.symbol_type.pop();

        self.collect_optional_declarations_expr(&mut declaration.guard);
        self.collect_declarations_expr(&mut declaration.body);
    }

    fn collect_function_single_declaration(
        &mut self,
        node: &mut Node,
        id: SymbolId,
        name: &Expr,
        declaration: &mut Node,
    ) {
        let name_as_str = self.fn_identifier_to_string(name);

        self.declare_symbol(SymbolInfo::new(
            id,
            &name_as_str,
            SymbolType::Function,
            Some(name.span.clone()),
        ));

        // Function arguments have their own scope.
        node.symbol_table = Some(Rc::clone(&self.push_symbol_table()));

        self.collect_declarations(declaration);
        self.pop_symbol_table();
    }

    fn collect_function_declarations(
        &mut self,
        node: &mut Node,
        id: SymbolId,
        name: &Expr,
        declarations: &mut [Node],
    ) {
        let name_as_str = self.fn_identifier_to_string(name);
        self.declare_symbol(SymbolInfo::new(
            id,
            &name_as_str,
            SymbolType::Function,
            Some(name.span.clone()),
        ));

        for declaration_node in declarations {
            node.symbol_table = Some(Rc::clone(&self.push_symbol_table()));
            self.collect_declarations(declaration_node);
            self.pop_symbol_table();
        }
    }

    fn collect_pattern(&mut self, pattern: &mut Pattern) {
        match &mut pattern.kind {
            PatternKind::Identifier { id, name } => {
                self.declare_symbol(SymbolInfo::new(
                    *id,
                    &name.to_string(),
                    self.symbol_type
                        .last()
                        .unwrap_or(&SymbolType::Variable)
                        .clone(),
                    Some(pattern.span.clone()),
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
            PatternKind::EnumPattern { elements, .. } => {
                for element in elements.iter_mut() {
                    self.collect_pattern(element);
                }
            }
            PatternKind::Wildcard => {} // Wildcard discards values, nothing to do here.
            PatternKind::Literal(_) => {} // Literal patterns don't need to be declared.
        }
    }
}
