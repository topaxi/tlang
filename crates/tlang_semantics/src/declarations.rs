use std::cell::RefCell;
use std::rc::Rc;
use tlang_ast::{
    node::{AstNode, FunctionDeclaration, Node, UnaryOp},
    symbols::{SymbolId, SymbolInfo, SymbolTable, SymbolType},
};

pub struct DeclarationAnalyzer {
    symbol_table_stack: Vec<Rc<RefCell<SymbolTable>>>,
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
        }
    }

    fn root_symbol_table(&self) -> Rc<RefCell<SymbolTable>> {
        Rc::clone(&self.symbol_table_stack[0])
    }

    fn get_last_symbol_table(&self) -> Rc<RefCell<SymbolTable>> {
        Rc::clone(self.symbol_table_stack.last().unwrap())
    }

    fn get_last_symbol_table_mut(&mut self) -> &mut Rc<RefCell<SymbolTable>> {
        self.symbol_table_stack.last_mut().unwrap()
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

    pub fn add_builtin_symbols(&mut self, symbols: &[(&str, SymbolType)]) {
        for (name, symbol_type) in symbols {
            self.root_symbol_table()
                .borrow_mut()
                .insert(SymbolInfo::new(
                    SymbolId::new(0), // Builtins have ID 0 for now.
                    &name,
                    symbol_type.clone(),
                ));
        }
    }

    pub fn analyze(&mut self, ast: &mut Node) {
        self.collect_declarations(ast);
    }

    fn collect_declarations(&mut self, ast: &mut Node) {
        let mut ast_node = std::mem::replace(&mut ast.ast_node, AstNode::None);

        match &mut ast_node {
            AstNode::Program(nodes) => self.collect_program_declarations(ast, nodes),
            AstNode::ExpressionStatement(node) => self.collect_declarations(node),
            AstNode::Block(nodes, return_value) => {
                self.collect_block_declarations(ast, nodes, return_value)
            }
            AstNode::VariableDeclaration {
                id,
                pattern,
                expression,
                type_annotation: _,
            } => self.collect_variable_declaration(ast, *id, pattern, expression),
            AstNode::FunctionDeclaration(decl) => {
                // TODO: Refactor and deduplicate function declaration handling.
                for param in &mut decl.parameters {
                    self.collect_declarations(param);
                }
                if let Some(ref mut guard) = decl.guard {
                    self.collect_declarations(guard);
                }
                self.collect_declarations(&mut decl.body);
            }
            AstNode::FunctionSingleDeclaration {
                id,
                name,
                declaration,
            } => self.collect_function_declaration(ast, *id, name, declaration),
            AstNode::FunctionDeclarations {
                id,
                name,
                declarations,
            } => self.collect_function_declarations(ast, *id, name, declarations),
            AstNode::FunctionParameter {
                id,
                node,
                type_annotation: _,
            } => self.collect_function_parameter(ast, *id, node),
            AstNode::FunctionExpression {
                id,
                name,
                declaration,
            } => {
                self.push_symbol_table();
                ast.symbol_table = Some(Rc::clone(&self.get_last_symbol_table()));
                if let Some(name) = name {
                    let name_as_str = self.fn_identifier_to_string(name);
                    let symbol_table = self.get_last_symbol_table_mut();

                    symbol_table.borrow_mut().insert(SymbolInfo::new(
                        *id,
                        &name_as_str,
                        SymbolType::Function,
                    ));
                }

                for param in &mut declaration.parameters {
                    self.collect_declarations(param);
                }

                self.collect_declarations(&mut declaration.body);
                self.pop_symbol_table();
            }
            AstNode::ReturnStatement(expr) => {
                if let Some(expr) = expr {
                    self.collect_declarations(expr);
                }
            }
            AstNode::ListPattern(patterns) => {
                self.collect_list_pattern(ast, patterns);
            }
            AstNode::EnumPattern {
                identifier,
                elements,
                named_fields,
            } => self.collect_enum_pattern(
                ast,
                SymbolId::new(0),
                identifier,
                elements,
                *named_fields,
            ),
            AstNode::Call {
                function,
                arguments,
            } => {
                self.collect_declarations(function);
                for argument in arguments {
                    self.collect_declarations(argument);
                }
            }
            AstNode::RecursiveCall(expr) => {
                self.collect_declarations(expr);
            }
            AstNode::UnaryOp(_, node) => self.collect_declarations(node),
            AstNode::BinaryOp { op: _, lhs, rhs } => {
                self.collect_declarations(lhs);
                self.collect_declarations(rhs);
            }
            AstNode::List(values) => {
                for value in values {
                    self.collect_declarations(value);
                }
            }
            AstNode::Dict(kvs) => {
                for (key, value) in kvs {
                    // TODO: Undecided what kind of values dict keys can be.
                    self.collect_declarations(key);
                    self.collect_declarations(value);
                }
            }
            AstNode::IfElse {
                condition,
                then_branch,
                else_branch,
            } => {
                self.collect_declarations(condition);
                self.collect_declarations(then_branch);
                if let Some(else_branch) = else_branch {
                    self.collect_declarations(else_branch);
                }
            }
            AstNode::FieldExpression { base, field } => {
                self.collect_declarations(base);
                self.collect_declarations(field);
            }
            AstNode::IndexExpression { base, index } => {
                self.collect_declarations(base);
                self.collect_declarations(index);
            }
            AstNode::EnumDeclaration { .. }
            | AstNode::EnumVariant { .. }
            | AstNode::Match { .. }
            | AstNode::MatchArm { .. }
            | AstNode::Range { .. }
            | AstNode::TypeAnnotation { .. } => {
                // TODO
            }
            AstNode::None
            | AstNode::Wildcard
            | AstNode::Identifier(_)
            | AstNode::NestedIdentifier(_)
            | AstNode::Literal(_)
            | AstNode::SingleLineComment(_)
            | AstNode::MultiLineComment(_) => {
                // Nothing to do here
            }
        }

        ast.ast_node = ast_node;
    }

    fn collect_program_declarations(&mut self, node: &mut Node, nodes: &mut [Node]) {
        node.symbol_table = Some(Rc::clone(&self.root_symbol_table()));

        for node in nodes {
            self.collect_declarations(node);
        }
    }

    fn collect_block_declarations(
        &mut self,
        node: &mut Node,
        nodes: &mut [Node],
        expr: &mut Option<Box<Node>>,
    ) {
        self.push_symbol_table();
        node.symbol_table = Some(Rc::clone(&self.get_last_symbol_table()));

        for node in nodes {
            self.collect_declarations(node);
        }

        if let Some(expr) = expr {
            self.collect_declarations(expr);
        }

        self.pop_symbol_table();
    }

    fn collect_variable_declaration(
        &mut self,
        _node: &mut Node,
        id: SymbolId,
        pattern: &mut Node,
        expr: &mut Node,
    ) {
        self.collect_declarations(expr);

        let symbol_table = self.get_last_symbol_table_mut();

        match pattern.ast_node {
            AstNode::Identifier(ref name) => {
                symbol_table
                    .borrow_mut()
                    .insert(SymbolInfo::new(id, &name, SymbolType::Variable));
            }
            AstNode::ListPattern(ref mut patterns) => {
                for pattern in patterns {
                    // TODO: We probably need to do this recursively.
                    if let AstNode::Identifier(ref name) = pattern.ast_node {
                        symbol_table.borrow_mut().insert(SymbolInfo::new(
                            id,
                            &name,
                            SymbolType::Variable,
                        ));
                    }
                }
            }
            AstNode::EnumPattern { .. } => self.collect_declarations(pattern),
            AstNode::VariableDeclaration { .. } => self.collect_declarations(pattern),
            _ => panic!("Expected identifier, found {:?}", pattern.ast_node),
        };
    }

    /// TODO: This is a temporary solution. We need to find a better way to handle this.
    fn fn_identifier_to_string(&self, identifier: &Node) -> String {
        match identifier.ast_node {
            AstNode::Identifier(ref name) => name.to_string(),
            AstNode::NestedIdentifier(ref names) => names.join("::"),
            AstNode::FieldExpression {
                ref base,
                ref field,
            } => {
                let base_name = match base.ast_node {
                    AstNode::Identifier(ref name) => name.to_string(),
                    _ => panic!("Expected identifier, found {:?}", base),
                };

                let field_name = match field.ast_node {
                    AstNode::Identifier(ref name) => name.to_string(),
                    _ => panic!("Expected identifier, found {:?}", field),
                };

                format!("{}.{}", base_name, field_name)
            }
            _ => panic!("Expected identifier, found {:?}", identifier.ast_node),
        }
    }

    fn collect_function_declaration(
        &mut self,
        node: &mut Node,
        id: SymbolId,
        name: &Node,
        declaration: &mut FunctionDeclaration,
    ) {
        let name_as_str = self.fn_identifier_to_string(name);
        let symbol_table = self.get_last_symbol_table_mut();

        symbol_table
            .borrow_mut()
            .insert(SymbolInfo::new(id, &name_as_str, SymbolType::Function));

        // Function arguments have their own scope.
        self.push_symbol_table();
        node.symbol_table = Some(Rc::clone(&self.get_last_symbol_table()));

        for param in &mut declaration.parameters {
            self.collect_declarations(param);
        }

        if let Some(ref mut guard) = declaration.guard {
            self.collect_declarations(guard);
        }

        self.collect_declarations(&mut declaration.body);
        self.pop_symbol_table();
    }

    fn collect_function_declarations(
        &mut self,
        node: &mut Node,
        id: SymbolId,
        name: &Node,
        declarations: &mut [Node],
    ) {
        let name_as_str = self.fn_identifier_to_string(name);
        let symbol_table = self.get_last_symbol_table_mut();

        symbol_table
            .borrow_mut()
            .insert(SymbolInfo::new(id, &name_as_str, SymbolType::Function));

        self.push_symbol_table();
        node.symbol_table = Some(Rc::clone(&self.get_last_symbol_table()));

        for declaration_node in declarations {
            self.collect_declarations(declaration_node);
        }

        self.pop_symbol_table();
    }

    fn collect_function_parameter(&mut self, _node: &mut Node, id: SymbolId, name: &mut Node) {
        let symbol_table = self.get_last_symbol_table_mut();

        match name.ast_node {
            AstNode::Identifier(ref name) => {
                symbol_table
                    .borrow_mut()
                    .insert(SymbolInfo::new(id, &name, SymbolType::Parameter));
            }
            AstNode::ListPattern(ref mut nodes) => {
                for node in nodes.iter_mut() {
                    // TODO: I think this id here might be wrong.
                    self.collect_function_parameter(_node, id, node);
                }
            }
            AstNode::UnaryOp(UnaryOp::Rest, ref mut identifier) => match identifier.ast_node {
                AstNode::Identifier(ref name) => {
                    symbol_table.borrow_mut().insert(SymbolInfo::new(
                        id,
                        name,
                        SymbolType::Parameter,
                    ));
                }
                _ => panic!("Expected identifier, found {:?}", identifier.ast_node),
            },
            AstNode::Literal(_) => {} // Nothing to do for literals
            AstNode::EnumPattern {
                ref mut identifier,
                ref mut elements,
                named_fields,
            } => {
                // TODO: The passed in id is for the enum, not the extracted value.
                self.collect_enum_pattern(_node, id, identifier, elements, named_fields)
            }
            AstNode::Wildcard => {} // Wildcard discards values, nothing to do here.
            _ => panic!("Expected identifier or list, found {:?}", name.ast_node),
        }
    }

    fn collect_list_pattern(&mut self, _node: &mut Node, _patterns: &mut [Node]) {
        // TODO
    }

    fn collect_enum_pattern(
        &mut self,
        _node: &mut Node,
        id: SymbolId,
        _identifier: &mut Node,
        elements: &mut [Node],
        _named_fields: bool,
    ) {
        for element in elements.iter_mut() {
            println!("Collecting enum pattern {:?}", element);

            match element.ast_node {
                AstNode::Identifier(ref name) => {
                    self.get_last_symbol_table_mut()
                        .borrow_mut()
                        .insert(SymbolInfo::new(
                            // TODO: Verify that this id is correct.
                            id,
                            &name,
                            SymbolType::Variable,
                        ));
                }
                AstNode::Wildcard => {} // Wildcard discards values, nothing to do here.
                _ => panic!("Expected identifier, found {:?}", element.ast_node),
            }
        }
    }
}
