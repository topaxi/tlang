use std::cell::RefCell;
use std::rc::Rc;
use tlang_ast::{
    node::{AstNode, FunctionDeclaration, Node},
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
    fn collect_optional_declarations(&mut self, node: &mut Option<Node>) {
        if let Some(node) = node {
            self.collect_declarations(node);
        }
    }

    fn collect_declarations(&mut self, node: &mut Node) {
        let mut ast_node = std::mem::take(&mut node.ast_node);

        match &mut ast_node {
            AstNode::Module(nodes) => self.collect_module_declarations(node, nodes),
            AstNode::ExpressionStatement(node) => self.collect_declarations(node),
            AstNode::Block(nodes, return_value) => {
                self.collect_block_declarations(node, nodes, return_value)
            }
            AstNode::IdentifierPattern { id, name } => {
                self.declare_symbol(SymbolInfo::new(
                    *id,
                    name,
                    SymbolType::Variable,
                    Some(node.span.clone()),
                ));
            }
            AstNode::VariableDeclaration {
                id,
                pattern,
                expression,
                type_annotation: _,
            } => self.collect_variable_declaration(node, *id, pattern, expression),
            AstNode::FunctionDeclaration(declaration) => {
                self.collect_function_declaration(declaration);
            }
            AstNode::FunctionSingleDeclaration {
                id,
                name,
                declaration,
            } => self.collect_function_single_declaration(node, *id, name, declaration),
            AstNode::FunctionDeclarations {
                id,
                name,
                declarations,
            } => self.collect_function_declarations(node, *id, name, declarations),
            AstNode::FunctionParameter {
                id,
                pattern,
                type_annotation: _,
            } => self.collect_function_parameter(node, *id, pattern),
            AstNode::FunctionExpression {
                id,
                name,
                declaration,
            } => {
                node.symbol_table = Some(Rc::clone(&self.push_symbol_table()));
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
            AstNode::ReturnStatement(expr) => {
                self.collect_optional_declarations(expr);
            }
            AstNode::ListPattern(patterns) => {
                self.collect_list_pattern(patterns);
            }
            AstNode::EnumPattern {
                identifier,
                elements,
                named_fields,
            } => self.collect_enum_pattern(node, identifier, elements, *named_fields),
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
                self.collect_optional_declarations(else_branch);
            }
            AstNode::FieldExpression { base, field } => {
                self.collect_declarations(base);
                self.collect_declarations(field);
            }
            AstNode::IndexExpression { base, index } => {
                self.collect_declarations(base);
                self.collect_declarations(index);
            }
            AstNode::EnumDeclaration { id, name, variants } => {
                self.declare_symbol(SymbolInfo::new(
                    *id,
                    name,
                    SymbolType::Enum,
                    Some(node.span.clone()),
                ));

                for variant in variants {
                    self.collect_declarations(variant);
                }
            }
            AstNode::EnumVariant {
                name: _,
                parameters,
                ..
            } => {
                // TODO: We need the parent enum name here.
                // self.collect_declarations(name)

                for param in parameters {
                    self.collect_declarations(param);
                }
            }
            AstNode::Match { .. }
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

        node.ast_node = ast_node;
    }

    fn collect_module_declarations(&mut self, node: &mut Node, nodes: &mut [Node]) {
        node.symbol_table = Some(Rc::clone(&self.push_symbol_table()));

        for node in nodes {
            self.collect_declarations(node);
        }

        self.pop_symbol_table();
    }

    fn collect_block_declarations(
        &mut self,
        node: &mut Node,
        nodes: &mut [Node],
        expr: &mut Box<Option<Node>>,
    ) {
        node.symbol_table = Some(Rc::clone(&self.push_symbol_table()));

        for node in nodes {
            self.collect_declarations(node);
        }

        self.collect_optional_declarations(expr);

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

        match pattern.ast_node {
            AstNode::Identifier(ref name) => {
                self.declare_symbol(SymbolInfo::new(
                    id,
                    name,
                    SymbolType::Variable,
                    Some(pattern.span.clone()),
                ));
            }
            AstNode::ListPattern(_) => self.collect_declarations(pattern),
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

    fn collect_function_declaration(&mut self, declaration: &mut FunctionDeclaration) {
        for param in &mut declaration.parameters {
            self.collect_declarations(param);
        }

        self.collect_optional_declarations(&mut declaration.guard);
        self.collect_declarations(&mut declaration.body);
    }

    fn collect_function_single_declaration(
        &mut self,
        node: &mut Node,
        id: SymbolId,
        name: &Node,
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
        name: &Node,
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

    fn collect_function_parameter(&mut self, _node: &mut Node, id: SymbolId, pattern: &mut Node) {
        match pattern.ast_node {
            AstNode::Identifier(ref name) => {
                self.declare_symbol(SymbolInfo::new(
                    id,
                    name,
                    SymbolType::Parameter,
                    Some(pattern.span.clone()),
                ));
            }
            AstNode::IdentifierPattern { .. } => self.collect_declarations(pattern),
            AstNode::ListPattern(_) => self.collect_declarations(pattern),
            AstNode::EnumPattern { .. } => self.collect_declarations(pattern),
            AstNode::Wildcard => {} // Wildcard discards values, nothing to do here.
            AstNode::Literal(_) => {} // Literal patterns don't need to be declared.
            _ => panic!(
                "Expected identifier, list or enum pattern, found {:?}",
                pattern.ast_node
            ),
        }
    }

    fn collect_list_pattern(&mut self, patterns: &mut [Node]) {
        for pattern in patterns {
            self.collect_declarations(pattern);
        }
    }

    fn collect_enum_pattern(
        &mut self,
        _node: &mut Node,
        _identifier: &mut Node,
        elements: &mut [Node],
        _named_fields: bool,
    ) {
        for element in elements.iter_mut() {
            println!("Collecting enum pattern {:?}", element);

            match element.ast_node {
                AstNode::IdentifierPattern { .. } => self.collect_declarations(element),
                AstNode::Wildcard => {} // Wildcard discards values, nothing to do here.
                _ => panic!("Expected identifier, found {:?}", element.ast_node),
            }
        }
    }
}
