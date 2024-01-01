use std::cell::RefCell;
use std::rc::Rc;
use tlang_ast::{
    node::{AstNode, FunctionDeclaration, Node, PrefixOp},
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

    pub fn add_builtin_symbols(&mut self, symbols: Vec<(&str, SymbolType)>) {
        for (name, symbol_type) in symbols {
            self.root_symbol_table().borrow_mut().insert(SymbolInfo {
                id: SymbolId::new(0), // Builtins have ID 0 for now.
                name: name.to_string(),
                symbol_type,
            });
        }
    }

    pub fn analyze(&mut self, ast: &mut Node) {
        self.collect_declarations(ast);
    }

    fn collect_declarations(&mut self, ast: &mut Node) {
        let mut ast_node = std::mem::replace(&mut ast.ast_node, AstNode::None);

        match ast_node {
            AstNode::Program(ref mut nodes) => self.collect_program_declarations(ast, nodes),
            AstNode::ExpressionStatement(ref mut node) => self.collect_declarations(node),
            AstNode::Block(ref mut nodes, ref mut return_value) => {
                self.collect_block_declarations(ast, nodes, return_value)
            }
            AstNode::VariableDeclaration {
                ref id,
                ref name,
                ref mut value,
            } => self.collect_variable_declaration(ast, *id, name, value),
            AstNode::FunctionDeclaration {
                ref id,
                ref name,
                ref mut declaration,
            } => self.collect_function_declaration(ast, *id, name, declaration),
            AstNode::FunctionDeclarations {
                ref id,
                ref name,
                ref mut declarations,
            } => self.collect_function_declarations(ast, *id, name, declarations),
            AstNode::FunctionParameter {
                ref id,
                ref mut node,
            } => self.collect_function_parameter(ast, *id, node),
            _ => {}
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
        name: &str,
        expr: &mut Node,
    ) {
        self.collect_declarations(expr);

        let symbol_table = self.get_last_symbol_table_mut();

        symbol_table.borrow_mut().insert(SymbolInfo {
            id,
            name: name.to_string(),
            symbol_type: SymbolType::Variable,
        });
    }

    fn collect_function_declaration(
        &mut self,
        node: &mut Node,
        id: SymbolId,
        name: &String,
        declaration: &mut FunctionDeclaration,
    ) {
        let symbol_table = self.get_last_symbol_table_mut();

        symbol_table.borrow_mut().insert(SymbolInfo {
            id,
            name: name.to_string(),
            symbol_type: SymbolType::Function,
        });

        // Function arguments have their own scope.
        self.push_symbol_table();
        node.symbol_table = Some(Rc::clone(&self.get_last_symbol_table()));

        for param in &mut declaration.parameters {
            self.collect_declarations(param);
        }

        self.collect_declarations(&mut declaration.body);
        self.pop_symbol_table();
    }

    fn collect_function_declarations(
        &mut self,
        node: &mut Node,
        id: SymbolId,
        name: &String,
        declarations: &mut [FunctionDeclaration],
    ) {
        let symbol_table = self.get_last_symbol_table_mut();

        symbol_table.borrow_mut().insert(SymbolInfo {
            id,
            name: name.to_string(),
            symbol_type: SymbolType::Function,
        });

        for declaration in declarations {
            // Function arguments have their own scope.
            self.push_symbol_table();
            node.symbol_table = Some(Rc::clone(&self.get_last_symbol_table()));
            for param in &mut declaration.parameters {
                self.collect_declarations(param);
            }
            self.collect_declarations(&mut declaration.body);
            self.pop_symbol_table();
        }
    }

    fn collect_function_parameter(&mut self, _node: &mut Node, id: SymbolId, name: &mut Node) {
        let symbol_table = self.get_last_symbol_table_mut();

        match name.ast_node {
            AstNode::Identifier(ref name) => {
                symbol_table.borrow_mut().insert(SymbolInfo {
                    id,
                    name: name.to_string(),
                    symbol_type: SymbolType::Variable,
                });
            }
            AstNode::List(ref mut nodes) => {
                for node in nodes.iter_mut() {
                    // TODO: I think this id here might be wrong.
                    self.collect_function_parameter(_node, id, node);
                }
            }
            AstNode::PrefixOp(PrefixOp::Rest, ref mut identifier) => match identifier.ast_node {
                AstNode::Identifier(ref name) => {
                    symbol_table.borrow_mut().insert(SymbolInfo {
                        id,
                        name: name.to_string(),
                        symbol_type: SymbolType::Variable,
                    });
                }
                _ => panic!("Expected identifier, found {:?}", identifier.ast_node),
            },
            AstNode::Literal(_) => {} // Nothing to do for literals
            AstNode::EnumExtraction {
                ref mut identifier,
                ref mut elements,
                named_fields,
            } => {
                // TODO: The passed in id is for the enum, not the extracted value.
                self.collect_enum_extraction(_node, id, identifier, elements, named_fields)
            }
            _ => panic!("Expected identifier or list, found {:?}", name.ast_node),
        }
    }

    fn collect_enum_extraction(
        &mut self,
        _node: &mut Node,
        id: SymbolId,
        _identifier: &mut Node,
        elements: &mut [Node],
        _named_fields: bool,
    ) {
        for element in elements.iter_mut() {
            match element.ast_node {
                AstNode::Identifier(ref name) => {
                    self.get_last_symbol_table_mut()
                        .borrow_mut()
                        .insert(SymbolInfo {
                            // TODO: Verify that this id is correct.
                            id,
                            name: name.to_string(),
                            symbol_type: SymbolType::Variable,
                        });
                }
                AstNode::Wildcard => {} // Wildcard discards values, nothing to do here.
                _ => panic!("Expected identifier, found {:?}", element.ast_node),
            }
        }
    }
}
