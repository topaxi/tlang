use std::{cell::RefCell, rc::Rc};

use tlang_ast::{
    node::{AstNode, FunctionDeclaration, Node},
    symbols::{SymbolTable, SymbolType},
};

use crate::declarations::DeclarationAnalyzer;

pub struct SemanticAnalyzer {
    declaration_analyzer: DeclarationAnalyzer,
    symbol_table_stack: Vec<Rc<RefCell<SymbolTable>>>,
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
        }
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

    pub fn add_builtin_symbols(&mut self, symbols: Vec<(&str, SymbolType)>) {
        self.declaration_analyzer.add_builtin_symbols(symbols)
    }

    pub fn analyze(&mut self, ast: &mut Node) {
        self.collect_declarations(ast);
        // self.collect_initializations(ast);
        self.analyze_node(ast);
    }

    fn collect_declarations(&mut self, ast: &mut Node) {
        self.declaration_analyzer.analyze(ast);
    }

    fn analyze_node(&mut self, ast: &mut Node) {
        if let Some(symbol_table) = &ast.symbol_table {
            self.push_symbol_table(symbol_table);
        }

        let mut ast_node = std::mem::replace(&mut ast.ast_node, AstNode::None);

        match ast_node {
            AstNode::Program(ref mut nodes) => {
                nodes.iter_mut().for_each(|node| self.analyze_node(node))
            }
            AstNode::ExpressionStatement(ref mut node) => self.analyze_node(node),
            AstNode::Block(ref mut nodes, ref mut return_value) => {
                nodes.iter_mut().for_each(|node| self.analyze_node(node));

                if let Some(return_value) = return_value {
                    self.analyze_node(return_value);
                }
            }
            AstNode::VariableDeclaration {
                id: _,
                ref name,
                ref mut value,
            } => self.analyze_variable_declaration(name, value),
            AstNode::FunctionDeclaration {
                id: _,
                ref name,
                ref mut declaration,
            } => self.analyze_function_declaration(ast, name, declaration),
            AstNode::FunctionDeclarations {
                id: _,
                ref name,
                ref mut declarations,
            } => self.analyze_function_declarations(ast, name, declarations),
            AstNode::FunctionParameter {
                id: _,
                ref mut node,
            } => self.analyze_function_parameter(ast, node),
            AstNode::Identifier(ref name) => {
                if self.get_last_symbol_table().borrow().get(name).is_none() {
                    panic!("Undefined symbol: {}", name);
                }
            }
            AstNode::BinaryOp {
                op: _,
                ref mut lhs,
                ref mut rhs,
            } => {
                self.analyze_node(lhs);
                self.analyze_node(rhs);
            }
            _ => {}
        }

        ast.ast_node = ast_node;

        if ast.symbol_table.is_some() {
            self.pop_symbol_table();
        }
    }

    fn analyze_variable_declaration(&mut self, name: &str, value: &mut Box<Node>) {
        // When declaring a variable, we can only reference symbols that were declared before.
        // This includes our own variable name.
        // E.g. `let a = a;` is not allowed. But `let a = 1; let a = a;` is.

        // This is not fully implemented yet, let's temporarily remove the symbol from the current
        // symbol table, this will make sure that we cannot self-reference at all for the moment.
        // Which we then can enable again once we have a proper implementation.

        let symbol = self
            .get_last_symbol_table()
            .borrow_mut()
            .symbols
            .remove(name);

        self.analyze_node(value);

        if let Some(symbol) = symbol {
            self.get_last_symbol_table()
                .borrow_mut()
                .insert(name.to_string(), symbol);
        }
    }

    fn analyze_function_declaration(
        &mut self,
        _node: &mut Node,
        _name: &str,
        declaration: &mut FunctionDeclaration,
    ) {
        for parameter in &mut declaration.parameters {
            self.analyze_node(parameter);
        }
        self.analyze_node(&mut declaration.body);
    }

    fn analyze_function_declarations(
        &mut self,
        node: &mut Node,
        name: &str,
        declarations: &mut Vec<FunctionDeclaration>,
    ) {
        for declaration in declarations {
            self.analyze_function_declaration(node, name, declaration)
        }
    }

    fn analyze_function_parameter(&mut self, _node: &mut Node, _name: &mut Node) {
        // TODO: In case we have default arguments, we'll have to check whether the used nodes are
        // declared.
    }
}
