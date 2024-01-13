use std::{cell::RefCell, rc::Rc};

use tlang_ast::{
    node::{AstNode, FunctionDeclaration, Node},
    symbols::{SymbolId, SymbolTable, SymbolType},
};

use crate::{
    declarations::DeclarationAnalyzer,
    diagnostic::{Diagnostic, Severity},
};

pub struct SemanticAnalyzer {
    declaration_analyzer: DeclarationAnalyzer,
    symbol_table_stack: Vec<Rc<RefCell<SymbolTable>>>,
    diagnostics: Vec<Diagnostic>,
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
        }
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
        self.declaration_analyzer.add_builtin_symbols(symbols)
    }

    pub fn analyze(&mut self, ast: &mut Node) -> Result<(), Vec<Diagnostic>> {
        self.collect_declarations(ast);
        // self.collect_initializations(ast);
        self.analyze_node(ast);

        if self.get_errors().is_empty() {
            Ok(())
        } else {
            Err(self.get_errors().clone())
        }
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
                id,
                ref pattern,
                ref mut expression,
                type_annotation: _,
            } => self.analyze_variable_declaration(id, pattern, expression),
            AstNode::FunctionSingleDeclaration {
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
                type_annotation: _,
            } => self.analyze_function_parameter(ast, node),
            AstNode::Identifier(ref name) => {
                if self
                    .get_last_symbol_table()
                    .borrow()
                    .get_by_name(name)
                    .is_none()
                {
                    self.diagnostics.push(Diagnostic::new(
                        format!("Use of undeclared variable `{}`", name),
                        Severity::Error,
                    ));
                }
            }
            AstNode::Call {
                ref mut function,
                ref mut arguments,
            } => {
                self.analyze_call(function, arguments);
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

    fn analyze_variable_declaration(
        &mut self,
        id: SymbolId,
        _pattern: &Node,
        expression: &mut Box<Node>,
    ) {
        // When declaring a variable, we can only reference symbols that were declared before.
        // This includes our own variable name.
        // E.g. `let a = a;` is not allowed. But `let a = 1; let a = a;` is.
        // By removing the symbol from the table while analyzing the expression, we can check
        // whether the expression references any symbols that were not declared before.
        let symbol = self.get_last_symbol_table().borrow_mut().remove(id);

        self.analyze_node(expression);

        if let Some(symbol) = symbol {
            self.get_last_symbol_table().borrow_mut().insert(symbol);
        }
    }

    fn analyze_function_declaration(
        &mut self,
        _node: &mut Node,
        _name: &Node,
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
        name: &Node,
        declarations: &mut Vec<Node>,
    ) {
        for declaration in declarations {
            if let AstNode::FunctionDeclaration(decl) = &mut declaration.ast_node {
                self.analyze_function_declaration(node, name, decl);
            }
        }
    }

    fn analyze_function_parameter(&mut self, _node: &mut Node, _name: &mut Node) {
        // TODO: In case we have default arguments, we'll have to check whether the used nodes are
        // declared.
    }

    fn analyze_call(&mut self, function: &mut Box<Node>, arguments: &mut [Node]) {
        self.analyze_node(function);
        for argument in arguments {
            self.analyze_node(argument);
        }
    }
}
