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

        match &mut ast_node {
            AstNode::Program(nodes) => nodes.iter_mut().for_each(|node| self.analyze_node(node)),
            AstNode::ExpressionStatement(node) => self.analyze_node(node),
            AstNode::Block(nodes, ref mut return_value) => {
                nodes.iter_mut().for_each(|node| self.analyze_node(node));

                if let Some(return_value) = return_value {
                    self.analyze_node(return_value);
                }
            }
            AstNode::VariableDeclaration {
                id,
                pattern,
                expression,
                type_annotation: _,
            } => self.analyze_variable_declaration(*id, pattern, expression),
            AstNode::FunctionSingleDeclaration {
                id: _,
                name,
                declaration,
            } => self.analyze_function_declaration(ast, name, declaration),
            AstNode::FunctionDeclarations {
                id: _,
                name,
                declarations,
            } => self.analyze_function_declarations(ast, name, declarations),
            AstNode::FunctionDeclaration(declaration) => {
                for parameter in &mut declaration.parameters {
                    self.analyze_node(parameter);
                }

                self.analyze_node(&mut declaration.body);
            }
            AstNode::FunctionExpression {
                id: _,
                name,
                declaration,
            } => {
                if let Some(name) = name {
                    self.analyze_node(name);
                }
                for parameter in &mut declaration.parameters {
                    self.analyze_node(parameter);
                }
                self.analyze_node(&mut declaration.body);
            }
            AstNode::FunctionParameter {
                id: _,
                node,
                type_annotation: _,
            } => self.analyze_function_parameter(ast, node),
            AstNode::ReturnStatement(expr) => {
                if let Some(expr) = expr {
                    self.analyze_node(expr);
                }
            }
            AstNode::IfElse {
                condition,
                then_branch,
                else_branch,
            } => {
                self.analyze_node(condition);
                self.analyze_node(then_branch);
                if let Some(else_branch) = else_branch {
                    self.analyze_node(else_branch);
                }
            }
            AstNode::Identifier(name) => {
                if let Some(ref mut symbol_info) =
                    self.get_last_symbol_table().borrow().get_by_name(name)
                {
                    // How does this even work, we clone the struct 🤔
                    symbol_info.used = true;
                } else {
                    let did_you_mean = did_you_mean(
                        name,
                        &self
                            .get_last_symbol_table()
                            .borrow()
                            .get_all_symbol_names()
                            .iter()
                            .collect::<Vec<_>>(),
                    );

                    if let Some(suggestion) = did_you_mean {
                        self.diagnostics.push(Diagnostic::new(
                            format!(
                                "Use of undeclared variable `{}`, did you mean `{}`",
                                name, suggestion
                            ),
                            Severity::Error,
                        ));
                    } else {
                        self.diagnostics.push(Diagnostic::new(
                            format!("Use of undeclared variable `{}`", name),
                            Severity::Error,
                        ));
                    }
                }
            }
            AstNode::NestedIdentifier(_idents) => {
                // TODO
            }
            AstNode::Call {
                function,
                arguments,
            } => {
                self.analyze_call(function, arguments);
            }
            AstNode::RecursiveCall(node) => {
                self.analyze_node(node);
            }
            AstNode::BinaryOp { op: _, lhs, rhs } => {
                self.analyze_node(lhs);
                self.analyze_node(rhs);
            }
            AstNode::UnaryOp(_, node) => {
                self.analyze_node(node);
            }
            AstNode::List(values) => values.iter_mut().for_each(|node| self.analyze_node(node)),
            AstNode::IndexExpression { base, index } => {
                self.analyze_node(base);
                self.analyze_node(index);
            }
            AstNode::ListPattern(_)
            | AstNode::Dict(_)
            | AstNode::EnumDeclaration { .. }
            | AstNode::EnumVariant { .. }
            | AstNode::EnumPattern { .. }
            | AstNode::FieldExpression { .. }
            | AstNode::Range { .. }
            | AstNode::Match { .. }
            | AstNode::MatchArm { .. }
            | AstNode::TypeAnnotation { .. } => {
                // TODO
            }
            AstNode::None
            | AstNode::Wildcard
            | AstNode::Literal(_)
            | AstNode::SingleLineComment(_)
            | AstNode::MultiLineComment(_) => {
                // Nothing to do here
            }
        }

        ast.ast_node = ast_node;

        if let Some(symbol_table) = &ast.symbol_table {
            let symbol_table = symbol_table.borrow();
            let local_symbols = symbol_table.get_all_local_symbols();
            let mut unused_symbols = local_symbols
                .iter()
                .filter(|symbol| !symbol.used)
                .collect::<Vec<_>>();

            for unused_symbol in unused_symbols.iter_mut() {
                self.diagnostics.push(Diagnostic::new(
                    format!("Unused variable `{}`", unused_symbol.name),
                    Severity::Warning,
                ));
            }

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
        name: &mut Node,
        declaration: &mut FunctionDeclaration,
    ) {
        self.analyze_node(name);

        for parameter in &mut declaration.parameters {
            self.analyze_node(parameter);
        }
        if let Some(ref mut guard) = declaration.guard {
            self.analyze_node(guard);
        }
        self.analyze_node(&mut declaration.body);
    }

    fn analyze_function_declarations(
        &mut self,
        node: &mut Node,
        name: &mut Node,
        declarations: &mut Vec<Node>,
    ) {
        for declaration in declarations {
            if let AstNode::FunctionDeclaration(decl) = &mut declaration.ast_node {
                self.analyze_function_declaration(node, name, decl);
            }
        }
    }

    fn analyze_function_parameter(&mut self, _node: &mut Node, name: &mut Node) {
        self.analyze_node(name);
    }

    fn analyze_call(&mut self, function: &mut Box<Node>, arguments: &mut [Node]) {
        self.analyze_node(function);
        for argument in arguments {
            self.analyze_node(argument);
        }
    }
}

fn did_you_mean(name: &str, candidates: &[&String]) -> Option<String> {
    let mut best_distance = usize::MAX;
    let mut best_candidate = None;
    for candidate in candidates {
        let distance = levenshtein_distance(name, candidate);
        if distance < best_distance {
            best_distance = distance;
            best_candidate = Some(candidate);
        }
    }
    if best_distance < 3 {
        Some(best_candidate.unwrap().to_string())
    } else {
        None
    }
}

fn levenshtein_distance(a: &str, b: &str) -> usize {
    let mut matrix = vec![vec![0; b.len() + 1]; a.len() + 1];
    for i in 0..=a.len() {
        matrix[i][0] = i;
    }
    for j in 0..=b.len() {
        matrix[0][j] = j;
    }
    for j in 1..=b.len() {
        for i in 1..=a.len() {
            let substitution_cost = if a.chars().nth(i - 1) == b.chars().nth(j - 1) {
                0
            } else {
                1
            };
            matrix[i][j] = *[
                matrix[i - 1][j] + 1,
                matrix[i][j - 1] + 1,
                matrix[i - 1][j - 1] + substitution_cost,
            ]
            .iter()
            .min()
            .unwrap();
        }
    }
    matrix[a.len()][b.len()]
}
