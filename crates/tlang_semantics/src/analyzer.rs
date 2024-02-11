use std::{cell::RefCell, rc::Rc};

use tlang_ast::{
    node::{AstNode, ExprKind, Node, NodeKind},
    symbols::{SymbolId, SymbolInfo, SymbolTable, SymbolType},
};

use crate::{
    declarations::DeclarationAnalyzer,
    diagnostic::{Diagnostic, Severity},
};

pub struct SemanticAnalyzer {
    declaration_analyzer: DeclarationAnalyzer,
    symbol_table_stack: Vec<Rc<RefCell<SymbolTable>>>,
    diagnostics: Vec<Diagnostic>,
    identifier_is_declaration: bool,
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
            identifier_is_declaration: false,
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

    fn mark_as_used_by_name(&mut self, name: &str, node: &Node) {
        let symbol_info = self.get_last_symbol_table().borrow().get_by_name(name);

        if let Some(symbol_info) = symbol_info {
            let symbol_table = self.get_last_symbol_table();
            // TODO: This only marks the by it's name, not by it's id. Maybe we should
            //       mark it in the collection pass? Or keep track of the current id in
            //       case the name is being shadowed?
            symbol_table.borrow_mut().mark_as_used(symbol_info.id);
        } else {
            let did_you_mean = did_you_mean(
                name,
                &self.get_last_symbol_table().borrow().get_all_symbols(),
            );

            if let Some(suggestion) = did_you_mean {
                self.diagnostics.push(Diagnostic::new(
                    format!(
                        "Use of undeclared variable `{}`, did you mean the {} `{}`",
                        name, suggestion.symbol_type, suggestion.name
                    ),
                    Severity::Error,
                    node.span.clone(),
                ));
            } else {
                self.diagnostics.push(Diagnostic::new(
                    format!("Use of undeclared variable `{}`", name),
                    Severity::Error,
                    node.span.clone(),
                ));
            }
        }
    }

    #[inline(always)]
    fn analyze_optional_node(&mut self, ast: &mut Option<Node>) {
        if let Some(node) = ast {
            self.analyze_node(node);
        }
    }

    fn analyze_node(&mut self, ast: &mut Node) {
        if let Some(symbol_table) = &ast.symbol_table {
            self.push_symbol_table(symbol_table);
        }

        let mut ast_node = std::mem::take(&mut ast.ast_node);

        match &mut ast_node {
            NodeKind::Expr(expr) => match &mut expr.kind {
                ExprKind::Block(nodes, return_value) => {
                    for node in nodes.iter_mut() {
                        self.analyze_node(node)
                    }

                    self.analyze_optional_node(return_value);
                }
                _ => todo!(),
            },
            NodeKind::Legacy(AstNode::Module(nodes)) => {
                nodes.iter_mut().for_each(|node| self.analyze_node(node))
            }
            NodeKind::Legacy(AstNode::ExpressionStatement(node)) => self.analyze_node(node),
            NodeKind::Legacy(AstNode::Block(nodes, return_value)) => {
                for node in nodes.iter_mut() {
                    self.analyze_node(node)
                }

                self.analyze_optional_node(return_value);
            }
            NodeKind::Legacy(AstNode::VariableDeclaration {
                id,
                pattern,
                expression,
                type_annotation: _,
            }) => self.analyze_variable_declaration(*id, pattern, expression),
            NodeKind::Legacy(AstNode::FunctionSingleDeclaration {
                id: _,
                name,
                declaration,
            }) => self.analyze_function_declaration(ast, name, declaration),
            NodeKind::Legacy(AstNode::FunctionDeclarations {
                id: _,
                name,
                declarations,
            }) => self.analyze_function_declarations(ast, name, declarations),
            NodeKind::Legacy(AstNode::FunctionDeclaration(declaration)) => {
                for parameter in &mut declaration.parameters {
                    self.analyze_node(parameter);
                }

                self.analyze_node(&mut declaration.body);
            }
            NodeKind::Legacy(AstNode::FunctionExpression {
                id: _,
                name,
                declaration,
            }) => {
                self.analyze_optional_node(name);
                self.analyze_node(declaration);
            }
            NodeKind::Legacy(AstNode::FunctionParameter {
                id: _,
                pattern: node,
                type_annotation: _,
            }) => self.analyze_function_parameter(ast, node),
            NodeKind::Legacy(AstNode::ReturnStatement(expr)) => {
                self.analyze_optional_node(expr);
            }
            NodeKind::Legacy(AstNode::IfElse {
                condition,
                then_branch,
                else_branch,
            }) => {
                self.analyze_node(condition);
                self.analyze_node(then_branch);
                self.analyze_optional_node(else_branch);
            }
            NodeKind::Legacy(AstNode::Identifier(_)) if self.identifier_is_declaration => {}
            NodeKind::Legacy(AstNode::Identifier(name)) => self.mark_as_used_by_name(name, ast),
            NodeKind::Legacy(AstNode::NestedIdentifier(idents)) => {
                if let Some(first_ident) = idents.first() {
                    self.mark_as_used_by_name(first_ident, ast);
                }

                // TODO: Handle nested identifiers
            }
            NodeKind::Legacy(AstNode::Call {
                function,
                arguments,
            }) => {
                self.analyze_call(function, arguments);
            }
            NodeKind::Legacy(AstNode::RecursiveCall(node)) => {
                self.analyze_node(node);
            }
            NodeKind::Legacy(AstNode::BinaryOp { op: _, lhs, rhs }) => {
                self.analyze_node(lhs);
                self.analyze_node(rhs);
            }
            NodeKind::Legacy(AstNode::UnaryOp(_, node)) => {
                self.analyze_node(node);
            }
            NodeKind::Legacy(AstNode::List(values)) => {
                for value in values {
                    self.analyze_node(value);
                }
            }
            NodeKind::Legacy(AstNode::IndexExpression { base, index }) => {
                self.analyze_node(base);
                self.analyze_node(index);
            }
            NodeKind::Legacy(AstNode::FieldExpression { base, field: _ }) => {
                self.analyze_node(base);
                // TODO: We are checking for unused variables, this should be refactored into
                //       it's own pass. Skipping analyzing field of variable as we do not have
                //       any type information yet.
                // self.analyze_node(field);
            }
            NodeKind::Legacy(AstNode::ListPattern(patterns)) => {
                for pattern in patterns {
                    self.analyze_declaration_node(pattern);
                }
            }
            NodeKind::Legacy(AstNode::EnumPattern {
                identifier,
                elements: _,
                ..
            }) => {
                self.analyze_node(identifier);
            }
            NodeKind::Legacy(
                AstNode::Dict(_)
                | AstNode::EnumDeclaration { .. }
                | AstNode::EnumVariant { .. }
                | AstNode::IdentifierPattern { .. }
                | AstNode::Range { .. }
                | AstNode::Match { .. }
                | AstNode::MatchArm { .. }
                | AstNode::TypeAnnotation { .. },
            ) => {
                // TODO
            }
            NodeKind::None
            | NodeKind::Legacy(
                AstNode::None
                | AstNode::Wildcard
                | AstNode::Literal(_)
                | AstNode::SingleLineComment(_)
                | AstNode::MultiLineComment(_),
            ) => {
                // Nothing to do here
            }
        }

        ast.ast_node = ast_node;

        if let Some(symbol_table) = &ast.symbol_table {
            let symbol_table = symbol_table.borrow();
            let local_symbols = symbol_table.get_all_local_symbols();
            let mut unused_symbols = local_symbols
                .iter()
                .filter(|symbol| symbol.id != SymbolId::new(0))
                .filter(|symbol| !symbol.used)
                .filter(|symbol| !symbol.name.starts_with('_'))
                .collect::<Vec<_>>();

            for unused_symbol in unused_symbols.iter_mut() {
                self.diagnostics.push(Diagnostic::new(
                    format!(
                        "Unused {} `{}`, if this is intentional, prefix the name with an underscore: `_{}`",
                        unused_symbol.symbol_type, unused_symbol.name, unused_symbol.name
                    ),
                    Severity::Warning,
                    unused_symbol.defined_at.clone().unwrap_or_else(|| ast.span.clone()).clone()
                ));
            }

            self.pop_symbol_table();
        }
    }

    fn analyze_declaration_node(&mut self, ast: &mut Node) {
        self.identifier_is_declaration = true;
        self.analyze_node(ast);
        self.identifier_is_declaration = false;
    }

    fn analyze_variable_declaration(
        &mut self,
        id: SymbolId,
        pattern: &mut Node,
        expression: &mut Node,
    ) {
        self.analyze_declaration_node(pattern);

        // When declaring a variable, we can only reference symbols that were declared before.
        // This includes our own variable name.
        // E.g. `let a = a;` is not allowed. But `let a = 1; let a = a;` is.
        // By removing the symbol from the table while analyzing the expression, we can check
        // whether the expression references any symbols that were not declared before.
        let symbol = self.get_last_symbol_table().borrow_mut().remove(id);

        self.analyze_node(expression);

        if let Some(symbol) = symbol {
            self.get_last_symbol_table()
                .borrow_mut()
                .insert_beginning(symbol);
        }
    }

    fn analyze_function_declaration(
        &mut self,
        _node: &mut Node,
        name: &mut Node,
        declaration: &mut Node,
    ) {
        self.analyze_declaration_node(name);
        self.analyze_node(declaration);
    }

    fn analyze_function_declarations(
        &mut self,
        _node: &mut Node,
        name: &mut Node,
        declarations: &mut Vec<Node>,
    ) {
        self.analyze_declaration_node(name);
        for declaration in declarations {
            self.analyze_node(declaration);
        }
    }

    fn analyze_function_parameter(&mut self, _node: &mut Node, name: &mut Node) {
        self.analyze_declaration_node(name);
    }

    fn analyze_call(&mut self, function: &mut Box<Node>, arguments: &mut [Node]) {
        self.analyze_node(function);
        for argument in arguments {
            self.analyze_node(argument);
        }
    }
}

fn did_you_mean(name: &str, candidates: &[SymbolInfo]) -> Option<SymbolInfo> {
    let mut best_distance = usize::MAX;
    let mut best_candidate = None;
    for candidate in candidates {
        let distance = levenshtein_distance(name, &candidate.name);
        if distance < best_distance {
            best_distance = distance;
            best_candidate = Some(candidate);
        }
    }
    if best_distance < 3 {
        Some(best_candidate.unwrap().clone())
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
