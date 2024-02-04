use crate::node;

pub trait Visitor<'ast>: Sized {
    fn visit_module(&mut self, statements: &'ast [node::Node]) {
        walk_module(self, statements);
    }

    fn visit_block(
        &mut self,
        statements: &'ast [node::Node],
        expression: &'ast Option<node::Node>,
    ) {
        walk_block(self, statements, expression);
    }

    fn visit_statement(&mut self, statement: &'ast node::Node) {
        walk_statement(self, statement);
    }

    fn visit_function_declaration(&mut self, declaration: &'ast node::FunctionDeclaration) {
        walk_function_declaration(self, declaration);
    }

    fn visit_expression(&mut self, expression: &'ast node::Node) {
        walk_expression(self, expression);
    }

    fn visit_function_parameter(&mut self, _parameter: &'ast node::Node) {
        todo!()
    }

    fn visit_function_guard(&mut self, _guard: &'ast node::Node) {
        todo!()
    }

    fn visit_function_return_type_annotation(&mut self, _annotation: &'ast node::Node) {
        todo!()
    }

    fn visit_function_body(&mut self, body: &'ast node::Node) {
        walk_function_body(self, body);
    }
}

pub fn walk_module<'ast, V: Visitor<'ast>>(visitor: &mut V, statements: &'ast [node::Node]) {
    for statement in statements {
        visitor.visit_statement(statement);
    }
}

pub fn walk_block<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    statements: &'ast [node::Node],
    expression: &'ast Option<node::Node>,
) {
    for statement in statements {
        visitor.visit_statement(statement);
    }
    if let Some(expression) = expression {
        visitor.visit_expression(expression);
    }
}

pub fn walk_statement<'ast, V: Visitor<'ast>>(visitor: &mut V, statement: &'ast node::Node) {
    match &statement.ast_node {
        node::AstNode::None => {}
        node::AstNode::Block(statements, expression) => {
            visitor.visit_block(statements, expression);
        }
        node::AstNode::FunctionDeclaration(declaration) => {
            visitor.visit_function_declaration(declaration);
        }
        _ => todo!(),
    }
}

pub fn walk_function_body<'ast, V: Visitor<'ast>>(visitor: &mut V, body: &'ast node::Node) {
    visitor.visit_expression(body);
}

pub fn walk_function_declaration<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    declaration: &'ast node::FunctionDeclaration,
) {
    for parameter in &declaration.parameters {
        visitor.visit_function_parameter(parameter)
    }
    if let Some(ref guard) = *declaration.guard {
        visitor.visit_function_guard(guard);
    }
    if let Some(ref return_type_annotation) = *declaration.return_type_annotation {
        visitor.visit_function_return_type_annotation(return_type_annotation);
    }
    visitor.visit_function_body(&declaration.body);
}

pub fn walk_expression<'ast, V: Visitor<'ast>>(visitor: &mut V, expression: &'ast node::Node) {
    match &expression.ast_node {
        node::AstNode::None => {}
        node::AstNode::Block(statements, expression) => {
            visitor.visit_block(statements, expression);
        }
        node::AstNode::BinaryOp { op: _, lhs, rhs } => {
            visitor.visit_expression(lhs);
            visitor.visit_expression(rhs);
        }
        node::AstNode::UnaryOp(_, expression) => {
            visitor.visit_expression(expression);
        }
        _ => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::node::AstNode;
    use crate::node::Node;
    use crate::span::Span;

    struct TestVisitor {
        visited: Vec<&'static str>,
    }

    impl<'ast> Visitor<'ast> for TestVisitor {
        fn visit_module(&mut self, statements: &'ast [Node]) {
            self.visited.push("visit_module");
            walk_module(self, statements);
        }
        fn visit_block(&mut self, statements: &'ast [Node], expression: &'ast Option<Node>) {
            self.visited.push("visit_block");
            walk_block(self, statements, expression);
        }
        fn visit_statement(&mut self, statement: &'ast Node) {
            self.visited.push("visit_statement");
            walk_statement(self, statement);
        }
        fn visit_function_declaration(&mut self, declaration: &'ast node::FunctionDeclaration) {
            self.visited.push("visit_function_declaration");
            walk_function_declaration(self, declaration);
        }
        fn visit_expression(&mut self, expression: &'ast Node) {
            self.visited.push("visit_expression");
            walk_expression(self, expression);
        }
        fn visit_function_parameter(&mut self, _parameter: &'ast Node) {
            self.visited.push("visit_function_parameter");
        }
        fn visit_function_guard(&mut self, _guard: &'ast Node) {
            self.visited.push("visit_function_guard");
        }
        fn visit_function_return_type_annotation(&mut self, _annotation: &'ast Node) {
            self.visited.push("visit_function_return_type_annotation");
        }
        fn visit_function_body(&mut self, body: &'ast Node) {
            self.visited.push("visit_function_body");
            walk_function_body(self, body);
        }
    }

    #[test]
    fn test_walk_module() {
        let statements = vec![Node {
            ast_node: AstNode::None,
            symbol_table: None,
            span: Span::default(),
        }];
        let mut visitor = TestVisitor { visited: vec![] };
        walk_module(&mut visitor, &statements);
        assert_eq!(visitor.visited, vec!["visit_statement"]);
    }

    #[test]
    fn test_walk_block() {
        let statements = vec![Node {
            ast_node: AstNode::None,
            symbol_table: None,
            span: Span::default(),
        }];
        let expression = Some(Node {
            ast_node: AstNode::None,
            symbol_table: None,
            span: Span::default(),
        });
        let mut visitor = TestVisitor { visited: vec![] };
        walk_block(&mut visitor, &statements, &expression);
        assert_eq!(visitor.visited, vec!["visit_statement", "visit_expression"]);
    }

    #[test]
    fn test_walk_statement() {
        let statement = Node {
            ast_node: AstNode::Block(vec![], Box::new(None)),
            symbol_table: None,
            span: Span::default(),
        };
        let mut visitor = TestVisitor { visited: vec![] };
        walk_statement(&mut visitor, &statement);
        assert_eq!(visitor.visited, vec!["visit_block"]);
    }

    #[test]
    fn test_walk_function_declaration() {
        let declaration = node::FunctionDeclaration {
            parameters: vec![Node {
                ast_node: AstNode::None,
                symbol_table: None,
                span: Span::default(),
            }],
            guard: Box::new(Some(Node {
                ast_node: AstNode::None,
                symbol_table: None,
                span: Span::default(),
            })),
            return_type_annotation: Box::new(None),
            body: Box::new(Node {
                ast_node: AstNode::Block(vec![], Box::new(None)),
                symbol_table: None,
                span: Span::default(),
            }),
        };
        let mut visitor = TestVisitor { visited: vec![] };
        walk_function_declaration(&mut visitor, &declaration);
        assert_eq!(
            visitor.visited,
            vec![
                "visit_function_parameter",
                "visit_function_guard",
                "visit_function_body",
                "visit_expression",
                "visit_block"
            ]
        );
    }
}
