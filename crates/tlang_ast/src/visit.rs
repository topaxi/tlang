use crate::node;

pub trait Visitor<'ast>: Sized {
    fn visit_module(&mut self, module: &'ast node::Module) {
        walk_module(self, module);
    }

    fn visit_block(
        &mut self,
        statements: &'ast [node::Stmt],
        expression: &'ast Option<node::Expr>,
    ) {
        walk_block(self, statements, expression);
    }

    fn visit_ident(&mut self, _ident: &'ast node::Ident) {}

    fn visit_pat(&mut self, pattern: &'ast node::Pattern) {
        walk_pat(self, pattern);
    }

    fn visit_stmt(&mut self, statement: &'ast node::Stmt) {
        walk_stmt(self, statement);
    }

    fn visit_fn_decl(&mut self, declaration: &'ast node::FunctionDeclaration) {
        walk_fn_decl(self, declaration);
    }

    fn visit_expr(&mut self, expression: &'ast node::Expr) {
        walk_expr(self, expression);
    }

    fn visit_fn_param(&mut self, parameter: &'ast node::FunctionParameter) {
        walk_pat(self, &parameter.pattern);

        if let Some(ref type_annotation) = *parameter.type_annotation {
            walk_ty(self, &type_annotation);
        }
    }

    fn visit_fn_guard(&mut self, guard: &'ast node::Expr) {
        walk_expr(self, guard);
    }

    fn visit_fn_ret_ty(&mut self, annotation: &'ast node::Ty) {
        walk_ty(self, annotation);
    }

    fn visit_fn_body(&mut self, body: &'ast node::Expr) {
        walk_fn_body(self, body);
    }
}

pub fn walk_module<'ast, V: Visitor<'ast>>(visitor: &mut V, module: &'ast node::Module) {
    for statement in &module.statements {
        visitor.visit_stmt(statement);
    }
}

pub fn walk_pat<'ast, V: Visitor<'ast>>(visitor: &mut V, pattern: &'ast node::Pattern) {
    match &pattern.kind {
        node::PatternKind::Identifier { name, .. } => visitor.visit_ident(name),
        node::PatternKind::Rest(pattern) => visitor.visit_pat(pattern),
        node::PatternKind::List(patterns) => {
            for pattern in patterns {
                visitor.visit_pat(pattern);
            }
        }
        node::PatternKind::Enum {
            identifier,
            elements,
            ..
        } => {
            visitor.visit_expr(identifier);
            for element in elements {
                visitor.visit_pat(element);
            }
        }
        node::PatternKind::Literal(literal) => {
            visitor.visit_expr(literal);
        }
        node::PatternKind::Wildcard => {}
    }
}

pub fn walk_ty<'ast, V: Visitor<'ast>>(_visitor: &mut V, _ty: &'ast node::Ty) {
    todo!()
}

pub fn walk_block<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    statements: &'ast [node::Stmt],
    expression: &'ast Option<node::Expr>,
) {
    for statement in statements {
        visitor.visit_stmt(statement);
    }
    if let Some(expression) = expression {
        visitor.visit_expr(expression);
    }
}

pub fn walk_stmt<'ast, V: Visitor<'ast>>(visitor: &mut V, statement: &'ast node::Stmt) {
    match &statement.kind {
        node::StmtKind::None => {}
        node::StmtKind::Expr(expr) => {
            visitor.visit_expr(expr);
        }
        node::StmtKind::FunctionDeclaration(declaration) => {
            visitor.visit_fn_decl(declaration);
        }
        _ => todo!(),
    }
}

pub fn walk_fn_body<'ast, V: Visitor<'ast>>(visitor: &mut V, body: &'ast node::Expr) {
    visitor.visit_expr(body);
}

pub fn walk_fn_decl<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    declaration: &'ast node::FunctionDeclaration,
) {
    for parameter in &declaration.parameters {
        visitor.visit_fn_param(parameter)
    }
    if let Some(ref guard) = *declaration.guard {
        visitor.visit_fn_guard(guard);
    }
    if let Some(ref return_type_annotation) = *declaration.return_type_annotation {
        visitor.visit_fn_ret_ty(return_type_annotation);
    }
    visitor.visit_fn_body(&declaration.body);
}

pub fn walk_expr<'ast, V: Visitor<'ast>>(visitor: &mut V, expression: &'ast node::Expr) {
    match &expression.kind {
        node::ExprKind::None => {}
        node::ExprKind::Block(statements, expression) => {
            visitor.visit_block(statements, expression);
        }
        node::ExprKind::BinaryOp { op: _, lhs, rhs } => {
            visitor.visit_expr(lhs);
            visitor.visit_expr(rhs);
        }
        node::ExprKind::UnaryOp(_, expression) => {
            visitor.visit_expr(expression);
        }
        _ => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{span::Span, symbols::SymbolId};

    struct TestVisitor {
        visited: Vec<&'static str>,
    }

    impl<'ast> Visitor<'ast> for TestVisitor {
        fn visit_module(&mut self, module: &'ast node::Module) {
            self.visited.push("visit_module");
            walk_module(self, module);
        }
        fn visit_block(
            &mut self,
            statements: &'ast [node::Stmt],
            expression: &'ast Option<node::Expr>,
        ) {
            self.visited.push("visit_block");
            walk_block(self, statements, expression);
        }
        fn visit_stmt(&mut self, statement: &'ast node::Stmt) {
            self.visited.push("visit_statement");
            walk_stmt(self, statement);
        }
        fn visit_fn_decl(&mut self, declaration: &'ast node::FunctionDeclaration) {
            self.visited.push("visit_function_declaration");
            walk_fn_decl(self, declaration);
        }
        fn visit_expr(&mut self, expression: &'ast node::Expr) {
            self.visited.push("visit_expression");
            walk_expr(self, expression);
        }
        fn visit_fn_param(&mut self, _parameter: &'ast node::FunctionParameter) {
            self.visited.push("visit_function_parameter");
        }
        fn visit_fn_guard(&mut self, _guard: &'ast node::Expr) {
            self.visited.push("visit_function_guard");
        }
        fn visit_fn_ret_ty(&mut self, _annotation: &'ast node::Ty) {
            self.visited.push("visit_function_return_type_annotation");
        }
        fn visit_fn_body(&mut self, body: &'ast node::Expr) {
            self.visited.push("visit_function_body");
            walk_fn_body(self, body);
        }
    }

    #[test]
    fn test_walk_module() {
        let module = node::Module::new(vec![node::Stmt {
            kind: node::StmtKind::None,
            symbol_table: None,
            span: Span::default(),
        }]);
        let mut visitor = TestVisitor { visited: vec![] };
        walk_module(&mut visitor, &module);
        assert_eq!(visitor.visited, vec!["visit_statement"]);
    }

    #[test]
    fn test_walk_block() {
        let statements = vec![node::Stmt {
            kind: node::StmtKind::None,
            symbol_table: None,
            span: Span::default(),
        }];
        let expression = Some(node::Expr {
            kind: node::ExprKind::None,
            symbol_table: None,
            span: Span::default(),
        });
        let mut visitor = TestVisitor { visited: vec![] };
        walk_block(&mut visitor, &statements, &expression);
        assert_eq!(visitor.visited, vec!["visit_statement", "visit_expression"]);
    }

    #[test]
    fn test_walk_statement() {
        let statement = node::Stmt {
            kind: node::StmtKind::Expr(Box::new(node::Expr::new(node::ExprKind::None))),
            symbol_table: None,
            span: Span::default(),
        };
        let mut visitor = TestVisitor { visited: vec![] };
        walk_stmt(&mut visitor, &statement);
        assert_eq!(visitor.visited, vec!["visit_expression"]);
    }

    #[test]
    fn test_walk_function_declaration() {
        let declaration = node::FunctionDeclaration {
            id: SymbolId::new(2),
            name: Box::new(node::Expr::new(node::ExprKind::Path(node::Path::new(
                vec![node::Ident::new("my_fn", Span::default())],
            )))),
            parameters: vec![node::FunctionParameter {
                pattern: Box::new(node::Pattern::new(node::PatternKind::Identifier {
                    id: SymbolId::new(1),
                    name: node::Ident::new("x", Span::default()),
                })),
                type_annotation: Box::new(None),
                span: Span::default(),
            }],
            guard: Box::new(Some(node::Expr {
                kind: node::ExprKind::None,
                symbol_table: None,
                span: Span::default(),
            })),
            return_type_annotation: Box::new(None),
            body: Box::new(node::Expr {
                kind: node::ExprKind::Block(vec![], Box::new(None)),
                symbol_table: None,
                span: Span::default(),
            }),
        };
        let mut visitor = TestVisitor { visited: vec![] };
        walk_fn_decl(&mut visitor, &declaration);
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
