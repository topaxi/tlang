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

    fn visit_pat(&mut self, pattern: &'ast node::Pat) {
        walk_pat(self, pattern);
    }

    fn visit_stmt(&mut self, statement: &'ast node::Stmt) {
        walk_stmt(self, statement);
    }

    fn visit_struct_decl(&mut self, decl: &'ast node::StructDeclaration) {
        self.visit_ident(&decl.name);

        for node::StructField { name, ty, .. } in &decl.fields {
            self.visit_ident(name);
            self.visit_ty(ty);
        }
    }

    fn visit_enum_decl(&mut self, decl: &'ast node::EnumDeclaration) {
        walk_enum_decl(self, decl);
    }

    fn visit_fn_decls(&mut self, declarations: &'ast [node::FunctionDeclaration]) {
        for declaration in declarations {
            walk_fn_decl(self, declaration);
        }
    }

    fn visit_fn_decl(&mut self, declaration: &'ast node::FunctionDeclaration) {
        walk_fn_decl(self, declaration);
    }

    fn visit_expr(&mut self, expression: &'ast node::Expr) {
        walk_expr(self, expression);
    }

    fn visit_else_clause(&mut self, clause: &'ast node::ElseClause) {
        if let Some(ref condition) = clause.condition {
            walk_expr(self, condition);
        }

        walk_block(
            self,
            &clause.consequence.statements,
            &clause.consequence.expression,
        );
    }

    fn visit_fn_param(&mut self, parameter: &'ast node::FunctionParameter) {
        walk_fn_param(self, parameter);
    }

    fn visit_fn_guard(&mut self, guard: &'ast node::Expr) {
        walk_expr(self, guard);
    }

    fn visit_ty(&mut self, ty: &'ast node::Ty) {
        walk_ty(self, ty);
    }

    fn visit_fn_ret_ty(&mut self, annotation: &'ast node::Ty) {
        walk_ty(self, annotation);
    }

    fn visit_fn_body(&mut self, body: &'ast node::Block) {
        walk_fn_body(self, body);
    }

    fn visit_variant(&mut self, variant: &'ast node::EnumVariant) {
        walk_variant(self, variant);
    }

    fn visit_path(&mut self, path: &'ast node::Path) {
        walk_path(self, path);
    }
}

pub fn walk_module<'ast, V: Visitor<'ast>>(visitor: &mut V, module: &'ast node::Module) {
    for statement in &module.statements {
        visitor.visit_stmt(statement);
    }
}

pub fn walk_path<'ast, V: Visitor<'ast>>(visitor: &mut V, path: &'ast node::Path) {
    for segment in &path.segments {
        visitor.visit_ident(segment);
    }
}

pub fn walk_pat<'ast, V: Visitor<'ast>>(visitor: &mut V, pattern: &'ast node::Pat) {
    match &pattern.kind {
        node::PatKind::Identifier(ident_pattern) => visitor.visit_ident(ident_pattern),
        node::PatKind::Rest(pattern) => visitor.visit_pat(pattern),
        node::PatKind::List(patterns) => {
            for pattern in patterns {
                visitor.visit_pat(pattern);
            }
        }
        node::PatKind::Enum(enum_pattern) => {
            visitor.visit_path(&enum_pattern.path);

            for (ident, pat) in &enum_pattern.elements {
                visitor.visit_ident(ident);
                visitor.visit_pat(pat);
            }
        }
        node::PatKind::Literal(_) => {}
        node::PatKind::_Self => {}
        node::PatKind::Wildcard => {}
        node::PatKind::None => {}
    }
}

pub fn walk_ty<'ast, V: Visitor<'ast>>(visitor: &mut V, ty: &'ast node::Ty) {
    for ty in &ty.parameters {
        visitor.visit_ty(ty);
    }
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
        node::StmtKind::Let(let_decl) => {
            visitor.visit_pat(&let_decl.pattern);
            visitor.visit_expr(&let_decl.expression);

            if let Some(ty) = &let_decl.type_annotation {
                visitor.visit_ty(ty);
            }
        }
        node::StmtKind::FunctionDeclaration(declaration) => {
            visitor.visit_fn_decl(declaration);
        }
        node::StmtKind::FunctionDeclarations(declarations) => {
            visitor.visit_fn_decls(declarations);
        }
        node::StmtKind::Return(expr) => {
            if let Some(expr) = expr.as_ref() {
                visitor.visit_expr(expr);
            }
        }
        node::StmtKind::StructDeclaration(decl) => visitor.visit_struct_decl(decl),
        node::StmtKind::EnumDeclaration(decl) => visitor.visit_enum_decl(decl),
    }
}

pub fn walk_enum_decl<'ast, V: Visitor<'ast>>(visitor: &mut V, decl: &'ast node::EnumDeclaration) {
    visitor.visit_ident(&decl.name);

    for variant in &decl.variants {
        visitor.visit_variant(variant);
    }
}

pub fn walk_variant<'ast, V: Visitor<'ast>>(visitor: &mut V, variant: &'ast node::EnumVariant) {
    visitor.visit_ident(&variant.name);

    for node::StructField { name, ty, .. } in &variant.parameters {
        visitor.visit_ident(name);
        visitor.visit_ty(ty);
    }
}

pub fn walk_fn_param<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    parameter: &'ast node::FunctionParameter,
) {
    visitor.visit_pat(&parameter.pattern);

    if let Some(ref type_annotation) = parameter.type_annotation {
        visitor.visit_ty(type_annotation);
    }
}

pub fn walk_fn_body<'ast, V: Visitor<'ast>>(visitor: &mut V, body: &'ast node::Block) {
    visitor.visit_block(&body.statements, &body.expression);
}

pub fn walk_fn_decl<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    declaration: &'ast node::FunctionDeclaration,
) {
    for parameter in &declaration.parameters {
        visitor.visit_fn_param(parameter);
    }
    if let Some(ref guard) = declaration.guard {
        visitor.visit_fn_guard(guard);
    }
    if let Some(ref return_type_annotation) = declaration.return_type_annotation {
        visitor.visit_fn_ret_ty(return_type_annotation);
    }
    visitor.visit_fn_body(&declaration.body);
}

pub fn walk_expr<'ast, V: Visitor<'ast>>(visitor: &mut V, expression: &'ast node::Expr) {
    match &expression.kind {
        node::ExprKind::None => {}
        node::ExprKind::Block(block) | node::ExprKind::Loop(block) => {
            visitor.visit_block(&block.statements, &block.expression);
        }
        node::ExprKind::Break(Some(expr)) => {
            visitor.visit_expr(expr);
        }
        node::ExprKind::Break(_) => {}
        node::ExprKind::ForLoop(for_loop) => {
            if let Some((pat, expr)) = &for_loop.acc {
                visitor.visit_pat(pat);
                visitor.visit_expr(expr);
            }

            visitor.visit_pat(&for_loop.pat);
            visitor.visit_expr(&for_loop.iter);
            visitor.visit_block(&for_loop.block.statements, &for_loop.block.expression);
        }
        node::ExprKind::BinaryOp(binary_op_expression) => {
            visitor.visit_expr(&binary_op_expression.lhs);
            visitor.visit_expr(&binary_op_expression.rhs);
        }
        node::ExprKind::UnaryOp(_, expression) => {
            visitor.visit_expr(expression);
        }
        node::ExprKind::FieldExpression(field_expr) => {
            visitor.visit_expr(&field_expr.base);
            visitor.visit_ident(&field_expr.field);
        }
        node::ExprKind::Call(call_expr) | node::ExprKind::RecursiveCall(call_expr) => {
            for argument in &call_expr.arguments {
                visitor.visit_expr(argument);
            }
            visitor.visit_expr(&call_expr.callee);
        }
        node::ExprKind::Dict(kvs) => {
            for (key, value) in kvs {
                visitor.visit_expr(key);
                visitor.visit_expr(value);
            }
        }
        node::ExprKind::FunctionExpression(decl) => {
            visitor.visit_fn_decl(decl);
        }
        node::ExprKind::IndexExpression(index_expr) => {
            visitor.visit_expr(&index_expr.base);
            visitor.visit_expr(&index_expr.index);
        }
        node::ExprKind::Let(pattern, expr) => {
            visitor.visit_pat(pattern);
            visitor.visit_expr(expr);
        }
        node::ExprKind::Literal(_) => {}
        node::ExprKind::List(exprs) => {
            for expr in exprs {
                visitor.visit_expr(expr);
            }
        }
        node::ExprKind::IfElse(if_else_expr) => {
            visitor.visit_expr(&if_else_expr.condition);
            visitor.visit_block(
                &if_else_expr.then_branch.statements,
                &if_else_expr.then_branch.expression,
            );

            for else_branch in &if_else_expr.else_branches {
                visitor.visit_else_clause(else_branch);
            }
        }
        node::ExprKind::Match(match_expr) => {
            visitor.visit_expr(&match_expr.expression);

            for arm in &match_expr.arms {
                visitor.visit_pat(&arm.pattern);
                visitor.visit_expr(&arm.expression);
            }
        }
        node::ExprKind::Path(path) => {
            visitor.visit_path(path);
        }
        node::ExprKind::Range(range_expr) => {
            visitor.visit_expr(&range_expr.start);
            visitor.visit_expr(&range_expr.end);
        }
        node::ExprKind::Wildcard => {}
        node::ExprKind::Continue => {}
        node::ExprKind::Cast(..) => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use self::node::{BinaryOpExpression, BinaryOpKind, Ident, Path};
    use pretty_assertions::assert_eq;

    use super::*;
    use crate::node_id::NodeId;
    use crate::span::Span;
    use crate::token::Literal;

    struct TestVisitor {
        visited: Vec<&'static str>,
    }

    impl<'ast> Visitor<'ast> for TestVisitor {
        fn visit_module(&mut self, module: &'ast node::Module) {
            self.visited.push("visit_module");
            walk_module(self, module);
        }
        fn visit_ident(&mut self, _ident: &'ast node::Ident) {
            self.visited.push("visit_identifier");
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
        fn visit_pat(&mut self, pattern: &'ast node::Pat) {
            self.visited.push("visit_pattern");
            walk_pat(self, pattern);
        }
        fn visit_fn_param(&mut self, parameter: &'ast node::FunctionParameter) {
            self.visited.push("visit_function_parameter");
            walk_fn_param(self, parameter);
        }
        fn visit_fn_guard(&mut self, guard: &'ast node::Expr) {
            self.visited.push("visit_function_guard");
            walk_expr(self, guard);
        }
        fn visit_fn_ret_ty(&mut self, _annotation: &'ast node::Ty) {
            self.visited.push("visit_function_return_type_annotation");
        }
        fn visit_fn_body(&mut self, body: &'ast node::Block) {
            self.visited.push("visit_function_body");
            walk_fn_body(self, body);
        }
    }

    #[test]
    fn test_walk_module() {
        let module = node::Module::new(NodeId::new(0), vec![node::Stmt::default()]);
        let mut visitor = TestVisitor { visited: vec![] };
        walk_module(&mut visitor, &module);
        assert_eq!(visitor.visited, vec!["visit_statement"]);
    }

    #[test]
    fn test_walk_block() {
        let statements = vec![node::Stmt::default()];
        let expression = Some(node::Expr::default());
        let mut visitor = TestVisitor { visited: vec![] };
        walk_block(&mut visitor, &statements, &expression);
        assert_eq!(visitor.visited, vec!["visit_statement", "visit_expression"]);
    }

    #[test]
    fn test_walk_statement() {
        let statement = node::Stmt::new(NodeId::new(0), node::StmtKind::Expr(Box::default()));
        let mut visitor = TestVisitor { visited: vec![] };
        walk_stmt(&mut visitor, &statement);
        assert_eq!(visitor.visited, vec!["visit_expression"]);
    }

    #[test]
    fn test_walk_function_declaration() {
        let declaration = node::FunctionDeclaration {
            id: NodeId::new(0),
            name: node::Expr::new(
                NodeId::new(0),
                node::ExprKind::Path(Box::new(node::Path::new(vec![Ident::new(
                    "my_fn",
                    Span::default(),
                )]))),
            ),
            parameters: vec![node::FunctionParameter {
                pattern: node::Pat::new(
                    NodeId::new(0),
                    node::PatKind::Identifier(Box::new(Ident::new("x", Span::default()))),
                ),
                type_annotation: None,
                span: Span::default(),
            }],
            guard: Some(node::Expr::new(
                NodeId::new(0),
                node::ExprKind::BinaryOp(Box::new(BinaryOpExpression {
                    op: BinaryOpKind::GreaterThanOrEqual,
                    lhs: node::Expr::new(
                        NodeId::new(0),
                        node::ExprKind::Path(Box::new(Path::new(vec![Ident::new(
                            "x",
                            Span::default(),
                        )]))),
                    ),
                    rhs: node::Expr::new(
                        NodeId::new(0),
                        node::ExprKind::Literal(Box::new(Literal::Integer(5))),
                    ),
                })),
            )),
            return_type_annotation: None,
            body: node::Block::default(),
            ..Default::default()
        };
        let mut visitor = TestVisitor { visited: vec![] };
        walk_fn_decl(&mut visitor, &declaration);
        assert_eq!(
            visitor.visited,
            vec![
                "visit_function_parameter",
                "visit_pattern",
                "visit_identifier",
                "visit_function_guard",
                "visit_expression",
                "visit_identifier",
                "visit_expression",
                "visit_function_body",
                "visit_block"
            ]
        );
    }
}
