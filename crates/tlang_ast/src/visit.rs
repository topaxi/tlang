use crate::node;
use tlang_span::NodeId;

pub trait Visitor<'ast>: Sized {
    type Context;

    #[allow(unused_variables)]
    fn enter_scope(&mut self, node_id: NodeId, ctx: &mut Self::Context) {}
    #[allow(unused_variables)]
    fn leave_scope(&mut self, node_id: NodeId, ctx: &mut Self::Context) {}
    fn visit_module(&mut self, module: &'ast node::Module, ctx: &mut Self::Context) {
        walk_module(self, module, ctx);
    }

    fn visit_block(
        &mut self,
        statements: &'ast [node::Stmt],
        expression: &'ast Option<node::Expr>,
        ctx: &mut Self::Context,
    ) {
        walk_block(self, statements, expression, ctx);
    }

    #[allow(unused_variables)]
    fn visit_ident(&mut self, _ident: &'ast node::Ident, ctx: &mut Self::Context) {}

    fn visit_pat(&mut self, pattern: &'ast node::Pat, ctx: &mut Self::Context) {
        walk_pat(self, pattern, ctx);
    }

    fn visit_stmt(&mut self, statement: &'ast node::Stmt, ctx: &mut Self::Context) {
        walk_stmt(self, statement, ctx);
    }

    fn visit_struct_decl(&mut self, decl: &'ast node::StructDeclaration, ctx: &mut Self::Context) {
        self.visit_ident(&decl.name, ctx);

        for node::StructField { name, ty, .. } in &decl.fields {
            self.visit_ident(name, ctx);
            self.visit_ty(ty, ctx);
        }
    }

    fn visit_enum_decl(&mut self, decl: &'ast node::EnumDeclaration, ctx: &mut Self::Context) {
        walk_enum_decl(self, decl, ctx);
    }

    fn visit_fn_decls(&mut self, declarations: &'ast [node::FunctionDeclaration], ctx: &mut Self::Context) {
        for declaration in declarations {
            walk_fn_decl(self, declaration, ctx);
        }
    }

    fn visit_fn_decl(&mut self, declaration: &'ast node::FunctionDeclaration, ctx: &mut Self::Context) {
        walk_fn_decl(self, declaration, ctx);
    }

    fn visit_expr(&mut self, expression: &'ast node::Expr, ctx: &mut Self::Context) {
        walk_expr(self, expression, ctx);
    }

    fn visit_else_clause(&mut self, clause: &'ast node::ElseClause, ctx: &mut Self::Context) {
        if let Some(ref condition) = clause.condition {
            walk_expr(self, condition, ctx);
        }

        walk_block(
            self,
            &clause.consequence.statements,
            &clause.consequence.expression,
            ctx,
        );
    }

    fn visit_fn_param(&mut self, parameter: &'ast node::FunctionParameter, ctx: &mut Self::Context) {
        walk_fn_param(self, parameter, ctx);
    }

    fn visit_fn_guard(&mut self, guard: &'ast node::Expr, ctx: &mut Self::Context) {
        walk_expr(self, guard, ctx);
    }

    fn visit_ty(&mut self, ty: &'ast node::Ty, ctx: &mut Self::Context) {
        walk_ty(self, ty, ctx);
    }

    fn visit_fn_ret_ty(&mut self, annotation: &'ast node::Ty, ctx: &mut Self::Context) {
        walk_ty(self, annotation, ctx);
    }

    fn visit_fn_body(&mut self, body: &'ast node::Block, ctx: &mut Self::Context) {
        walk_fn_body(self, body, ctx);
    }

    fn visit_variant(&mut self, variant: &'ast node::EnumVariant, ctx: &mut Self::Context) {
        walk_variant(self, variant, ctx);
    }

    fn visit_path(&mut self, path: &'ast node::Path, ctx: &mut Self::Context) {
        walk_path(self, path, ctx);
    }
}

pub fn walk_module<'ast, V: Visitor<'ast>>(visitor: &mut V, module: &'ast node::Module, ctx: &mut V::Context) {
    for statement in &module.statements {
        visitor.visit_stmt(statement, ctx);
    }
}

pub fn walk_path<'ast, V: Visitor<'ast>>(visitor: &mut V, path: &'ast node::Path, ctx: &mut V::Context) {
    for segment in &path.segments {
        visitor.visit_ident(segment, ctx);
    }
}

pub fn walk_pat<'ast, V: Visitor<'ast>>(visitor: &mut V, pattern: &'ast node::Pat, ctx: &mut V::Context) {
    match &pattern.kind {
        node::PatKind::Identifier(ident_pattern) => visitor.visit_ident(ident_pattern, ctx),
        node::PatKind::Rest(pattern) => visitor.visit_pat(pattern, ctx),
        node::PatKind::List(patterns) => {
            for pattern in patterns {
                visitor.visit_pat(pattern, ctx);
            }
        }
        node::PatKind::Enum(enum_pattern) => {
            visitor.visit_path(&enum_pattern.path, ctx);

            for (ident, pat) in &enum_pattern.elements {
                visitor.visit_ident(ident, ctx);
                visitor.visit_pat(pat, ctx);
            }
        }
        node::PatKind::Literal(_) => {}
        node::PatKind::_Self => {}
        node::PatKind::Wildcard => {}
        node::PatKind::None => {}
    }
}

pub fn walk_ty<'ast, V: Visitor<'ast>>(visitor: &mut V, ty: &'ast node::Ty, ctx: &mut V::Context) {
    for ty in &ty.parameters {
        visitor.visit_ty(ty, ctx);
    }
}

pub fn walk_block<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    statements: &'ast [node::Stmt],
    expression: &'ast Option<node::Expr>,
    ctx: &mut V::Context,
) {
    for statement in statements {
        visitor.visit_stmt(statement, ctx);
    }
    if let Some(expression) = expression {
        visitor.visit_expr(expression, ctx);
    }
}

pub fn walk_stmt<'ast, V: Visitor<'ast>>(visitor: &mut V, statement: &'ast node::Stmt, ctx: &mut V::Context) {
    match &statement.kind {
        node::StmtKind::None => {}
        node::StmtKind::Expr(expr) => {
            visitor.visit_expr(expr, ctx);
        }
        node::StmtKind::Let(let_decl) => {
            visitor.visit_pat(&let_decl.pattern, ctx);
            visitor.visit_expr(&let_decl.expression, ctx);

            if let Some(ty) = &let_decl.type_annotation {
                visitor.visit_ty(ty, ctx);
            }
        }
        node::StmtKind::FunctionDeclaration(declaration) => {
            visitor.visit_fn_decl(declaration, ctx);
        }
        node::StmtKind::FunctionDeclarations(declarations) => {
            visitor.visit_fn_decls(declarations, ctx);
        }
        node::StmtKind::Return(expr) => {
            if let Some(expr) = expr.as_ref() {
                visitor.visit_expr(expr, ctx);
            }
        }
        node::StmtKind::StructDeclaration(decl) => visitor.visit_struct_decl(decl, ctx),
        node::StmtKind::EnumDeclaration(decl) => visitor.visit_enum_decl(decl, ctx),
    }
}

pub fn walk_enum_decl<'ast, V: Visitor<'ast>>(visitor: &mut V, decl: &'ast node::EnumDeclaration, ctx: &mut V::Context) {
    visitor.visit_ident(&decl.name, ctx);

    for variant in &decl.variants {
        visitor.visit_variant(variant, ctx);
    }
}

pub fn walk_variant<'ast, V: Visitor<'ast>>(visitor: &mut V, variant: &'ast node::EnumVariant, ctx: &mut V::Context) {
    visitor.visit_ident(&variant.name, ctx);

    for node::StructField { name, ty, .. } in &variant.parameters {
        visitor.visit_ident(name, ctx);
        visitor.visit_ty(ty, ctx);
    }
}

pub fn walk_fn_param<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    parameter: &'ast node::FunctionParameter,
    ctx: &mut V::Context,
) {
    visitor.visit_pat(&parameter.pattern, ctx);

    if let Some(ref type_annotation) = parameter.type_annotation {
        visitor.visit_ty(type_annotation, ctx);
    }
}

pub fn walk_fn_body<'ast, V: Visitor<'ast>>(visitor: &mut V, body: &'ast node::Block, ctx: &mut V::Context) {
    visitor.visit_block(&body.statements, &body.expression, ctx);
}

pub fn walk_fn_decl<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    declaration: &'ast node::FunctionDeclaration,
    ctx: &mut V::Context,
) {
    for parameter in &declaration.parameters {
        visitor.visit_fn_param(parameter, ctx);
    }
    if let Some(ref guard) = declaration.guard {
        visitor.visit_fn_guard(guard, ctx);
    }
    if let Some(ref return_type_annotation) = declaration.return_type_annotation {
        visitor.visit_fn_ret_ty(return_type_annotation, ctx);
    }
    visitor.visit_fn_body(&declaration.body, ctx);
}

pub fn walk_expr<'ast, V: Visitor<'ast>>(visitor: &mut V, expression: &'ast node::Expr, ctx: &mut V::Context) {
    match &expression.kind {
        node::ExprKind::None => {}
        node::ExprKind::Block(block) | node::ExprKind::Loop(block) => {
            visitor.visit_block(&block.statements, &block.expression, ctx);
        }
        node::ExprKind::Break(Some(expr)) => {
            visitor.visit_expr(expr, ctx);
        }
        node::ExprKind::Break(_) => {}
        node::ExprKind::ForLoop(for_loop) => {
            if let Some((pat, expr)) = &for_loop.acc {
                visitor.visit_pat(pat, ctx);
                visitor.visit_expr(expr, ctx);
            }

            visitor.visit_pat(&for_loop.pat, ctx);
            visitor.visit_expr(&for_loop.iter, ctx);
            visitor.visit_block(&for_loop.block.statements, &for_loop.block.expression, ctx);
        }
        node::ExprKind::BinaryOp(binary_op_expression) => {
            visitor.visit_expr(&binary_op_expression.lhs, ctx);
            visitor.visit_expr(&binary_op_expression.rhs, ctx);
        }
        node::ExprKind::UnaryOp(_, expression) => {
            visitor.visit_expr(expression, ctx);
        }
        node::ExprKind::FieldExpression(field_expr) => {
            visitor.visit_expr(&field_expr.base, ctx);
            visitor.visit_ident(&field_expr.field, ctx);
        }
        node::ExprKind::Call(call_expr) | node::ExprKind::RecursiveCall(call_expr) => {
            for argument in &call_expr.arguments {
                visitor.visit_expr(argument, ctx);
            }
            visitor.visit_expr(&call_expr.callee, ctx);
        }
        node::ExprKind::Dict(kvs) => {
            for (key, value) in kvs {
                visitor.visit_expr(key, ctx);
                visitor.visit_expr(value, ctx);
            }
        }
        node::ExprKind::FunctionExpression(decl) => {
            visitor.visit_fn_decl(decl, ctx);
        }
        node::ExprKind::IndexExpression(index_expr) => {
            visitor.visit_expr(&index_expr.base, ctx);
            visitor.visit_expr(&index_expr.index, ctx);
        }
        node::ExprKind::Let(pattern, expr) => {
            visitor.visit_pat(pattern, ctx);
            visitor.visit_expr(expr, ctx);
        }
        node::ExprKind::Literal(_) => {}
        node::ExprKind::List(exprs) => {
            for expr in exprs {
                visitor.visit_expr(expr, ctx);
            }
        }
        node::ExprKind::IfElse(if_else_expr) => {
            visitor.visit_expr(&if_else_expr.condition, ctx);
            visitor.visit_block(
                &if_else_expr.then_branch.statements,
                &if_else_expr.then_branch.expression,
                ctx,
            );

            for else_branch in &if_else_expr.else_branches {
                visitor.visit_else_clause(else_branch, ctx);
            }
        }
        node::ExprKind::Match(match_expr) => {
            visitor.visit_expr(&match_expr.expression, ctx);

            for arm in &match_expr.arms {
                visitor.visit_pat(&arm.pattern, ctx);
                visitor.visit_expr(&arm.expression, ctx);
            }
        }
        node::ExprKind::Path(path) => {
            visitor.visit_path(path, ctx);
        }
        node::ExprKind::Range(range_expr) => {
            visitor.visit_expr(&range_expr.start, ctx);
            visitor.visit_expr(&range_expr.end, ctx);
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

    use tlang_span::{NodeId, Span};

    use super::*;
    use crate::token::Literal;

    struct TestVisitor {
        visited: Vec<&'static str>,
    }

    impl<'ast> Visitor<'ast> for TestVisitor {
        type Context = ();

        fn visit_module(&mut self, module: &'ast node::Module, ctx: &mut Self::Context) {
            self.visited.push("visit_module");
            walk_module(self, module, ctx);
        }
        fn visit_ident(&mut self, _ident: &'ast node::Ident, _ctx: &mut Self::Context) {
            self.visited.push("visit_identifier");
        }
        fn visit_block(
            &mut self,
            statements: &'ast [node::Stmt],
            expression: &'ast Option<node::Expr>,
            ctx: &mut Self::Context,
        ) {
            self.visited.push("visit_block");
            walk_block(self, statements, expression, ctx);
        }
        fn visit_stmt(&mut self, statement: &'ast node::Stmt, ctx: &mut Self::Context) {
            self.visited.push("visit_statement");
            walk_stmt(self, statement, ctx);
        }
        fn visit_fn_decl(&mut self, declaration: &'ast node::FunctionDeclaration, ctx: &mut Self::Context) {
            self.visited.push("visit_function_declaration");
            walk_fn_decl(self, declaration, ctx);
        }
        fn visit_expr(&mut self, expression: &'ast node::Expr, ctx: &mut Self::Context) {
            self.visited.push("visit_expression");
            walk_expr(self, expression, ctx);
        }
        fn visit_pat(&mut self, pattern: &'ast node::Pat, ctx: &mut Self::Context) {
            self.visited.push("visit_pattern");
            walk_pat(self, pattern, ctx);
        }
        fn visit_fn_param(&mut self, parameter: &'ast node::FunctionParameter, ctx: &mut Self::Context) {
            self.visited.push("visit_function_parameter");
            walk_fn_param(self, parameter, ctx);
        }
        fn visit_fn_guard(&mut self, guard: &'ast node::Expr, ctx: &mut Self::Context) {
            self.visited.push("visit_function_guard");
            walk_expr(self, guard, ctx);
        }
        fn visit_fn_ret_ty(&mut self, _annotation: &'ast node::Ty, _ctx: &mut Self::Context) {
            self.visited.push("visit_function_return_type_annotation");
        }
        fn visit_fn_body(&mut self, body: &'ast node::Block, ctx: &mut Self::Context) {
            self.visited.push("visit_function_body");
            walk_fn_body(self, body, ctx);
        }
    }

    #[test]
    fn test_walk_module() {
        let module = node::Module::new(
            NodeId::new(1),
            vec![node::Stmt::new(NodeId::new(2), node::StmtKind::None)],
            Span::default(),
        );
        let mut visitor = TestVisitor { visited: vec![] };
        let mut ctx = ();
        walk_module(&mut visitor, &module, &mut ctx);
        assert_eq!(visitor.visited, vec!["visit_statement"]);
    }

    #[test]
    fn test_walk_block() {
        let statements = vec![node::Stmt::new(NodeId::new(1), node::StmtKind::None)];
        let expression = Some(node::Expr::new(NodeId::new(2), node::ExprKind::None));
        let mut visitor = TestVisitor { visited: vec![] };
        let mut ctx = ();
        walk_block(&mut visitor, &statements, &expression, &mut ctx);
        assert_eq!(visitor.visited, vec!["visit_statement", "visit_expression"]);
    }

    #[test]
    fn test_walk_statement() {
        let statement = node::Stmt::new(
            NodeId::new(1),
            node::StmtKind::Expr(Box::new(node::Expr::new(
                NodeId::new(2),
                node::ExprKind::None,
            ))),
        );
        let mut visitor = TestVisitor { visited: vec![] };
        let mut ctx = ();
        walk_stmt(&mut visitor, &statement, &mut ctx);
        assert_eq!(visitor.visited, vec!["visit_expression"]);
    }

    #[test]
    fn test_walk_function_declaration() {
        let declaration = node::FunctionDeclaration {
            id: NodeId::new(1),
            name: node::Expr::new(
                NodeId::new(2),
                node::ExprKind::Path(Box::new(node::Path::new(vec![Ident::new(
                    "my_fn",
                    Span::default(),
                )]))),
            ),
            parameters: vec![node::FunctionParameter {
                pattern: node::Pat::new(
                    NodeId::new(3),
                    node::PatKind::Identifier(Box::new(Ident::new("x", Span::default()))),
                ),
                type_annotation: None,
                span: Span::default(),
            }],
            guard: Some(node::Expr::new(
                NodeId::new(4),
                node::ExprKind::BinaryOp(Box::new(BinaryOpExpression {
                    op: BinaryOpKind::GreaterThanOrEqual,
                    lhs: node::Expr::new(
                        NodeId::new(5),
                        node::ExprKind::Path(Box::new(Path::new(vec![Ident::new(
                            "x",
                            Span::default(),
                        )]))),
                    ),
                    rhs: node::Expr::new(
                        NodeId::new(6),
                        node::ExprKind::Literal(Box::new(Literal::Integer(5))),
                    ),
                })),
            )),
            return_type_annotation: None,
            body: node::Block::new(NodeId::new(7), vec![], None),
            leading_comments: vec![],
            trailing_comments: vec![],
            span: Span::default(),
        };
        let mut visitor = TestVisitor { visited: vec![] };
        let mut ctx = ();
        walk_fn_decl(&mut visitor, &declaration, &mut ctx);
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
