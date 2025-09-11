#[cfg(test)]
mod tests {
    use crate::hir;
    use crate::visit::{post_order_walk_expr, walk_expr};
    use crate::{PostOrderVisitor, Visitor};
    use tlang_ast::token::Literal;
    use tlang_span::HirIdAllocator;

    // Test visitor that records the order of visits
    #[derive(Default)]
    struct OrderRecordingVisitor {
        visit_order: Vec<String>,
    }

    impl<'hir> Visitor<'hir> for OrderRecordingVisitor {
        fn visit_expr(&mut self, expr: &'hir mut hir::Expr, ctx: &mut Self::Context) {
            self.visit_order.push(format!("pre-expr-{:?}", expr.hir_id));
            walk_expr(self, expr, ctx);
            self.visit_order
                .push(format!("post-expr-{:?}", expr.hir_id));
        }

        fn visit_literal(&mut self, literal: &'hir mut Literal, _ctx: &mut Self::Context) {
            self.visit_order.push(format!("literal-{:?}", literal));
        }
    }

    #[derive(Default)]
    struct PostOrderRecordingVisitor {
        visit_order: Vec<String>,
    }

    impl<'hir> PostOrderVisitor<'hir> for PostOrderRecordingVisitor {
        fn visit_expr(&mut self, expr: &'hir mut hir::Expr, ctx: &mut Self::Context) {
            post_order_walk_expr(self, expr, ctx);
            self.visit_order.push(format!("expr-{:?}", expr.hir_id));
        }

        fn visit_literal(&mut self, literal: &'hir mut Literal, _ctx: &mut Self::Context) {
            self.visit_order.push(format!("literal-{:?}", literal));
        }
    }

    fn create_binary_expr() -> hir::Expr {
        let mut allocator = HirIdAllocator::default();
        let span = tlang_span::Span::default();

        let left_literal = hir::Expr {
            kind: hir::ExprKind::Literal(Box::new(Literal::Integer(1))),
            hir_id: allocator.next_id(),
            span,
        };

        let right_literal = hir::Expr {
            kind: hir::ExprKind::Literal(Box::new(Literal::Integer(2))),
            hir_id: allocator.next_id(),
            span,
        };

        hir::Expr {
            kind: hir::ExprKind::Binary(
                hir::BinaryOpKind::Add,
                Box::new(left_literal),
                Box::new(right_literal),
            ),
            hir_id: allocator.next_id(),
            span,
        }
    }

    #[test]
    fn test_pre_order_visitor() {
        let mut expr = create_binary_expr();
        let mut visitor = OrderRecordingVisitor::default();
        visitor.visit_expr(&mut expr, &mut ());

        // Pre-order should visit parent before children
        let order = visitor.visit_order;
        assert!(order.len() >= 3);

        // Should start with parent expr
        assert!(order[0].starts_with("pre-expr"));

        // Should have literals visited after parent
        let literal_positions: Vec<_> = order
            .iter()
            .enumerate()
            .filter(|(_, s)| s.starts_with("literal"))
            .map(|(i, _)| i)
            .collect();
        assert_eq!(literal_positions.len(), 2);
    }

    #[test]
    fn test_post_order_visitor() {
        let mut expr = create_binary_expr();
        let parent_id = expr.hir_id;
        let mut visitor = PostOrderRecordingVisitor::default();
        visitor.visit_expr(&mut expr, &mut ());

        // Post-order should visit children before parent
        let order = visitor.visit_order;
        assert!(order.len() >= 3);

        // Should end with parent expr
        let parent_record = format!("expr-{:?}", parent_id);
        let parent_position = order.iter().position(|s| s == &parent_record).unwrap();

        // Literals should be visited before the parent expr
        let literal_positions: Vec<_> = order
            .iter()
            .enumerate()
            .filter(|(_, s)| s.starts_with("literal"))
            .map(|(i, _)| i)
            .collect();

        assert_eq!(literal_positions.len(), 2);
        assert!(literal_positions.iter().all(|&pos| pos < parent_position));
    }
}
