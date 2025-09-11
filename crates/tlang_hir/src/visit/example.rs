use crate::visit::post_order_walk_expr;
/// Example usage of the PostOrderVisitor trait
///
/// This example demonstrates how to use the post-order visitor to process
/// HIR nodes in post-order traversal, where children are visited before their parents.
use crate::{PostOrderVisitor, hir};

pub struct ExpressionCounter {
    expression_count: usize,
    literal_count: usize,
}

impl ExpressionCounter {
    pub fn new() -> Self {
        Self {
            expression_count: 0,
            literal_count: 0,
        }
    }

    pub fn expression_count(&self) -> usize {
        self.expression_count
    }

    pub fn literal_count(&self) -> usize {
        self.literal_count
    }
}

impl<'hir> PostOrderVisitor<'hir> for ExpressionCounter {
    fn visit_expr(&mut self, expr: &'hir mut hir::Expr, ctx: &mut Self::Context) {
        // First visit all children expressions
        post_order_walk_expr(self, expr, ctx);

        // Then count this expression (post-order processing)
        self.expression_count += 1;

        println!(
            "Processed expression #{}: {:?}",
            self.expression_count, expr.kind
        );
    }

    fn visit_literal(
        &mut self,
        literal: &'hir mut tlang_ast::token::Literal,
        _ctx: &mut Self::Context,
    ) {
        self.literal_count += 1;
        println!("Found literal #{}: {:?}", self.literal_count, literal);
    }
}

#[cfg(test)]
mod example_tests {
    use super::*;
    use tlang_ast::token::Literal;
    use tlang_span::HirIdAllocator;

    #[test]
    fn test_expression_counter() {
        let mut allocator = HirIdAllocator::default();
        let span = tlang_span::Span::default();

        // Create a simple binary expression: 1 + 2
        let mut expr = hir::Expr {
            kind: hir::ExprKind::Binary(
                hir::BinaryOpKind::Add,
                Box::new(hir::Expr {
                    kind: hir::ExprKind::Literal(Box::new(Literal::Integer(1))),
                    hir_id: allocator.next_id(),
                    span,
                }),
                Box::new(hir::Expr {
                    kind: hir::ExprKind::Literal(Box::new(Literal::Integer(2))),
                    hir_id: allocator.next_id(),
                    span,
                }),
            ),
            hir_id: allocator.next_id(),
            span,
        };

        let mut counter = ExpressionCounter::new();
        counter.visit_expr(&mut expr, &mut ());

        // In post-order: literals are processed first, then the binary expression
        assert_eq!(counter.expression_count(), 3); // 2 literals + 1 binary
        assert_eq!(counter.literal_count(), 2);
    }
}
