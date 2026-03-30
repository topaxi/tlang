use oxc_ast::ast::{BinaryOperator, LogicalOperator, UnaryOperator};
use tlang_ast::node as ast;
use tlang_hir as hir;

/// Map a HIR binary op to either a JS binary or logical operator.
/// Returns `Err(LogicalOperator)` for `&&`/`||`, `Ok(BinaryOperator)` for everything else.
pub(crate) fn map_binary_op(op: hir::BinaryOpKind) -> Result<BinaryOperator, LogicalOperator> {
    match op {
        hir::BinaryOpKind::Add => Ok(BinaryOperator::Addition),
        hir::BinaryOpKind::Sub => Ok(BinaryOperator::Subtraction),
        hir::BinaryOpKind::Mul => Ok(BinaryOperator::Multiplication),
        hir::BinaryOpKind::Div => Ok(BinaryOperator::Division),
        hir::BinaryOpKind::Mod => Ok(BinaryOperator::Remainder),
        hir::BinaryOpKind::Exp => Ok(BinaryOperator::Exponential),
        hir::BinaryOpKind::Eq => Ok(BinaryOperator::StrictEquality),
        hir::BinaryOpKind::NotEq => Ok(BinaryOperator::StrictInequality),
        hir::BinaryOpKind::Less => Ok(BinaryOperator::LessThan),
        hir::BinaryOpKind::LessEq => Ok(BinaryOperator::LessEqualThan),
        hir::BinaryOpKind::Greater => Ok(BinaryOperator::GreaterThan),
        hir::BinaryOpKind::GreaterEq => Ok(BinaryOperator::GreaterEqualThan),
        hir::BinaryOpKind::BitwiseOr => Ok(BinaryOperator::BitwiseOR),
        hir::BinaryOpKind::BitwiseAnd => Ok(BinaryOperator::BitwiseAnd),
        hir::BinaryOpKind::BitwiseXor => Ok(BinaryOperator::BitwiseXOR),
        hir::BinaryOpKind::And => Err(LogicalOperator::And),
        hir::BinaryOpKind::Or => Err(LogicalOperator::Or),
        // Assign is handled separately by the caller.
        hir::BinaryOpKind::Assign => {
            unreachable!("Assign should be handled as AssignmentExpression")
        }
    }
}

pub(crate) fn map_unary_op(op: &ast::UnaryOp) -> UnaryOperator {
    match op {
        ast::UnaryOp::Not => UnaryOperator::LogicalNot,
        ast::UnaryOp::Minus => UnaryOperator::UnaryNegation,
        ast::UnaryOp::BitwiseNot => UnaryOperator::BitwiseNot,
        ast::UnaryOp::Spread | ast::UnaryOp::Rest => {
            unreachable!("Spread/Rest are not unary operators")
        }
    }
}
