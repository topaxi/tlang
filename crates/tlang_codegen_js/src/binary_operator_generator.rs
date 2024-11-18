use tlang_hir::hir;

use crate::generator::CodegenJS;

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum JSAssociativity {
    Left,
    Right,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct JSOperatorInfo {
    pub precedence: u8,
    pub associativity: JSAssociativity,
}

impl CodegenJS {
    pub(crate) fn generate_binary_op(
        &mut self,
        op: hir::BinaryOpKind,
        lhs: &hir::Expr,
        rhs: &hir::Expr,
        parent_op: Option<hir::BinaryOpKind>,
    ) {
        let needs_parentheses = parent_op.is_some_and(|parent| {
            should_wrap_with_parentheses(map_operator_info(parent), map_operator_info(op))
        });

        if needs_parentheses {
            self.push_char('(');
        }

        self.generate_expr(lhs, Some(op));
        self.generate_binary_operator_token(op);
        self.generate_expr(rhs, Some(op));

        if needs_parentheses {
            self.push_char(')');
        }
    }

    fn generate_binary_operator_token(self: &mut CodegenJS, op: hir::BinaryOpKind) {
        match op {
            hir::BinaryOpKind::Assign => self.push_str(" = "),
            hir::BinaryOpKind::Add => self.push_str(" + "),
            hir::BinaryOpKind::Sub => self.push_str(" - "),
            hir::BinaryOpKind::Mul => self.push_str(" * "),
            hir::BinaryOpKind::Div => self.push_str(" / "),
            hir::BinaryOpKind::Mod => self.push_str(" % "),
            hir::BinaryOpKind::Exp => self.push_str(" ** "),
            hir::BinaryOpKind::Eq => self.push_str(" === "),
            hir::BinaryOpKind::NotEq => self.push_str(" !== "),
            hir::BinaryOpKind::Less => self.push_str(" < "),
            hir::BinaryOpKind::LessEq => self.push_str(" <= "),
            hir::BinaryOpKind::Greater => self.push_str(" > "),
            hir::BinaryOpKind::GreaterEq => self.push_str(" >= "),
            hir::BinaryOpKind::And => self.push_str(" && "),
            hir::BinaryOpKind::Or => self.push_str(" || "),
            hir::BinaryOpKind::BitwiseOr => self.push_str(" | "),
            hir::BinaryOpKind::BitwiseAnd => self.push_str(" & "),
            hir::BinaryOpKind::BitwiseXor => self.push_str(" ^ "),
        }
    }
}

pub(crate) fn should_wrap_with_parentheses(
    parent_op_info: JSOperatorInfo,
    op_info: JSOperatorInfo,
) -> bool {
    op_info.precedence < parent_op_info.precedence
        || (op_info.precedence == parent_op_info.precedence
            && op_info.associativity == JSAssociativity::Right)
}

pub(crate) fn map_operator_info(op: hir::BinaryOpKind) -> JSOperatorInfo {
    match op {
        hir::BinaryOpKind::Assign => JSOperatorInfo {
            precedence: 1,
            associativity: JSAssociativity::Right,
        },
        hir::BinaryOpKind::Add | hir::BinaryOpKind::Sub => JSOperatorInfo {
            precedence: 7,
            associativity: JSAssociativity::Left,
        },
        hir::BinaryOpKind::Mul | hir::BinaryOpKind::Div | hir::BinaryOpKind::Mod => {
            JSOperatorInfo {
                precedence: 8,
                associativity: JSAssociativity::Left,
            }
        }
        hir::BinaryOpKind::Eq
        | hir::BinaryOpKind::NotEq
        | hir::BinaryOpKind::Less
        | hir::BinaryOpKind::LessEq
        | hir::BinaryOpKind::Greater
        | hir::BinaryOpKind::GreaterEq => JSOperatorInfo {
            precedence: 6,
            associativity: JSAssociativity::Left,
        },
        hir::BinaryOpKind::And => JSOperatorInfo {
            precedence: 4,
            associativity: JSAssociativity::Left,
        },
        hir::BinaryOpKind::Or => JSOperatorInfo {
            precedence: 3,
            associativity: JSAssociativity::Left,
        },
        hir::BinaryOpKind::BitwiseAnd
        | hir::BinaryOpKind::BitwiseOr
        | hir::BinaryOpKind::BitwiseXor => JSOperatorInfo {
            precedence: 9,
            associativity: JSAssociativity::Left,
        },
        hir::BinaryOpKind::Exp => JSOperatorInfo {
            precedence: 10,
            associativity: JSAssociativity::Right,
        },
    }
}
