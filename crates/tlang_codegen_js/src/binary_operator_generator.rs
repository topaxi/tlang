use tlang_ast::node as ast;
use tlang_hir::hir;

use crate::generator::CodegenJS;

#[derive(Debug, Clone, Copy, PartialEq)]
enum JSAssociativity {
    Left,
    Right,
}

#[derive(Debug, Clone, Copy)]
struct JSOperatorInfo {
    precedence: u8,
    associativity: JSAssociativity,
}

impl CodegenJS {
    pub(crate) fn generate_binary_op(
        &mut self,
        op: ast::BinaryOpKind,
        lhs: &hir::Expr,
        rhs: &hir::Expr,
        parent_op: Option<ast::BinaryOpKind>,
    ) {
        let needs_parentheses =
            parent_op.map_or(false, |parent| should_wrap_with_parentheses(op, parent));

        if needs_parentheses {
            self.push_char('(');
        }

        if let ast::BinaryOpKind::Pipeline = op {
            // If rhs was an identifier, we just pass lhs it as an argument to a function call.
            if let hir::ExprKind::Path(_) = &rhs.kind {
                self.generate_expr(rhs, None);
                self.push_char('(');
                self.generate_expr(lhs, None);
                self.push_char(')');
            // If rhs is a Call node and we prepend the lhs to the argument list.
            } else if let hir::ExprKind::Call(call_expr) = &rhs.kind {
                self.generate_expr(&call_expr.callee, None);
                self.push_char('(');

                // If we have a wildcard in the argument list, we instead replace the wildcard with the lhs.
                // Otherwise we prepend the lhs to the argument list.
                if call_expr.has_wildcard() {
                    for (i, arg) in call_expr.arguments.iter().enumerate() {
                        if i > 0 {
                            self.push_str(", ");
                        }

                        if let hir::ExprKind::Wildcard = arg.kind {
                            self.generate_expr(lhs, None);
                        } else {
                            self.generate_expr(arg, None);
                        }
                    }
                } else {
                    self.generate_expr(lhs, None);
                    for arg in &call_expr.arguments {
                        self.push_str(", ");
                        self.generate_expr(arg, None);
                    }
                };
                self.push_char(')');
            }
        } else {
            self.generate_expr(lhs, Some(op));
            self.generate_binary_operator_token(op);
            self.generate_expr(rhs, Some(op));
        }

        if needs_parentheses {
            self.push_char(')');
        }
    }

    fn generate_binary_operator_token(self: &mut CodegenJS, op: ast::BinaryOpKind) {
        match op {
            ast::BinaryOpKind::Assign => self.push_str(" = "),
            ast::BinaryOpKind::Add => self.push_str(" + "),
            ast::BinaryOpKind::Subtract => self.push_str(" - "),
            ast::BinaryOpKind::Multiply => self.push_str(" * "),
            ast::BinaryOpKind::Divide => self.push_str(" / "),
            ast::BinaryOpKind::Modulo => self.push_str(" % "),
            ast::BinaryOpKind::Exponentiation => self.push_str(" ** "),
            ast::BinaryOpKind::Equal => self.push_str(" === "),
            ast::BinaryOpKind::NotEqual => self.push_str(" !== "),
            ast::BinaryOpKind::LessThan => self.push_str(" < "),
            ast::BinaryOpKind::LessThanOrEqual => self.push_str(" <= "),
            ast::BinaryOpKind::GreaterThan => self.push_str(" > "),
            ast::BinaryOpKind::GreaterThanOrEqual => self.push_str(" >= "),
            ast::BinaryOpKind::And => self.push_str(" && "),
            ast::BinaryOpKind::Or => self.push_str(" || "),
            ast::BinaryOpKind::BitwiseOr => self.push_str(" | "),
            ast::BinaryOpKind::BitwiseAnd => self.push_str(" & "),
            ast::BinaryOpKind::BitwiseXor => self.push_str(" ^ "),
            ast::BinaryOpKind::Pipeline => {
                unreachable!("Pipeline operator does not exist yet in JS")
            }
        }
    }
}

fn should_wrap_with_parentheses(op: ast::BinaryOpKind, parent_op: ast::BinaryOpKind) -> bool {
    let op_info = map_operator_info(op);
    let parent_op_info = map_operator_info(parent_op);

    if op_info.precedence < parent_op_info.precedence {
        return true;
    }

    op_info.precedence == parent_op_info.precedence
        && op_info.associativity == JSAssociativity::Right
}

fn map_operator_info(op: ast::BinaryOpKind) -> JSOperatorInfo {
    match op {
        ast::BinaryOpKind::Assign => JSOperatorInfo {
            precedence: 1,
            associativity: JSAssociativity::Right,
        },
        ast::BinaryOpKind::Add | ast::BinaryOpKind::Subtract => JSOperatorInfo {
            precedence: 7,
            associativity: JSAssociativity::Left,
        },
        ast::BinaryOpKind::Multiply | ast::BinaryOpKind::Divide | ast::BinaryOpKind::Modulo => {
            JSOperatorInfo {
                precedence: 8,
                associativity: JSAssociativity::Left,
            }
        }
        ast::BinaryOpKind::Equal
        | ast::BinaryOpKind::NotEqual
        | ast::BinaryOpKind::LessThan
        | ast::BinaryOpKind::LessThanOrEqual
        | ast::BinaryOpKind::GreaterThan
        | ast::BinaryOpKind::GreaterThanOrEqual => JSOperatorInfo {
            precedence: 6,
            associativity: JSAssociativity::Left,
        },
        ast::BinaryOpKind::And => JSOperatorInfo {
            precedence: 4,
            associativity: JSAssociativity::Left,
        },
        ast::BinaryOpKind::Or => JSOperatorInfo {
            precedence: 3,
            associativity: JSAssociativity::Left,
        },
        ast::BinaryOpKind::BitwiseAnd
        | ast::BinaryOpKind::BitwiseOr
        | ast::BinaryOpKind::BitwiseXor => JSOperatorInfo {
            precedence: 9,
            associativity: JSAssociativity::Left,
        },
        ast::BinaryOpKind::Exponentiation => JSOperatorInfo {
            precedence: 10,
            associativity: JSAssociativity::Right,
        },
        ast::BinaryOpKind::Pipeline => unreachable!("Pipeline operator does not exist yet in JS"),
    }
}
