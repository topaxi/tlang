use tlang_ast::node::{BinaryOpKind, Expr, ExprKind};

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
        op: &BinaryOpKind,
        lhs: &Expr,
        rhs: &Expr,
        parent_op: Option<&BinaryOpKind>,
    ) {
        let needs_parentheses =
            parent_op.map_or(false, |parent| should_wrap_with_parentheses(op, parent));

        if needs_parentheses {
            self.push_char('(');
        }

        if let BinaryOpKind::Pipeline = op {
            // If rhs was an identifier, we just pass lhs it as an argument to a function call.
            if let ExprKind::Path(_) = rhs.kind {
                self.generate_expr(rhs, None);
                self.push_char('(');
                self.generate_expr(lhs, None);
                self.push_char(')');
            // If rhs is a Call node and we prepend the lhs to the argument list.
            } else if let ExprKind::Call(call_expr) = &rhs.kind {
                self.generate_expr(&call_expr.callee, None);
                self.push_char('(');

                // If we have a wildcard in the argument list, we instead replace the wildcard with the lhs.
                // Otherwise we prepend the lhs to the argument list.
                let has_wildcard = call_expr.arguments.iter().any(|arg| arg.is_wildcard());
                if has_wildcard {
                    for (i, arg) in call_expr.arguments.iter().enumerate() {
                        if i > 0 {
                            self.push_str(", ");
                        }

                        if let ExprKind::Wildcard = arg.kind {
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

    fn generate_binary_operator_token(self: &mut CodegenJS, op: &BinaryOpKind) {
        match op {
            BinaryOpKind::Add => self.push_str(" + "),
            BinaryOpKind::Subtract => self.push_str(" - "),
            BinaryOpKind::Multiply => self.push_str(" * "),
            BinaryOpKind::Divide => self.push_str(" / "),
            BinaryOpKind::Modulo => self.push_str(" % "),
            BinaryOpKind::Exponentiation => self.push_str(" ** "),
            BinaryOpKind::Equal => self.push_str(" === "),
            BinaryOpKind::NotEqual => self.push_str(" !== "),
            BinaryOpKind::LessThan => self.push_str(" < "),
            BinaryOpKind::LessThanOrEqual => self.push_str(" <= "),
            BinaryOpKind::GreaterThan => self.push_str(" > "),
            BinaryOpKind::GreaterThanOrEqual => self.push_str(" >= "),
            BinaryOpKind::And => self.push_str(" && "),
            BinaryOpKind::Or => self.push_str(" || "),
            BinaryOpKind::BitwiseOr => self.push_str(" | "),
            BinaryOpKind::BitwiseAnd => self.push_str(" & "),
            BinaryOpKind::BitwiseXor => self.push_str(" ^ "),
            BinaryOpKind::Pipeline => unreachable!("Pipeline operator does not exist yet in JS"),
        }
    }
}

fn should_wrap_with_parentheses(op: &BinaryOpKind, parent_op: &BinaryOpKind) -> bool {
    let op_info = map_operator_info(op);
    let parent_op_info = map_operator_info(parent_op);

    if op_info.precedence < parent_op_info.precedence {
        return true;
    }

    op_info.precedence == parent_op_info.precedence
        && op_info.associativity == JSAssociativity::Right
}

fn map_operator_info(op: &BinaryOpKind) -> JSOperatorInfo {
    match op {
        BinaryOpKind::Add | BinaryOpKind::Subtract => JSOperatorInfo {
            precedence: 6,
            associativity: JSAssociativity::Left,
        },
        BinaryOpKind::Multiply | BinaryOpKind::Divide | BinaryOpKind::Modulo => JSOperatorInfo {
            precedence: 7,
            associativity: JSAssociativity::Left,
        },
        BinaryOpKind::Equal
        | BinaryOpKind::NotEqual
        | BinaryOpKind::LessThan
        | BinaryOpKind::LessThanOrEqual
        | BinaryOpKind::GreaterThan
        | BinaryOpKind::GreaterThanOrEqual => JSOperatorInfo {
            precedence: 5,
            associativity: JSAssociativity::Left,
        },
        BinaryOpKind::And => JSOperatorInfo {
            precedence: 3,
            associativity: JSAssociativity::Left,
        },
        BinaryOpKind::Or => JSOperatorInfo {
            precedence: 2,
            associativity: JSAssociativity::Left,
        },
        BinaryOpKind::BitwiseAnd | BinaryOpKind::BitwiseOr | BinaryOpKind::BitwiseXor => {
            JSOperatorInfo {
                precedence: 8,
                associativity: JSAssociativity::Left,
            }
        }
        BinaryOpKind::Exponentiation => JSOperatorInfo {
            precedence: 9,
            associativity: JSAssociativity::Right,
        },
        BinaryOpKind::Pipeline => unreachable!("Pipeline operator does not exist yet in JS"),
    }
}
