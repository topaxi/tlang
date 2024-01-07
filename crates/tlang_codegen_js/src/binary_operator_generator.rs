use tlang_ast::node::{AstNode, BinaryOp, Node};

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

pub fn generate_binary_op(
    codegen: &mut CodegenJS,
    op: &BinaryOp,
    lhs: &Node,
    rhs: &Node,
    parent_op: Option<&BinaryOp>,
) {
    let needs_parentheses =
        parent_op.map_or(false, |parent| should_wrap_with_parentheses(op, parent));

    if needs_parentheses {
        codegen.push_char('(');
    }

    if let BinaryOp::Pipeline = op {
        // If rhs was an identifier, we just pass lhs it as an argument to a function call.
        if let AstNode::Identifier(_) = rhs.ast_node {
            codegen.generate_node(rhs, None);
            codegen.push_char('(');
            codegen.generate_node(lhs, None);
            codegen.push_char(')');
        // If rhs is a Call node and we prepend the lhs to the argument list.
        } else if let AstNode::Call {
            function,
            arguments,
        } = &rhs.ast_node
        {
            codegen.generate_node(function, None);
            codegen.push_char('(');

            // If we have a wildcard in the argument list, we instead replace the wildcard with the lhs.
            // Otherwise we prepend the lhs to the argument list.
            let has_wildcard = arguments
                .iter()
                .any(|arg| matches!(arg.ast_node, AstNode::Wildcard));
            if has_wildcard {
                for (i, arg) in arguments.iter().enumerate() {
                    if i > 0 {
                        codegen.push_str(", ");
                    }

                    if let AstNode::Wildcard = arg.ast_node {
                        codegen.generate_node(lhs, None);
                    } else {
                        codegen.generate_node(arg, None);
                    }
                }
            } else {
                codegen.generate_node(lhs, None);
                for arg in arguments.iter() {
                    codegen.push_str(", ");
                    codegen.generate_node(arg, None);
                }
            };
            codegen.push_char(')');
        }
    } else {
        codegen.generate_node(lhs, Some(op));
        generate_binary_operator_token(codegen, op);
        codegen.generate_node(rhs, Some(op));
    }

    if needs_parentheses {
        codegen.push_char(')');
    }
}

fn should_wrap_with_parentheses(op: &BinaryOp, parent_op: &BinaryOp) -> bool {
    let op_info = map_operator_info(op);
    let parent_op_info = map_operator_info(parent_op);

    if op_info.precedence < parent_op_info.precedence {
        return true;
    }

    op_info.precedence == parent_op_info.precedence
        && op_info.associativity == JSAssociativity::Right
}

fn generate_binary_operator_token(codegen: &mut CodegenJS, op: &BinaryOp) {
    match op {
        BinaryOp::Add => codegen.push_str(" + "),
        BinaryOp::Subtract => codegen.push_str(" - "),
        BinaryOp::Multiply => codegen.push_str(" * "),
        BinaryOp::Divide => codegen.push_str(" / "),
        BinaryOp::Modulo => codegen.push_str(" % "),
        BinaryOp::Exponentiation => codegen.push_str(" ** "),
        BinaryOp::Equal => codegen.push_str(" === "),
        BinaryOp::NotEqual => codegen.push_str(" !== "),
        BinaryOp::LessThan => codegen.push_str(" < "),
        BinaryOp::LessThanOrEqual => codegen.push_str(" <= "),
        BinaryOp::GreaterThan => codegen.push_str(" > "),
        BinaryOp::GreaterThanOrEqual => codegen.push_str(" >= "),
        BinaryOp::And => codegen.push_str(" && "),
        BinaryOp::Or => codegen.push_str(" || "),
        BinaryOp::BitwiseOr => codegen.push_str(" | "),
        BinaryOp::BitwiseAnd => codegen.push_str(" & "),
        BinaryOp::BitwiseXor => codegen.push_str(" ^ "),
        BinaryOp::Pipeline => unimplemented!("Pipeline does not neatly map to a JS operator."),
    }
}

fn map_operator_info(op: &BinaryOp) -> JSOperatorInfo {
    match op {
        BinaryOp::Add | BinaryOp::Subtract => JSOperatorInfo {
            precedence: 6,
            associativity: JSAssociativity::Left,
        },
        BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Modulo => JSOperatorInfo {
            precedence: 7,
            associativity: JSAssociativity::Left,
        },
        BinaryOp::Equal
        | BinaryOp::NotEqual
        | BinaryOp::LessThan
        | BinaryOp::LessThanOrEqual
        | BinaryOp::GreaterThan
        | BinaryOp::GreaterThanOrEqual => JSOperatorInfo {
            precedence: 5,
            associativity: JSAssociativity::Left,
        },
        BinaryOp::And => JSOperatorInfo {
            precedence: 3,
            associativity: JSAssociativity::Left,
        },
        BinaryOp::Or => JSOperatorInfo {
            precedence: 2,
            associativity: JSAssociativity::Left,
        },
        BinaryOp::BitwiseAnd | BinaryOp::BitwiseOr | BinaryOp::BitwiseXor => JSOperatorInfo {
            precedence: 8,
            associativity: JSAssociativity::Left,
        },
        BinaryOp::Exponentiation => JSOperatorInfo {
            precedence: 9,
            associativity: JSAssociativity::Right,
        },
        BinaryOp::Pipeline => unreachable!("Pipeline operator does not exist yet in JS"),
    }
}
