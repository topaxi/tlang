use oxc_ast::AstBuilder;
use oxc_ast::ast::*;
use oxc_span::SPAN;

/// Post-codegen pass that walks the OXC AST and simplifies boolean-return
/// patterns in if/else statements.
///
/// Transforms:
/// ```js
/// if (cond) { return true; } else { return false; }
/// ```
/// Into:
/// ```js
/// return cond;
/// ```
///
/// And the negated form:
/// ```js
/// if (cond) { return false; } else { return true; }
/// ```
/// Into:
/// ```js
/// return !cond;
/// ```
///
/// This operates as a safety net after codegen, catching boolean-return
/// patterns regardless of HIR origin (IfElse nodes, Match nodes, etc.).
pub fn simplify_boolean_returns<'a>(program: &mut Program<'a>, ast: &AstBuilder<'a>) {
    for stmt in &mut program.body {
        simplify_in_stmt(stmt, ast);
    }
}

/// Recursively walk a statement, simplifying boolean-return patterns.
fn simplify_in_stmt<'a>(stmt: &mut Statement<'a>, ast: &AstBuilder<'a>) {
    match stmt {
        Statement::BlockStatement(block) => {
            simplify_in_stmts(&mut block.body, ast);
        }
        Statement::IfStatement(if_stmt) => {
            // First, recurse into the branches.
            simplify_in_stmt(&mut if_stmt.consequent, ast);
            if let Some(ref mut alt) = if_stmt.alternate {
                simplify_in_stmt(alt, ast);
            }

            // Then, try to simplify this if statement itself.
            if let Some(replacement) = try_simplify_if_bool_return(if_stmt, ast) {
                *stmt = replacement;
            }
        }
        Statement::ExpressionStatement(expr_stmt) => {
            simplify_in_expr(&mut expr_stmt.expression, ast);
        }
        Statement::VariableDeclaration(var_decl) => {
            for declarator in &mut var_decl.declarations {
                if let Some(ref mut init) = declarator.init {
                    simplify_in_expr(init, ast);
                }
            }
        }
        Statement::ReturnStatement(ret) => {
            if let Some(ref mut arg) = ret.argument {
                simplify_in_expr(arg, ast);
            }
        }
        _ => {}
    }
}

fn simplify_in_stmts<'a>(stmts: &mut oxc_allocator::Vec<'a, Statement<'a>>, ast: &AstBuilder<'a>) {
    for stmt in stmts.iter_mut() {
        simplify_in_stmt(stmt, ast);
    }
}

/// Walk into expressions to find nested function bodies.
fn simplify_in_expr<'a>(expr: &mut Expression<'a>, ast: &AstBuilder<'a>) {
    match expr {
        Expression::FunctionExpression(func) => {
            if let Some(ref mut body) = func.body {
                simplify_in_stmts(&mut body.statements, ast);
            }
        }
        Expression::ArrowFunctionExpression(arrow) => {
            simplify_in_stmts(&mut arrow.body.statements, ast);
        }
        Expression::AssignmentExpression(assign) => {
            simplify_in_expr(&mut assign.right, ast);
        }
        Expression::SequenceExpression(seq) => {
            for sub_expr in &mut seq.expressions {
                simplify_in_expr(sub_expr, ast);
            }
        }
        Expression::CallExpression(call) => {
            simplify_in_expr(&mut call.callee, ast);
            for arg in &mut call.arguments {
                if let Argument::SpreadElement(spread) = arg {
                    simplify_in_expr(&mut spread.argument, ast);
                } else {
                    let arg_expr: &mut Expression<'a> = arg.to_expression_mut();
                    simplify_in_expr(arg_expr, ast);
                }
            }
        }
        _ => {}
    }
}

/// Try to simplify an if statement that returns boolean literals in both
/// branches. Returns `Some(replacement)` if the pattern matches.
fn try_simplify_if_bool_return<'a>(
    if_stmt: &mut IfStatement<'a>,
    ast: &AstBuilder<'a>,
) -> Option<Statement<'a>> {
    let alt = if_stmt.alternate.as_ref()?;

    let then_bool = stmt_returns_bool(&if_stmt.consequent)?;
    let else_bool = stmt_returns_bool(alt)?;

    if then_bool == else_bool {
        return None;
    }

    // Take the condition expression from the if statement. The dummy `false`
    // literal is needed because OXC arena-allocated nodes cannot be moved out
    // directly; the entire IfStatement will be discarded by the caller.
    let condition = std::mem::replace(
        &mut if_stmt.test,
        ast.expression_boolean_literal(SPAN, false),
    );

    let return_expr = if then_bool {
        // if (cond) { return true } else { return false } → return cond
        condition
    } else {
        // if (cond) { return false } else { return true } → return !cond
        ast.expression_unary(SPAN, UnaryOperator::LogicalNot, condition)
    };

    Some(ast.statement_return(SPAN, Some(return_expr)))
}

/// Check if a statement is (or contains) a single `return <boolean_literal>`.
fn stmt_returns_bool(stmt: &Statement<'_>) -> Option<bool> {
    match stmt {
        Statement::ReturnStatement(ret) => expr_is_bool(ret.argument.as_ref()?),
        Statement::BlockStatement(block) => {
            if block.body.len() != 1 {
                return None;
            }
            stmt_returns_bool(&block.body[0])
        }
        _ => None,
    }
}

/// Check if an expression is a boolean literal and return its value.
fn expr_is_bool(expr: &Expression<'_>) -> Option<bool> {
    if let Expression::BooleanLiteral(lit) = expr {
        Some(lit.value)
    } else {
        None
    }
}
