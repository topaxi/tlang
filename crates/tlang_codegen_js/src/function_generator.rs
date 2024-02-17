use tlang_ast::node::{
    AstNode, Expr, ExprKind, FunctionDeclaration, Node, NodeKind, Pattern, PatternKind, UnaryOp,
};

use crate::generator::{BlockContext, CodegenJS};

fn map_to_function_declaration(node: &Node) -> Option<&FunctionDeclaration> {
    match &node.ast_node {
        NodeKind::Legacy(AstNode::FunctionDeclaration(declaration)) => Some(declaration),
        _ => None,
    }
}

fn filter_comments(node: &Node) -> bool {
    matches!(
        node.ast_node,
        NodeKind::Legacy(AstNode::SingleLineComment(_) | AstNode::MultiLineComment(_))
    )
}

pub fn get_function_declaration_from_node(node: &Node) -> &FunctionDeclaration {
    match &node.ast_node {
        NodeKind::Legacy(AstNode::FunctionDeclaration(declaration)) => declaration,
        _ => panic!("Unexpected node in function declarations."),
    }
}

pub fn generate_function_declarations(codegen: &mut CodegenJS, name: &Expr, declarations: &[Node]) {
    let name_as_str = fn_identifier_to_string(name);

    let function_declarations = declarations
        .iter()
        .filter_map(map_to_function_declaration)
        .collect::<Vec<&FunctionDeclaration>>();

    let function_comments = declarations
        .iter()
        .filter(|node| filter_comments(node))
        .collect::<Vec<&Node>>();

    let is_any_definition_tail_recursive = function_declarations
        .iter()
        .any(|declaration| is_function_body_tail_recursive(&name_as_str, &declaration.body));

    for comment in function_comments {
        codegen.generate_node(comment, None);
    }
    codegen.push_indent();
    codegen.push_str(&format!("function {}(...args) {{\n", name_as_str));
    codegen.push_scope();
    codegen.current_scope().declare_variable("args");
    codegen.inc_indent();

    // Declare and output temporary variables for if let guards.
    for declaration in function_declarations.iter() {
        if let Some(ref expr) = *declaration.guard {
            if let ExprKind::Let(pattern, gexpr) = &expr.kind {
                match &pattern.kind {
                    PatternKind::Identifier { name, .. } => {
                        let tmp_variable = codegen.current_scope().declare_tmp_variable();
                        codegen.push_indent();
                        codegen.push_str(&format!("let {};\n", tmp_variable));
                        codegen
                            .current_scope()
                            .declare_variable_alias(&name.to_string(), &tmp_variable);
                    }
                    PatternKind::List(patterns) => {
                        for pattern in patterns {
                            if let PatternKind::Identifier { name, .. } = &pattern.kind {
                                let tmp_variable = codegen.current_scope().declare_tmp_variable();
                                codegen.push_indent();
                                codegen.push_str(&format!("let {};\n", tmp_variable));
                                codegen
                                    .current_scope()
                                    .declare_variable_alias(&name.to_string(), &tmp_variable);
                            }
                        }
                    }
                    PatternKind::Enum {
                        identifier,
                        elements,
                        named_fields: _,
                    } => {
                        let tmp_variable_enum = codegen.current_scope().declare_tmp_variable();
                        let enum_name = match &identifier.kind {
                            ExprKind::Identifier(name) => name.to_string(),
                            ExprKind::NestedIdentifier(names) => {
                                names.clone().pop().unwrap().to_string()
                            }
                            _ => unreachable!(),
                        };
                        codegen.push_indent();
                        codegen.push_str(&format!("let {};\n", tmp_variable_enum));
                        codegen
                            .current_scope()
                            .declare_variable_alias(&enum_name, &tmp_variable_enum);
                        for element in elements {
                            let identifier = match &element.kind {
                                // Skip any Wildcards
                                PatternKind::Wildcard => continue,
                                PatternKind::Identifier { name, .. } => name.to_string(),
                                _ => unreachable!(),
                            };
                            let tmp_variable = codegen.current_scope().declare_tmp_variable();
                            codegen.push_indent();
                            codegen.push_str(&format!("let {};\n", tmp_variable));
                            codegen
                                .current_scope()
                                .declare_variable_alias(&identifier, &tmp_variable);
                        }
                    }
                    _ => todo!("Handle if let guards with non-identifier patterns."),
                }
            }
        }
    }

    if is_any_definition_tail_recursive {
        codegen.push_indent();
        codegen.push_str("while (true) {\n");
        codegen.inc_indent();
    }

    codegen.push_indent();

    let mut comments = vec![];
    let mut first_declaration = true;
    for declaration in declarations.iter() {
        if matches!(
            &declaration.ast_node,
            NodeKind::Legacy(AstNode::SingleLineComment(_) | AstNode::MultiLineComment(_))
        ) {
            comments.push(declaration);
            continue;
        }

        let declaration = match &declaration.ast_node {
            NodeKind::Legacy(AstNode::FunctionDeclaration(declaration)) => declaration,
            _ => unreachable!("Unexpected node in function declarations."),
        };

        codegen.push_scope();

        if !first_declaration {
            codegen.push_str(" else ");
        }

        first_declaration = false;

        for (j, param) in declaration.parameters.iter().enumerate() {
            if let NodeKind::Legacy(AstNode::FunctionParameter {
                id: _,
                pattern,
                type_annotation: _,
            }) = &param.ast_node
            {
                match &pattern.kind {
                    PatternKind::Identifier { name, .. } => {
                        codegen
                            .current_scope()
                            .declare_variable_alias(&name.to_string(), &format!("args[{}]", j));
                    }
                    PatternKind::List(patterns) => {
                        for (i, pattern) in patterns.iter().enumerate() {
                            if let PatternKind::Identifier { name, .. } = &pattern.kind {
                                codegen.current_scope().declare_variable_alias(
                                    &name.to_string(),
                                    &format!("args[{}][{}]", j, i),
                                );
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        // Expand parameter matching if any definition has a different amount of
        // parameters.
        let parameter_variadic = function_declarations
            .iter()
            .map(|declaration| declaration.parameters.clone())
            .any(|params| params.len() != declaration.parameters.len());
        let pattern_matched_parameters =
            declaration
                .parameters
                .iter()
                .enumerate()
                .filter(|(_, param)| {
                    if let NodeKind::Legacy(AstNode::FunctionParameter {
                        id: _,
                        pattern,
                        type_annotation: _,
                    }) = &param.ast_node
                    {
                        matches!(
                            pattern.kind,
                            PatternKind::Literal(_)
                                | PatternKind::List(_)
                                | PatternKind::Enum { .. }
                        )
                    } else {
                        false
                    }
                });

        let variadic_or_pattern_matching =
            parameter_variadic || pattern_matched_parameters.clone().count() > 0;

        if variadic_or_pattern_matching {
            if parameter_variadic {
                codegen.push_str("if (args.length === ");
                codegen.push_str(&declaration.parameters.len().to_string());

                if pattern_matched_parameters.clone().count() > 0 {
                    codegen.push_str(" && ");
                }
            } else {
                codegen.push_str("if (");
            }
            // Filter only literal params.
            for (j, (k, param)) in pattern_matched_parameters.enumerate() {
                if j > 0 {
                    codegen.push_str(" && ");
                }

                if let NodeKind::Legacy(AstNode::FunctionParameter {
                    id: _,
                    pattern,
                    type_annotation: _,
                }) = &param.ast_node
                {
                    match &pattern.kind {
                        PatternKind::Identifier { .. } | PatternKind::Literal(_) => {
                            codegen.push_str(&format!("args[{}] === ", k));
                            codegen.generate_node(param, None);
                        }
                        PatternKind::List(patterns) => {
                            if patterns.is_empty() {
                                codegen.push_str(&format!("args[{}].length === 0", k));
                                continue;
                            }

                            // TODO: Handle multiple patterns, for now a simple recursive sum was the test case :D
                            let should_and = false;
                            for (i, pattern) in patterns.iter().enumerate() {
                                if should_and {
                                    codegen.push_str(" && ");
                                }

                                match &pattern.kind {
                                    PatternKind::Literal(_) => {
                                        codegen.push_str(&format!("args[{}][{}] === ", k, i));
                                        codegen.generate_pat(pattern);
                                    }
                                    PatternKind::Rest(identified) => {
                                        if let PatternKind::Identifier { ref name, .. } =
                                            identified.kind
                                        {
                                            codegen
                                                .push_str(&format!("args[{}].length >= {}", k, i));
                                            codegen.declare_function_pre_body_variable(
                                                &name.to_string(),
                                                &format!("args[{}].slice({})", k, i),
                                            );
                                        } else {
                                            unreachable!();
                                        }
                                    }
                                    PatternKind::Identifier { name, .. } => {
                                        codegen.declare_function_pre_body_variable(
                                            &name.to_string(),
                                            &format!("args[{}][{}]", k, i),
                                        );
                                    }
                                    PatternKind::Wildcard => {}
                                    _ => unreachable!(),
                                }
                            }
                        }
                        PatternKind::Enum {
                            identifier,
                            elements,
                            named_fields,
                        } => {
                            let identifier = match &identifier.kind {
                                ExprKind::Identifier(ident) => ident.to_string(),
                                ExprKind::NestedIdentifier(names) => {
                                    names.clone().pop().unwrap().to_string()
                                }
                                _ => unreachable!(),
                            };
                            codegen.push_str(&format!("args[{}].tag === \"{}\"", k, identifier));
                            for (i, element) in elements.iter().enumerate() {
                                let identifier = match &element.kind {
                                    // Skip any Wildcards
                                    PatternKind::Wildcard => continue,
                                    PatternKind::Identifier { name, .. } => name.to_string(),
                                    _ => unreachable!(),
                                };
                                if *named_fields {
                                    codegen.declare_function_pre_body_variable(
                                        &identifier,
                                        &format!("args[{}].{}", k, identifier),
                                    );
                                } else {
                                    codegen.declare_function_pre_body_variable(
                                        &identifier,
                                        &format!("args[{}][{}]", k, i),
                                    )
                                }
                            }
                        }
                        _ => unreachable!(),
                    }
                }
            }
        }

        if variadic_or_pattern_matching {
            if let Some(ref expr) = *declaration.guard {
                codegen.push_str(" && ");
                generate_function_definition_guard(codegen, expr);
            }
            codegen.push_str(") {\n");
        } else {
            if let Some(ref expr) = *declaration.guard {
                codegen.push_str("if (");
                generate_function_definition_guard(codegen, expr);
                codegen.push_str(") {\n");
            }
            codegen.push_str("{\n");
        }

        codegen.inc_indent();

        for comment in comments.iter() {
            codegen.push_indent();
            codegen.generate_node(comment, None);
        }
        comments.clear();

        // Alias identifier args back to the parameter names for readability of generated code.
        for (j, param) in declaration.parameters.iter().enumerate() {
            if let NodeKind::Legacy(AstNode::FunctionParameter {
                id: _,
                pattern,
                type_annotation: _,
            }) = &param.ast_node
            {
                if let PatternKind::Identifier { ref name, .. } = pattern.kind {
                    codegen.push_indent();
                    codegen.push_str(&format!("let {} = args[{}];\n", name, j));
                    codegen
                        .current_scope()
                        .declare_variable_alias(&name.to_string(), &name.to_string());
                }
            }
        }
        // We handle the tail recursion case in multiple function body declarations ourselves higher up.
        codegen.push_function_context(
            &name_as_str,
            &declaration.parameters,
            is_any_definition_tail_recursive,
            true,
        );
        generate_function_body(codegen, &declaration.body, false);
        codegen.pop_function_context();
        codegen.dec_indent();
        codegen.push_indent();
        codegen.push_char('}');
        codegen.pop_scope();
    }

    if is_any_definition_tail_recursive {
        codegen.dec_indent();
        codegen.push_char('\n');
        codegen.push_indent();
        codegen.push_char('}');
    }

    codegen.dec_indent();
    codegen.push_char('\n');
    codegen.push_indent();
    codegen.push_str("}\n");
    codegen.pop_scope();
}

fn generate_function_definition_guard(codegen: &mut CodegenJS, node: &Expr) {
    if let ExprKind::Let(pattern, expression) = &node.kind {
        match &pattern.kind {
            PatternKind::Identifier { name, .. } => {
                let guard_variable = codegen.current_scope().resolve_variable(&name.to_string());
                codegen.push_str(&format!("({} = ", guard_variable.unwrap()));
                codegen.generate_expr(expression, None);
                codegen.push_char(')');
            }
            PatternKind::Enum {
                identifier,
                elements,
                named_fields,
            } => {
                let enum_name = match &identifier.kind {
                    ExprKind::Identifier(name) => name.to_string(),
                    ExprKind::NestedIdentifier(names) => names.clone().pop().unwrap().to_string(),
                    _ => unreachable!(),
                };
                let guard_variable = codegen.current_scope().resolve_variable(&enum_name);
                codegen.push_str(&format!("({} = ", guard_variable.as_ref().unwrap()));
                codegen.generate_expr(expression, None);
                codegen.push_char(')');
                for (i, element) in elements.iter().enumerate() {
                    if i == 0 {
                        codegen.push_str(" && ");
                        codegen.push_str(&format!(
                            "{}.tag === \"{}\"",
                            guard_variable.as_ref().unwrap(),
                            enum_name
                        ));
                    }
                    let identifier = match &element.kind {
                        // Skip any Wildcards
                        PatternKind::Wildcard => continue,
                        PatternKind::Identifier { name, .. } => name.to_string(),
                        _ => unreachable!(),
                    };
                    codegen.push_str(" && ((");
                    let identifier_resolved = codegen.current_scope().resolve_variable(&identifier);
                    if *named_fields {
                        codegen.push_str(&format!(
                            "{} = {}.{}",
                            identifier_resolved.unwrap(),
                            guard_variable.as_ref().unwrap(),
                            identifier,
                        ));
                    } else {
                        codegen.push_str(&format!(
                            "{} = {}[{}]",
                            identifier_resolved.unwrap(),
                            guard_variable.as_ref().unwrap(),
                            i
                        ));
                    }
                    codegen.push_str("), true)");
                }
            }
            _ => todo!(),
        }
    } else {
        codegen.generate_expr(node, None);
    }
}

pub fn generate_function_parameter_list(codegen: &mut CodegenJS, parameters: &[Node]) {
    codegen.push_str("(");
    for (i, param) in parameters.iter().enumerate() {
        if i > 0 {
            codegen.push_str(", ");
        }
        codegen.generate_node(param, None);
    }
    codegen.push_str(")");
}

pub fn generate_function_declaration(codegen: &mut CodegenJS, name: &Expr, node: &Node) {
    let declaration = get_function_declaration_from_node(node);
    let name_as_str = fn_identifier_to_string(name);
    let is_tail_recursive = is_function_body_tail_recursive(&name_as_str, &declaration.body);
    codegen.push_function_context(
        &name_as_str,
        &declaration.parameters,
        is_tail_recursive,
        false,
    );

    codegen.push_indent();
    codegen.push_str(&format!("function {}", name_as_str));
    codegen.push_scope();
    generate_function_parameter_list(codegen, &declaration.parameters);
    codegen.push_str(" {\n");
    codegen.flush_statement_buffer();
    codegen.inc_indent();
    generate_function_body(codegen, &declaration.body, is_tail_recursive);
    codegen.dec_indent();
    codegen.push_indent();
    codegen.push_str("}\n");
    codegen.pop_scope();
    codegen.pop_function_context();
}

pub fn generate_function_expression(codegen: &mut CodegenJS, name: &Option<Expr>, node: &Node) {
    let declaration = get_function_declaration_from_node(node);
    codegen.push_scope();
    let name_as_str = if name.is_some() {
        fn_identifier_to_string(&name.clone().unwrap())
    } else {
        "".to_string()
    };
    let is_tail_recursive = is_function_body_tail_recursive(&name_as_str, &declaration.body);
    codegen.push_function_context(
        &name_as_str,
        &declaration.parameters,
        is_tail_recursive,
        false,
    );
    let function_keyword = match name_as_str.as_str() {
        "" => "function".to_string(),
        _ => format!("function {}", name_as_str),
    };

    codegen.push_str(&function_keyword);
    generate_function_parameter_list(codegen, &declaration.parameters);
    codegen.push_str(" {\n");
    codegen.inc_indent();
    codegen.flush_statement_buffer();
    generate_function_body(codegen, &declaration.body, is_tail_recursive);
    codegen.dec_indent();
    codegen.push_indent();
    codegen.push_char('}');
    codegen.pop_function_context();
    codegen.pop_scope();
}

pub fn generate_function_parameter(codegen: &mut CodegenJS, pattern: &Pattern) {
    match &pattern.kind {
        // Do not run generate_identifier, as that would resolve the parameter as a variable.
        PatternKind::Identifier { name, .. } => {
            codegen.current_scope().declare_variable(&name.to_string());
            codegen.push_str(&name.to_string());
        }
        PatternKind::Literal(_) => codegen.generate_pat(pattern),
        PatternKind::List(_) => todo!(),
        // Wildcards are handled within pipeline and call expressions,
        PatternKind::Wildcard => {
            unreachable!("Unexpected wildcard in function parameter.")
        }
        _ => todo!(),
    }
}

fn generate_function_body(codegen: &mut CodegenJS, body: &Expr, is_tail_recursive: bool) {
    codegen.push_context(BlockContext::FunctionBody);
    flush_function_pre_body(codegen);
    if is_tail_recursive {
        codegen.push_indent();
        codegen.push_str("while (true) {\n");
        codegen.inc_indent();
    }
    codegen.generate_expr(body, None);
    if is_tail_recursive {
        codegen.dec_indent();
        codegen.push_indent();
        codegen.push_str("}\n");
    }
    codegen.pop_context();
}

pub fn generate_return_statement(codegen: &mut CodegenJS, expr: &Option<Expr>) {
    // We do not render a return statement if we are in a tail recursive function body.
    // Which calls the current function recursively.
    if expr.is_some() {
        if let ExprKind::RecursiveCall(call_exp) = &expr.as_ref().unwrap().kind {
            let call_identifier = match &call_exp.kind {
                ExprKind::Call {
                    function,
                    arguments: _,
                } => {
                    if let ExprKind::Identifier(name) = &function.kind {
                        Some(name.to_string())
                    } else {
                        None
                    }
                }
                _ => None,
            };

            if call_identifier.is_some() {
                if let Some(function_context) = codegen.get_function_context() {
                    if function_context.is_tail_recursive
                        && function_context.name == call_identifier.unwrap().to_string()
                    {
                        return codegen.generate_expr(&expr.clone().unwrap(), None);
                    }
                }
            }
        }
    }

    codegen.push_indent();
    codegen.push_str("return");

    if let Some(expr) = expr {
        codegen.push_char(' ');
        codegen.generate_expr(expr, None);
    }

    codegen.push_str(";\n");
}

fn flush_function_pre_body(codegen: &mut CodegenJS) {
    for (name, value) in codegen.consume_function_pre_body_declarations().iter() {
        codegen.push_indent();
        codegen.push_str(&format!("let {} = {};\n", name, value));
        codegen.current_scope().declare_variable_alias(name, name);
    }
    codegen.flush_statement_buffer();
}

fn is_function_body_tail_recursive(function_name: &str, node: &Expr) -> bool {
    return false;
    // Recursively traverse nodes to check for tail recursive calls to the function itself.
    // We currently only support tail recursion to the function itself, not any other function.
    // Therefore we look for RecursiveCall nodes which reference the current function name.
    //match &node.kind {
    //    ExprKind::RecursiveCall(node) => {
    //        // Node is a Call expression, unwrap first.
    //        if let ExprKind::Call {
    //            function,
    //            arguments: _,
    //        } = &node.kind
    //        {
    //            // If the function is an identifier, check if it's the same as the current function name.
    //            if let ExprKind::Identifier(name) = &function.kind {
    //                if name == function_name {
    //                    return true;
    //                }
    //            }
    //        }
    //        false
    //    }
    //    ExprKind::Block(statements, expression) => {
    //        for statement in statements {
    //            if is_function_body_tail_recursive(function_name, statement) {
    //                return true;
    //            }
    //        }
    //        if let Some(expression) = expression.as_ref() {
    //            return is_function_body_tail_recursive(function_name, expression);
    //        }
    //        false
    //    }
    //    // :poop:
    //    NodeKind::Legacy(AstNode::ExpressionStatement(expression)) => {
    //        is_function_body_tail_recursive(function_name, expression)
    //    }
    //    NodeKind::Legacy(AstNode::Match {
    //        expression,
    //        arms: _,
    //    }) => is_function_body_tail_recursive(function_name, expression),
    //    NodeKind::Legacy(AstNode::MatchArm {
    //        pattern: _,
    //        expression,
    //    }) => is_function_body_tail_recursive(function_name, expression),
    //    NodeKind::Legacy(AstNode::IfElse {
    //        condition,
    //        then_branch,
    //        else_branch,
    //    }) => {
    //        is_function_body_tail_recursive(function_name, condition)
    //            || is_function_body_tail_recursive(function_name, then_branch)
    //            // TODO: Get rid of clone.
    //            || <Option<Node> as Clone>::clone(else_branch.as_ref()).map_or(false, |branch| {
    //                is_function_body_tail_recursive(function_name, &branch)
    //            })
    //    }
    //    NodeKind::Legacy(AstNode::ReturnStatement(node)) => {
    //        if let Some(node) = node.as_ref() {
    //            is_function_body_tail_recursive(function_name, node)
    //        } else {
    //            false
    //        }
    //    }
    //    _ => false,
    //}
}

fn fn_identifier_to_string(expr: &Expr) -> String {
    match &expr.kind {
        ExprKind::Identifier(name) => name.to_string(),
        _ => todo!(),
    }
}
