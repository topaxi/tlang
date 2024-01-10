use tlang_ast::node::{AstNode, FunctionDeclaration, Node, UnaryOp};

use crate::generator::{BlockContext, CodegenJS};

fn map_to_function_declaration(node: &Node) -> Option<&FunctionDeclaration> {
    match &node.ast_node {
        AstNode::FunctionDeclaration(declaration) => Some(declaration),
        _ => None,
    }
}

fn filter_comments(node: &Node) -> bool {
    matches!(
        node.ast_node,
        AstNode::SingleLineComment(_) | AstNode::MultiLineComment(_)
    )
}

pub fn generate_function_declarations(codegen: &mut CodegenJS, name: &Node, declarations: &[Node]) {
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
        if let Some(expr) = &declaration.guard {
            if let AstNode::VariableDeclaration { pattern, .. } = &expr.ast_node {
                match &pattern.ast_node {
                    AstNode::Identifier(name) => {
                        let tmp_variable = codegen.current_scope().declare_tmp_variable();
                        codegen.push_indent();
                        codegen.push_str(&format!("let {};\n", tmp_variable));
                        codegen
                            .current_scope()
                            .declare_variable_alias(name, &tmp_variable);
                    }
                    AstNode::List(patterns) => {
                        for pattern in patterns {
                            if let AstNode::Identifier(name) = &pattern.ast_node {
                                let tmp_variable = codegen.current_scope().declare_tmp_variable();
                                codegen.push_indent();
                                codegen.push_str(&format!("let {};\n", tmp_variable));
                                codegen
                                    .current_scope()
                                    .declare_variable_alias(name, &tmp_variable);
                            }
                        }
                    }
                    AstNode::EnumExtraction {
                        identifier,
                        elements,
                        named_fields: _,
                    } => {
                        let tmp_variable_enum = codegen.current_scope().declare_tmp_variable();
                        let enum_name = match &identifier.ast_node {
                            AstNode::Identifier(name) => name.clone(),
                            AstNode::NestedIdentifier(names) => names.clone().pop().unwrap(),
                            _ => unreachable!(),
                        };
                        codegen.push_indent();
                        codegen.push_str(&format!("let {};\n", tmp_variable_enum));
                        codegen
                            .current_scope()
                            .declare_variable_alias(&enum_name, &tmp_variable_enum);
                        for element in elements {
                            // Skip any Wildcards
                            if let AstNode::Wildcard = element.ast_node {
                                continue;
                            }
                            let identifier = match &element.ast_node {
                                AstNode::Identifier(name) => name.clone(),
                                AstNode::NestedIdentifier(names) => names.clone().pop().unwrap(),
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
    let mut first_declaration = true;
    for declaration in function_declarations.iter() {
        codegen.push_scope();

        if !first_declaration {
            codegen.push_str(" else ");
        }

        first_declaration = false;

        for (j, param) in declaration.parameters.iter().enumerate() {
            if let AstNode::FunctionParameter {
                id: _,
                node,
                type_annotation: _,
            } = &param.ast_node
            {
                match &node.ast_node {
                    AstNode::Identifier(name) => {
                        codegen
                            .current_scope()
                            .declare_variable_alias(name, &format!("args[{}]", j));
                    }
                    AstNode::List(patterns) => {
                        for (i, pattern) in patterns.iter().enumerate() {
                            if let AstNode::Identifier(name) = &pattern.ast_node {
                                codegen
                                    .current_scope()
                                    .declare_variable_alias(name, &format!("args[{}][{}]", j, i));
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
                    if let AstNode::FunctionParameter {
                        id: _,
                        node,
                        type_annotation: _,
                    } = &param.ast_node
                    {
                        matches!(
                            node.ast_node,
                            AstNode::Literal(_) | AstNode::List(_) | AstNode::EnumExtraction { .. }
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

                if let AstNode::FunctionParameter {
                    id: _,
                    node,
                    type_annotation: _,
                } = &param.ast_node
                {
                    match &node.ast_node {
                        AstNode::Identifier(_) | AstNode::Literal(_) => {
                            codegen.push_str(&format!("args[{}] === ", k));
                            codegen.generate_node(param, None);
                        }
                        AstNode::List(patterns) => {
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

                                match &pattern.ast_node {
                                    AstNode::Literal(_) => {
                                        codegen.push_str(&format!("args[{}][{}] === ", k, i));
                                        codegen.generate_node(pattern, None);
                                    }
                                    AstNode::UnaryOp(UnaryOp::Rest, identified) => {
                                        if let AstNode::Identifier(ref name) = identified.ast_node {
                                            codegen
                                                .push_str(&format!("args[{}].length >= {}", k, i));
                                            codegen.declare_function_pre_body_variable(
                                                name,
                                                &format!("args[{}].slice({})", k, i),
                                            );
                                        }
                                    }
                                    AstNode::Identifier(name) => {
                                        codegen.declare_function_pre_body_variable(
                                            name,
                                            &format!("args[{}][{}]", k, i),
                                        );
                                    }
                                    _ => unreachable!(),
                                }
                            }
                        }
                        AstNode::EnumExtraction {
                            identifier,
                            elements,
                            named_fields,
                        } => {
                            let identifier = match &identifier.ast_node {
                                AstNode::Identifier(name) => name.clone(),
                                AstNode::NestedIdentifier(names) => names.clone().pop().unwrap(),
                                _ => unreachable!(),
                            };
                            codegen.push_str(&format!("args[{}].tag === \"{}\"", k, identifier));
                            for (i, element) in elements.iter().enumerate() {
                                // Skip any Wildcards
                                if let AstNode::Wildcard = element.ast_node {
                                    continue;
                                }
                                let identifier = match &element.ast_node {
                                    AstNode::Identifier(name) => name.clone(),
                                    AstNode::NestedIdentifier(names) => {
                                        names.clone().pop().unwrap()
                                    }
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
            if let Some(expr) = &declaration.guard {
                codegen.push_str(" && ");
                generate_function_definition_guard(codegen, expr);
            }
            codegen.push_str(") {\n");
        } else {
            if let Some(expr) = &declaration.guard {
                codegen.push_str("if (");
                generate_function_definition_guard(codegen, expr);
                codegen.push_str(") {\n");
            }
            codegen.push_str("{\n");
        }

        codegen.inc_indent();
        // Alias identifier args back to the parameter names for readability of generated code.
        for (j, param) in declaration.parameters.iter().enumerate() {
            if let AstNode::FunctionParameter {
                id: _,
                node,
                type_annotation: _,
            } = &param.ast_node
            {
                if let AstNode::Identifier(ref name) = node.ast_node {
                    codegen.push_indent();
                    codegen.push_str(&format!("let {} = args[{}];\n", name, j));
                    codegen.current_scope().declare_variable_alias(name, name);
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

fn generate_function_definition_guard(codegen: &mut CodegenJS, node: &Node) {
    if let AstNode::VariableDeclaration {
        pattern,
        expression,
        ..
    } = &node.ast_node
    {
        match &pattern.ast_node {
            AstNode::Identifier(name) => {
                let guard_variable = codegen.current_scope().resolve_variable(name);
                codegen.push_str(&format!("({} = ", guard_variable.unwrap()));
                codegen.generate_node(expression, None);
                codegen.push_char(')');
            }
            AstNode::EnumExtraction {
                identifier,
                elements,
                named_fields,
            } => {
                let enum_name = match &identifier.ast_node {
                    AstNode::Identifier(name) => name.clone(),
                    AstNode::NestedIdentifier(names) => names.clone().pop().unwrap(),
                    _ => unreachable!(),
                };
                let guard_variable = codegen.current_scope().resolve_variable(&enum_name);
                codegen.push_str(&format!("({} = ", guard_variable.as_ref().unwrap()));
                codegen.generate_node(expression, None);
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
                    // Skip any Wildcards
                    if let AstNode::Wildcard = element.ast_node {
                        continue;
                    }
                    let identifier = match &element.ast_node {
                        AstNode::Identifier(name) => name.clone(),
                        AstNode::NestedIdentifier(names) => names.clone().pop().unwrap(),
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
        codegen.generate_node(node, None);
    }
}

pub fn generate_function_declaration(
    codegen: &mut CodegenJS,
    name: &Node,
    declaration: &FunctionDeclaration,
) {
    let name_as_str = fn_identifier_to_string(name);
    let is_tail_recursive = is_function_body_tail_recursive(&name_as_str, &declaration.body);
    codegen.push_function_context(
        &name_as_str,
        &declaration.parameters,
        is_tail_recursive,
        false,
    );

    codegen.push_indent();
    codegen.push_str(&format!("function {}(", name_as_str));
    codegen.push_scope();

    for (i, param) in declaration.parameters.iter().enumerate() {
        if i > 0 {
            codegen.push_str(", ");
        }
        codegen.generate_node(param, None);
    }

    codegen.push_str(") {\n");
    codegen.flush_statement_buffer();
    codegen.inc_indent();
    generate_function_body(codegen, &declaration.body, is_tail_recursive);
    codegen.dec_indent();
    codegen.push_indent();
    codegen.push_str("}\n");
    codegen.pop_scope();
    codegen.pop_function_context();
}

pub fn generate_function_expression(
    codegen: &mut CodegenJS,
    name: &Option<Box<Node>>,
    declaration: &FunctionDeclaration,
) {
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
        "" => "function(".to_string(),
        _ => format!("function {}(", name_as_str),
    };

    codegen.push_str(&function_keyword);

    for (i, param) in declaration.parameters.iter().enumerate() {
        if i > 0 {
            codegen.push_str(", ");
        }
        codegen.generate_node(param, None);
    }

    codegen.push_str(") {\n");
    codegen.inc_indent();
    codegen.flush_statement_buffer();
    generate_function_body(codegen, &declaration.body, is_tail_recursive);
    codegen.dec_indent();
    codegen.push_indent();
    codegen.push_char('}');
    codegen.pop_function_context();
    codegen.pop_scope();
}

pub fn generate_function_parameter(codegen: &mut CodegenJS, node: &Node) {
    match &node.ast_node {
        // Do not run generate_identifier, as that would resolve the parameter as a variable.
        AstNode::Identifier(ident) => {
            codegen.current_scope().declare_variable(ident);
            codegen.push_str(ident);
        }
        AstNode::Literal(_) => codegen.generate_node(node, None),
        AstNode::List(_) => todo!(),
        // Wildcards are handled within pipeline and call expressions,
        AstNode::Wildcard => unreachable!("Unexpected wildcard in function parameter."),
        _ => todo!(),
    }
}

fn generate_function_body(codegen: &mut CodegenJS, body: &Node, is_tail_recursive: bool) {
    codegen.push_context(BlockContext::FunctionBody);
    flush_function_pre_body(codegen);
    if is_tail_recursive {
        codegen.push_indent();
        codegen.push_str("while (true) {\n");
        codegen.inc_indent();
    }
    codegen.generate_node(body, None);
    if is_tail_recursive {
        codegen.dec_indent();
        codegen.push_indent();
        codegen.push_str("}\n");
    }
    codegen.pop_context();
}

pub fn generate_return_statement(codegen: &mut CodegenJS, expr: &Option<Box<Node>>) {
    // We do not render a return statement if we are in a tail recursive function body.
    // Which calls the current function recursively.
    if expr.is_some() {
        if let AstNode::RecursiveCall(call_exp) = &expr.as_ref().unwrap().ast_node {
            let call_identifier = match &call_exp.ast_node {
                AstNode::Call {
                    function,
                    arguments: _,
                } => {
                    if let AstNode::Identifier(name) = &function.ast_node {
                        Some(name)
                    } else {
                        None
                    }
                }
                _ => None,
            };

            if call_identifier.is_some() {
                if let Some(function_context) = codegen.get_function_context() {
                    if function_context.is_tail_recursive
                        && &function_context.name == call_identifier.unwrap()
                    {
                        return codegen.generate_node(&expr.clone().unwrap(), None);
                    }
                }
            }
        }
    }

    codegen.push_indent();
    codegen.push_str("return");

    if let Some(expr) = expr {
        codegen.push_char(' ');
        codegen.generate_node(expr, None);
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

fn is_function_body_tail_recursive(function_name: &str, node: &Node) -> bool {
    // Recursively traverse nodes to check for tail recursive calls to the function itself.
    // We currently only support tail recursion to the function itself, not any other function.
    // Therefore we look for RecursiveCall nodes which reference the current function name.
    match &node.ast_node {
        AstNode::RecursiveCall(node) => {
            // Node is a Call expression, unwrap first.
            if let AstNode::Call {
                function,
                arguments: _,
            } = &node.ast_node
            {
                // If the function is an identifier, check if it's the same as the current function name.
                if let AstNode::Identifier(name) = &function.ast_node {
                    if name == function_name {
                        return true;
                    }
                }
            }
            false
        }
        AstNode::Block(statements, expression) => {
            for statement in statements {
                if is_function_body_tail_recursive(function_name, statement) {
                    return true;
                }
            }
            if let Some(expression) = expression {
                return is_function_body_tail_recursive(function_name, expression);
            }
            false
        }
        AstNode::ExpressionStatement(expression) => {
            is_function_body_tail_recursive(function_name, expression)
        }
        AstNode::Match {
            expression,
            arms: _,
        } => is_function_body_tail_recursive(function_name, expression),
        AstNode::MatchArm {
            pattern: _,
            expression,
        } => is_function_body_tail_recursive(function_name, expression),
        AstNode::IfElse {
            condition,
            then_branch,
            else_branch,
        } => {
            is_function_body_tail_recursive(function_name, condition)
                || is_function_body_tail_recursive(function_name, then_branch)
                || else_branch.as_ref().map_or(false, |branch| {
                    is_function_body_tail_recursive(function_name, branch)
                })
        }
        AstNode::ReturnStatement(node) => {
            if let Some(node) = node {
                is_function_body_tail_recursive(function_name, node)
            } else {
                false
            }
        }
        _ => false,
    }
}

fn fn_identifier_to_string(name: &Node) -> String {
    match &name.ast_node {
        AstNode::Identifier(name) => name.clone(),
        _ => todo!(),
    }
}
