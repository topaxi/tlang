use std::collections::HashSet;

use tlang_ast::node as ast;
use tlang_ast::token::Token;

use crate::generator::{BlockContext, CodegenJS};

impl CodegenJS {
    pub(crate) fn generate_function_declarations(
        &mut self,
        declarations: &[ast::FunctionDeclaration],
        first_declaration_comments: &[Token],
    ) {
        let first_declaration = declarations.first().unwrap();
        let name_as_str = fn_identifier_to_string(&first_declaration.name);

        let is_any_definition_tail_recursive = declarations.iter().any(|declaration| {
            is_function_body_tail_recursive_block(&name_as_str, &declaration.body)
        });

        let mut seen_comments: HashSet<String> = HashSet::new();

        // These comments were already rendered
        for comment in first_declaration_comments {
            if let Some(comment_str) = comment.get_comment() {
                seen_comments.insert(comment_str.to_string());
            }
        }

        for declaration in declarations {
            for comment in &declaration.leading_comments {
                if let Some(comment_str) = comment.get_comment() {
                    if seen_comments.insert(comment_str.to_string()) {
                        self.push_indent();
                        self.generate_comment(comment);
                    }
                }
            }
        }

        self.generate_struct_method_binding(first_declaration);
        self.push_scope();
        self.push_str("function ");
        self.push_str(&name_as_str);
        self.push_char('(');
        let (arg_binding, arg_bindings) = self.generate_function_arguments(declarations);
        self.push_str(") {\n");
        self.inc_indent();

        // Declare and output temporary variables for if let guards.
        // TODO: This should be very similar to match arm let expressions, maybe we can unify them.
        //       Might even better if this was an AST transform instead, having
        //       FunctionDeclarations desugar into a FunctionDeclaration with a match stmt.
        //       Doing this as an AST transformation, we could gather the function declarations in
        //       after the parsing phase and get rid of the FunctionDeclarations AST node entirely.
        for declaration in declarations {
            if let Some(ref expr) = declaration.guard {
                if let ast::ExprKind::Let(pattern, _) = &expr.kind {
                    match &pattern.kind {
                        ast::PatternKind::Identifier(ident_pattern) => {
                            let tmp_variable = self
                                .current_scope()
                                .declare_variable(ident_pattern.name.as_str());
                            self.push_let_declaration(&tmp_variable);
                            self.push_newline();
                        }
                        ast::PatternKind::List(patterns) => {
                            for pattern in patterns {
                                if let ast::PatternKind::Identifier(ident_pattern) = &pattern.kind {
                                    let tmp_variable = self
                                        .current_scope()
                                        .declare_variable(ident_pattern.name.as_str());
                                    self.push_let_declaration(&tmp_variable);
                                    self.push_newline();
                                }
                            }
                        }
                        ast::PatternKind::Enum(enum_pattern) => {
                            let tmp_variable_enum = self.current_scope().declare_tmp_variable();
                            let enum_name = enum_pattern.path.segments.last().unwrap().as_str();
                            self.push_let_declaration(&tmp_variable_enum);
                            self.push_newline();
                            self.current_scope()
                                .declare_variable_alias(enum_name, &tmp_variable_enum);
                            for element in &enum_pattern.elements {
                                let identifier = match &element.kind {
                                    // Skip any Wildcards
                                    ast::PatternKind::Wildcard => continue,
                                    ast::PatternKind::Identifier(ident_pattern) => {
                                        ident_pattern.name.as_str()
                                    }
                                    _ => unreachable!(),
                                };
                                let tmp_variable =
                                    self.current_scope().declare_variable(identifier);
                                self.push_let_declaration(&tmp_variable);
                                self.push_newline();
                            }
                        }
                        _ => todo!("Handle if let guards with non-identifier patterns."),
                    }
                }
            }
        }

        if is_any_definition_tail_recursive {
            self.push_indent();
            self.push_str("while (true) {\n");
            self.inc_indent();
        }

        self.push_indent();

        let mut first_declaration = true;
        for declaration in declarations {
            self.push_scope();

            if !first_declaration {
                self.push_str(" else ");
            }

            for (j, param) in declaration.parameters.iter().enumerate() {
                match &param.pattern.kind {
                    ast::PatternKind::Identifier(ident_pattern) => {
                        self.current_scope().declare_variable_alias(
                            ident_pattern.name.as_str(),
                            arg_bindings[j].as_str(),
                        );
                    }
                    ast::PatternKind::List(patterns) => {
                        for (i, pattern) in patterns.iter().enumerate() {
                            if let ast::PatternKind::Identifier(ident_pattern) = &pattern.kind {
                                let arg_name = arg_bindings[j].as_str();

                                self.current_scope().declare_variable_alias(
                                    ident_pattern.name.as_str(),
                                    &format!("{arg_name}[{i}]"),
                                );
                            }
                        }
                    }
                    _ => {}
                }
            }

            // Expand parameter matching if any definition has a different amount of
            // parameters.
            let parameter_variadic = declarations
                .iter()
                .any(|d| d.parameters.len() != declaration.parameters.len());
            let pattern_matched_parameters = declaration
                .parameters
                .iter()
                .enumerate()
                .filter(|(_, param)| {
                    matches!(
                        param.pattern.kind,
                        ast::PatternKind::Literal(_)
                            | ast::PatternKind::List(_)
                            | ast::PatternKind::Enum { .. }
                    )
                })
                .collect::<Vec<_>>();

            let variadic_or_pattern_matching =
                parameter_variadic || !pattern_matched_parameters.is_empty();

            if variadic_or_pattern_matching {
                if parameter_variadic {
                    let args_binding = arg_binding.as_ref().unwrap();
                    self.push_str(&format!("if ({args_binding}.length === "));
                    self.push_str(&declaration.parameters.len().to_string());

                    if !pattern_matched_parameters.is_empty() {
                        self.push_str(" && ");
                    }
                } else {
                    self.push_str("if (");
                }
                // Filter only literal params.
                for (j, (k, param)) in pattern_matched_parameters.into_iter().enumerate() {
                    if j > 0 {
                        self.push_str(" && ");
                    }

                    let arg_name = arg_bindings[k].as_str();

                    match &param.pattern.kind {
                        ast::PatternKind::Identifier { .. } | ast::PatternKind::Literal(_) => {
                            self.push_str(&format!("{arg_name} === "));
                            self.generate_function_parameter(&param.pattern);
                        }
                        ast::PatternKind::List(patterns) => {
                            if patterns.is_empty() {
                                self.push_str(&format!("{arg_name}.length === 0"));
                                continue;
                            }

                            let mut patterns_len = patterns.len();
                            let last_pattern_is_rest = patterns.last().is_some()
                                && matches!(
                                    patterns.last().unwrap().kind,
                                    ast::PatternKind::Rest(_)
                                );

                            if last_pattern_is_rest {
                                patterns_len -= 1;
                            }

                            self.push_str(&format!("{arg_name}.length >= {patterns_len}"));

                            for (i, pattern) in patterns.iter().enumerate() {
                                match &pattern.kind {
                                    ast::PatternKind::Literal(_) => {
                                        self.push_str(" && ");

                                        self.push_str(&format!("{arg_name}[{i}] === "));
                                        self.generate_pat(pattern);
                                    }
                                    ast::PatternKind::Rest(identifier) => {
                                        if let ast::PatternKind::Identifier(ident_pattern) =
                                            &identifier.kind
                                        {
                                            self.declare_function_pre_body_variable(
                                                ident_pattern.name.as_str(),
                                                &format!("{arg_name}.slice({i})"),
                                            );
                                        } else {
                                            unreachable!();
                                        }
                                    }
                                    ast::PatternKind::Identifier(ident_pattern) => {
                                        self.declare_function_pre_body_variable(
                                            ident_pattern.name.as_str(),
                                            &format!("{arg_name}[{i}]"),
                                        );
                                    }
                                    ast::PatternKind::Wildcard => {}
                                    _ => unreachable!(),
                                }
                            }
                        }
                        ast::PatternKind::Enum(enum_pattern) => {
                            let identifier = enum_pattern.path.segments.last().unwrap().as_str();
                            self.push_str(&format!("{arg_name}.tag === \"{identifier}\""));
                            for (i, element) in enum_pattern.elements.iter().enumerate() {
                                let identifier = match &element.kind {
                                    // Skip any Wildcards
                                    ast::PatternKind::Wildcard => continue,
                                    ast::PatternKind::Identifier(ident_pattern) => {
                                        ident_pattern.name.as_str()
                                    }
                                    _ => unreachable!(),
                                };
                                if enum_pattern.named_fields {
                                    self.declare_function_pre_body_variable(
                                        identifier,
                                        &format!("{arg_name}.{identifier}"),
                                    );
                                } else {
                                    self.declare_function_pre_body_variable(
                                        identifier,
                                        &format!("{arg_name}[{i}]"),
                                    );
                                }
                            }
                        }
                        _ => unreachable!(),
                    }
                }
            }

            if variadic_or_pattern_matching {
                if let Some(ref expr) = declaration.guard {
                    self.push_str(" && ");
                    self.generate_function_definition_guard(expr);
                }
                self.push_str(") {\n");
            } else if let Some(ref expr) = declaration.guard {
                self.push_str("if (");
                self.generate_function_definition_guard(expr);
                self.push_str(") {\n");
            } else {
                self.push_str("{\n");
            }

            self.inc_indent();

            if first_declaration {
                self.generate_comments(first_declaration_comments);
            }

            self.generate_comments(&declaration.leading_comments);

            // Alias identifier args back to the parameter names for readability of generated code.
            for (j, param) in declaration.parameters.iter().enumerate() {
                if let ast::PatternKind::Identifier(ident_pattern) = &param.pattern.kind {
                    let arg_name = arg_bindings[j].as_str();

                    if arg_name == ident_pattern.name {
                        continue;
                    }

                    // `args` is a special case, as we use it internally to refer to the arguments
                    // in the generated code. Given that we currently always reference arguments
                    // via `args` instead of normal arguments. We could probably be more precise
                    // and use actual arguments, but given that each signature could define
                    // different names, this is the easiest way to handle this for now.
                    let name = if Some(&ident_pattern.name) == arg_binding.as_ref() {
                        self.current_scope().declare_variable("args")
                    } else if arg_bindings.contains(&ident_pattern.name) {
                        self.current_scope().declare_variable(&ident_pattern.name)
                    } else {
                        ident_pattern.name.clone()
                    };

                    self.push_let_declaration_to_identifier(&name, arg_name);
                    self.current_scope().declare_variable_alias(&name, &name);
                }
            }
            // We handle the tail recursion case in multiple function body declarations ourselves higher up.
            self.push_function_context(
                &name_as_str,
                &declaration.parameters,
                &arg_bindings,
                is_any_definition_tail_recursive,
            );
            self.generate_function_body(&declaration.body, false);
            self.pop_function_context();
            self.dec_indent();
            self.push_indent();
            self.push_char('}');
            self.pop_scope();

            self.generate_comments(&declaration.trailing_comments);

            first_declaration = false;
        }

        if is_any_definition_tail_recursive {
            self.dec_indent();
            self.push_newline();
            self.push_indent();
            self.push_char('}');
        }

        self.dec_indent();
        self.push_newline();
        self.push_indent();
        self.push_str("}\n");
        self.pop_scope();
    }

    fn generate_function_arguments(
        &mut self,
        declarations: &[ast::FunctionDeclaration],
    ) -> (Option<String>, Vec<String>) {
        // TODO: What was my plan here?
        //let is_member_method = is_member_method(&declarations[0].name);
        let first_declaration_number_of_args = declarations[0].parameters.len();
        // check whether all declarations have the same number of parameters
        let same_number_of_args = declarations
            .iter()
            .all(|d| d.parameters.len() == first_declaration_number_of_args);

        if same_number_of_args {
            let mut bindings = Vec::with_capacity(first_declaration_number_of_args);

            for i in 0..declarations[0].parameters.len() {
                // If the name of this parameter is the same in all declarations, we can reuse the
                // actual defined name. Otherwise we use `arg{i}` as the name.
                let arg_name = declarations.iter().find_map(|d| {
                    let param = &d.parameters[i];
                    match &param.pattern.kind {
                        ast::PatternKind::Identifier(ident_pattern) => {
                            Some(ident_pattern.name.to_string())
                        }
                        ast::PatternKind::Enum(enum_pattern) => {
                            Some(get_enum_name(&enum_pattern.path).to_lowercase())
                        }
                        _ => None,
                    }
                });

                let arg_name = if arg_name.is_some()
                    && declarations
                        .iter()
                        .all(|d| match &d.parameters[i].pattern.kind {
                            ast::PatternKind::Identifier(ident_pattern) => {
                                Some(ident_pattern.name.to_string()) == arg_name
                            }
                            ast::PatternKind::Enum(ident_pattern) => {
                                Some(get_enum_name(&ident_pattern.path).to_lowercase()) == arg_name
                            }

                            _ => true,
                        }) {
                    #[allow(clippy::unnecessary_unwrap)]
                    arg_name.unwrap()
                } else {
                    self.current_scope().declare_variable(&format!("arg{i}"))
                };

                if i > 0 {
                    self.push_str(", ");
                }
                // TODO: if member method and self, push `this`.
                self.push_str(&arg_name);
                bindings.push(arg_name);
            }

            (None, bindings)
        } else {
            let args_binding = self.current_scope().declare_variable("args");
            self.push_str("...");
            self.push_str(&args_binding);

            let max_args = declarations
                .iter()
                .map(|d| d.parameters.len())
                .max()
                .unwrap();

            let mut bindings = Vec::with_capacity(max_args);

            for i in 0..max_args {
                // TODO: if member method and self, push `this`.
                bindings.push(format!("args[{i}]"));
            }

            (Some(args_binding), bindings)
        }
    }

    fn generate_function_definition_guard(&mut self, node: &ast::Expr) {
        if let ast::ExprKind::Let(pattern, expression) = &node.kind {
            match &pattern.kind {
                ast::PatternKind::Identifier(ident_pattern) => {
                    let guard_variable = self
                        .current_scope()
                        .resolve_variable(ident_pattern.name.as_str());
                    self.push_str(&format!("({} = ", guard_variable.unwrap()));
                    self.generate_expr(expression, None);
                    self.push_char(')');
                }
                ast::PatternKind::Enum(enum_pattern) => {
                    let enum_name = enum_pattern.path.segments.last().unwrap().as_str();
                    let guard_variable = self.current_scope().resolve_variable(enum_name);
                    self.push_str(&format!("({} = ", guard_variable.as_ref().unwrap()));
                    self.generate_expr(expression, None);
                    self.push_char(')');
                    for (i, element) in enum_pattern.elements.iter().enumerate() {
                        if i == 0 {
                            self.push_str(" && ");
                            self.push_str(&format!(
                                "{}.tag === \"{}\"",
                                guard_variable.as_ref().unwrap(),
                                enum_name
                            ));
                        }
                        let identifier = match &element.kind {
                            // Skip any Wildcards
                            ast::PatternKind::Wildcard => continue,
                            ast::PatternKind::Identifier(ident_pattern) => {
                                ident_pattern.name.as_str()
                            }
                            _ => unreachable!(),
                        };
                        self.push_str(" && ((");
                        let identifier_resolved = self.current_scope().resolve_variable(identifier);
                        if enum_pattern.named_fields {
                            self.push_str(&format!(
                                "{} = {}.{}",
                                identifier_resolved.unwrap(),
                                guard_variable.as_ref().unwrap(),
                                identifier,
                            ));
                        } else {
                            self.push_str(&format!(
                                "{} = {}[{}]",
                                identifier_resolved.unwrap(),
                                guard_variable.as_ref().unwrap(),
                                i
                            ));
                        }
                        self.push_str("), true)");
                    }
                }
                _ => todo!(),
            }
        } else {
            self.generate_expr(node, None);
        }
    }

    fn generate_function_parameter_list(&mut self, parameters: &[ast::FunctionParameter]) {
        self.push_char('(');

        let mut iter = parameters.iter();

        if let Some(param) = iter.next() {
            self.generate_pat(&param.pattern);

            // If the first param was the self param, we didn't render anything and we need to skip
            // the comma being rendered in the loop ahead.
            if matches!(param.pattern.kind, ast::PatternKind::_Self) {
                if let Some(param) = iter.next() {
                    self.generate_pat(&param.pattern);
                }
            }
        }

        for param in iter {
            self.push_str(", ");
            self.generate_pat(&param.pattern);
        }

        self.push_char(')');
    }

    fn generate_struct_method_binding(&mut self, declaration: &ast::FunctionDeclaration) {
        if is_static_method(&declaration.name) {
            let lhs = match &declaration.name.kind {
                ast::ExprKind::Path(path) => path.join("."),
                _ => unreachable!(),
            };

            self.push_indent();
            self.push_str(&format!("{} = ", lhs));
        } else if is_member_method(&declaration.name) {
            let target_name: String = match &declaration.name.kind {
                ast::ExprKind::FieldExpression(expr) => fn_identifier_to_string(&expr.base),
                _ => unreachable!(),
            };

            let field_name = match &declaration.name.kind {
                ast::ExprKind::FieldExpression(expr) => expr.field.to_string(),
                _ => unreachable!(),
            };

            self.push_indent();
            self.push_str(&target_name);
            self.push_str("Constructor.prototype.");
            self.push_str(&field_name);
            self.push_str(" = ");
        } else {
            self.push_indent();
        }
    }

    pub(crate) fn generate_function_declaration(&mut self, declaration: &ast::FunctionDeclaration) {
        let name_as_str = fn_identifier_to_string(&declaration.name);
        let is_tail_recursive =
            is_function_body_tail_recursive_block(&name_as_str, &declaration.body);
        self.push_function_context(
            &name_as_str,
            &declaration.parameters,
            &[],
            is_tail_recursive,
        );

        self.generate_struct_method_binding(declaration);
        self.push_str("function ");
        self.push_str(&name_as_str);
        self.push_scope();
        self.generate_function_parameter_list(&declaration.parameters);
        self.push_str(" {\n");
        self.flush_statement_buffer();
        self.inc_indent();
        self.generate_function_body(&declaration.body, is_tail_recursive);
        self.dec_indent();
        self.push_indent();
        self.push_str("}\n");
        self.pop_scope();
        self.pop_function_context();
    }

    pub(crate) fn generate_function_expression(
        self: &mut CodegenJS,
        declaration: &ast::FunctionDeclaration,
    ) {
        self.push_scope();

        let name_as_str = fn_identifier_to_string(&declaration.name);
        let is_tail_recursive =
            is_function_body_tail_recursive_block(&name_as_str, &declaration.body);

        self.push_function_context(
            &name_as_str,
            &declaration.parameters,
            &[],
            is_tail_recursive,
        );

        self.push_str("function");

        match name_as_str.as_str() {
            "anonymous" => {}
            _ => {
                self.push_char(' ');
                self.push_str(&name_as_str);
            }
        };

        self.generate_function_parameter_list(&declaration.parameters);
        self.push_str(" {\n");
        self.inc_indent();
        self.flush_statement_buffer();
        self.generate_function_body(&declaration.body, is_tail_recursive);
        self.dec_indent();
        self.push_indent();
        self.push_char('}');
        self.pop_function_context();
        self.pop_scope();
    }

    fn generate_function_parameter(&mut self, pattern: &ast::Pattern) {
        match &pattern.kind {
            // Do not run generate_identifier, as that would resolve the parameter as a variable.
            ast::PatternKind::Identifier(ident_pattern) => {
                self.current_scope()
                    .declare_variable(ident_pattern.name.as_str());
                self.push_str(ident_pattern.name.as_str());
            }
            ast::PatternKind::Literal(_) => self.generate_pat(pattern),
            ast::PatternKind::List(_) => todo!(),
            // Wildcards are handled within pipeline and call expressions,
            ast::PatternKind::Wildcard => {
                unreachable!("Unexpected wildcard in function parameter.")
            }
            _ => todo!(),
        }
    }

    fn generate_function_body(&mut self, body: &ast::Block, is_tail_recursive: bool) {
        self.push_context(BlockContext::FunctionBody);
        self.flush_function_pre_body();
        if is_tail_recursive {
            self.push_indent();
            self.push_str("while (true) {\n");
            self.inc_indent();
        }
        self.generate_block(body);
        if is_tail_recursive {
            self.dec_indent();
            self.push_indent();
            self.push_str("}\n");
        }
        self.pop_context();
    }

    pub(crate) fn generate_return_statement(self: &mut CodegenJS, expr: &Option<ast::Expr>) {
        // We do not render a return statement if we are in a tail recursive function body.
        // Which calls the current function recursively.
        if expr.is_some() {
            if let ast::ExprKind::RecursiveCall(call_expr) = &expr.as_ref().unwrap().kind {
                let call_identifier = if let ast::ExprKind::Path(_) = &call_expr.callee.kind {
                    Some(fn_identifier_to_string(&call_expr.callee))
                } else {
                    None
                };

                if call_identifier.is_some() {
                    if let Some(function_context) = self.get_function_context() {
                        if function_context.is_tail_recursive
                            && function_context.name == call_identifier.unwrap()
                        {
                            return self.generate_optional_expr(expr, None);
                        }
                    }
                }
            }
        }

        self.push_indent();
        self.push_str("return");

        if let Some(expr) = expr {
            self.push_char(' ');
            self.generate_expr(expr, None);
        }

        self.push_str(";\n");
    }

    pub(crate) fn generate_recursive_call_expression(&mut self, expr: &ast::CallExpression) {
        // If call expression is referencing the current function, all we do is update the arguments,
        // as we are in a while loop.
        if let ast::ExprKind::Path(_) = &expr.callee.kind {
            if let Some(function_context) = self.get_function_context() {
                if function_context.is_tail_recursive
                        // TODO: Comparing identifier by string might not be the best idea.
                        && function_context.name == fn_identifier_to_string(&expr.callee)
                {
                    let params = function_context.parameter_bindings.clone();

                    let tmp_vars = params
                        .iter()
                        .map(|_| self.current_scope().declare_tmp_variable())
                        .collect::<Vec<_>>();

                    for (i, arg) in expr.arguments.iter().enumerate() {
                        self.push_let_declaration_to_expr(&tmp_vars[i], arg);
                        self.push_str(";\n");
                    }

                    for (i, arg_name) in params.iter().enumerate() {
                        self.push_indent();
                        self.push_str(arg_name);
                        self.push_str(" = ");
                        self.push_str(&tmp_vars[i]);
                        self.push_str(";\n");
                    }

                    return;
                }
            }
        }

        // For any other referenced function, we do a normal call expression.
        self.generate_call_expression(expr)
    }

    fn flush_function_pre_body(self: &mut CodegenJS) {
        for (name, value) in &self.consume_function_pre_body_declarations() {
            self.push_let_declaration_to_identifier(name, value);
            self.current_scope().declare_variable_alias(name, name);
        }
        self.flush_statement_buffer();
    }
}

fn get_enum_name(path: &ast::Path) -> String {
    return path.segments[path.segments.len() - 2].to_string();
}

fn is_function_body_tail_recursive_stmt(function_name: &str, stmt: &ast::Stmt) -> bool {
    match &stmt.kind {
        ast::StmtKind::Expr(expr) => is_function_body_tail_recursive(function_name, expr),
        ast::StmtKind::Return(expr) => {
            if let Some(expr) = expr.as_ref() {
                is_function_body_tail_recursive(function_name, expr)
            } else {
                false
            }
        }
        _ => false,
    }
}

fn is_function_body_tail_recursive_block(function_name: &str, block: &ast::Block) -> bool {
    for statement in &block.statements {
        if is_function_body_tail_recursive_stmt(function_name, statement) {
            return true;
        }
    }

    if let Some(ref expression) = block.expression {
        return is_function_body_tail_recursive(function_name, expression);
    }

    false
}

fn is_function_body_tail_recursive(function_name: &str, node: &ast::Expr) -> bool {
    // Recursively traverse nodes to check for tail recursive calls to the function itself.
    // We currently only support tail recursion to the function itself, not any other function.
    // Therefore we look for RecursiveCall nodes which reference the current function name.
    match &node.kind {
        ast::ExprKind::RecursiveCall(call_expr) => {
            // If the function is an identifier, check if it's the same as the current function name.
            if let ast::ExprKind::Path(_) = &call_expr.callee.kind {
                if fn_identifier_to_string(&call_expr.callee) == function_name {
                    return true;
                }
            }
            false
        }
        ast::ExprKind::Block(block) => is_function_body_tail_recursive_block(function_name, block),
        ast::ExprKind::Match(expr) => {
            is_function_body_tail_recursive(function_name, &expr.expression)
        }
        ast::ExprKind::IfElse(expr) => {
            is_function_body_tail_recursive(function_name, &expr.condition)
                || is_function_body_tail_recursive(function_name, &expr.then_branch)
                || expr.else_branches.iter().any(|else_clause| {
                    is_function_body_tail_recursive(function_name, &else_clause.consequence)
                })
        }
        _ => false,
    }
}

fn is_member_method(node: &ast::Expr) -> bool {
    matches!(&node.kind, ast::ExprKind::FieldExpression(_))
}

fn is_static_method(node: &ast::Expr) -> bool {
    if let ast::ExprKind::Path(path) = &node.kind {
        return path.segments.len() > 1;
    }

    false
}

pub(crate) fn fn_identifier_to_string(expr: &ast::Expr) -> String {
    match &expr.kind {
        ast::ExprKind::Path(path) => path.join("__"),
        ast::ExprKind::FieldExpression(expr) => expr.field.to_string(),
        kind => todo!("fn_identifier_to_string: {:?}", kind),
    }
}
