use oxc_ast::NONE;
use oxc_ast::ast::*;
use oxc_span::SPAN;
use tlang_ast::node::Visibility;
use tlang_ast::token::Literal;
use tlang_hir as hir;
use tlang_hir::HirId;

use crate::builtins;
use crate::error::CodegenError;
use crate::generator::{CodegenJS, InnerCodegen};

impl<'a> InnerCodegen<'a> {
    pub fn generate_stmts(&mut self, stmts: &[hir::Stmt]) -> Vec<Statement<'a>> {
        // Pre-register all declaration names before generating bodies so that
        // forward references (e.g. a function calling a sibling multi-clause
        // variant) resolve correctly.
        self.pre_register_declarations(stmts);

        let mut result = Vec::new();
        for stmt in stmts {
            let comments = &stmt.leading_comments;
            let mut generated = self.generate_stmt(stmt);
            if !comments.is_empty()
                && let Some(first) = generated.first_mut()
            {
                self.attach_leading_comments(first, comments);
            }
            result.extend(generated);
        }
        result
    }

    /// Scan `stmts` and register the JS name of every function, struct, and
    /// enum declaration into the `NameMap` **before** any body is generated.
    /// This ensures that forward references (e.g. a function body that calls a
    /// sibling arity-variant declared later in the same block) can be resolved.
    fn pre_register_declarations(&mut self, stmts: &[hir::Stmt]) {
        use crate::function_generator::fn_identifier_to_string;

        for stmt in stmts {
            match &stmt.kind {
                hir::StmtKind::FunctionDeclaration(decl) if !self.name_map.has(decl.hir_id) => {
                    let js_name = fn_identifier_to_string(&decl.name);
                    let is_method = matches!(decl.name.kind, hir::ExprKind::FieldAccess(_, _));
                    if is_method {
                        // Method names are assigned to prototypes (e.g.
                        // Result.prototype.unwrap) so they don't share
                        // namespace with other types' methods — skip dedup.
                        self.name_map.register_exact(decl.hir_id, &js_name);
                    } else {
                        self.name_map.register_local(decl.hir_id, &js_name);
                    }
                }
                hir::StmtKind::DynFunctionDeclaration(decl) if !self.name_map.has(decl.hir_id) => {
                    let js_name = fn_identifier_to_string(&decl.name);
                    let is_method = matches!(decl.name.kind, hir::ExprKind::FieldAccess(_, _));
                    if is_method {
                        self.name_map.register_exact(decl.hir_id, &js_name);
                    } else {
                        self.name_map.register_local(decl.hir_id, &js_name);
                    }
                }
                hir::StmtKind::StructDeclaration(decl) if !self.name_map.has(decl.hir_id) => {
                    self.name_map
                        .register_local(decl.hir_id, decl.name.as_str());
                }
                hir::StmtKind::EnumDeclaration(decl) if !self.name_map.has(decl.hir_id) => {
                    let enum_js = self
                        .name_map
                        .register_local(decl.hir_id, decl.name.as_str());
                    // Register each variant's HirId with its full JS access
                    // path (e.g. "Option.Some") so that pattern-match tag
                    // comparisons can resolve variant HirIds directly.
                    for variant in &decl.variants {
                        let variant_js = format!("{enum_js}.{}", variant.name.as_str());
                        self.name_map.register_exact(variant.hir_id, &variant_js);
                    }
                }
                _ => {}
            }
        }
    }

    pub fn generate_stmt(&mut self, statement: &hir::Stmt) -> Vec<Statement<'a>> {
        match &statement.kind {
            hir::StmtKind::Expr(expr) => self.generate_expr_stmt(expr),
            hir::StmtKind::Let(pattern, expression, _ty) => {
                self.generate_variable_declaration(pattern, expression)
            }
            hir::StmtKind::FunctionDeclaration(decl) => {
                let stmts = self.generate_function_declaration(decl);
                if decl.visibility == Visibility::Public {
                    stmts
                        .into_iter()
                        .map(|s| self.wrap_export_named(s))
                        .collect()
                } else {
                    stmts
                }
            }
            hir::StmtKind::DynFunctionDeclaration(decl) => {
                self.generate_dyn_function_declaration(decl)
            }
            hir::StmtKind::Return(expr) => self.generate_return_stmt(expr.as_deref()),
            hir::StmtKind::EnumDeclaration(decl) => {
                let stmt = self.generate_enum_declaration(decl);
                if decl.visibility == Visibility::Public {
                    vec![self.wrap_export_named(stmt)]
                } else {
                    vec![stmt]
                }
            }
            hir::StmtKind::StructDeclaration(decl) => {
                let stmt = self.generate_struct_declaration(decl);
                if decl.visibility == Visibility::Public {
                    vec![self.wrap_export_named(stmt)]
                } else {
                    vec![stmt]
                }
            }
            hir::StmtKind::ProtocolDeclaration(decl) => {
                let stmts = self.generate_protocol_declaration(decl);
                if decl.visibility == Visibility::Public {
                    stmts
                        .into_iter()
                        .map(|s| self.wrap_export_named(s))
                        .collect()
                } else {
                    stmts
                }
            }
            hir::StmtKind::ImplBlock(impl_block) => self.generate_impl_block(impl_block),
        }
    }

    /// Wrap a statement in an `export` declaration. If the statement contains
    /// a declaration (function, class, variable), it becomes
    /// `export <declaration>`. Otherwise the statement is returned unchanged.
    fn wrap_export_named(&self, stmt: Statement<'a>) -> Statement<'a> {
        if self.bundle_mode {
            return stmt;
        }
        let declaration = match stmt {
            Statement::FunctionDeclaration(func) => Declaration::FunctionDeclaration(func),
            Statement::ClassDeclaration(class) => Declaration::ClassDeclaration(class),
            Statement::VariableDeclaration(var) => Declaration::VariableDeclaration(var),
            other => return other,
        };

        let export = self.ast.module_declaration_export_named_declaration(
            SPAN,
            Some(declaration),
            self.ast.vec(),
            None::<StringLiteral<'a>>,
            ImportOrExportKind::Value,
            NONE,
        );
        Statement::from(export)
    }

    fn generate_expr_stmt(&mut self, expr: &hir::Expr) -> Vec<Statement<'a>> {
        match &expr.kind {
            // Naked block in stmt position: inline its stmts.
            hir::ExprKind::Block(block) => self.generate_block_stmts_propagate_scope(block),
            // Self-referencing tail call emits param reassignment + continue rec.
            hir::ExprKind::TailCall(call_expr) if self.is_self_referencing_tail_call(expr) => {
                self.generate_recursive_call_stmts(call_expr)
            }
            // If-else as statement
            hir::ExprKind::IfElse(cond, then_branch, else_branches) => {
                self.generate_if_else_stmts(cond, then_branch, else_branches)
            }
            // Match as statement
            hir::ExprKind::Match(match_expr, arms) => self.generate_match_stmts(match_expr, arms),
            // Loop as statement
            hir::ExprKind::Loop(block) => self.generate_loop_stmts(block),
            // Break
            hir::ExprKind::Break(value) => self.generate_break_stmts(value),
            // Continue
            hir::ExprKind::Continue => vec![self.ast.statement_continue(SPAN, None)],
            // Everything else: wrap in ExpressionStatement
            _ => {
                let e = self.generate_expr(expr);
                vec![self.expr_stmt(e)]
            }
        }
    }

    fn generate_variable_declaration(
        &mut self,
        pattern: &hir::Pat,
        value: &hir::Expr,
    ) -> Vec<Statement<'a>> {
        match &pattern.kind {
            hir::PatKind::Identifier(hir_id, ident) => {
                self.generate_variable_declaration_identifier(*hir_id, ident.as_str(), value)
            }
            hir::PatKind::List(patterns) => {
                self.generate_variable_declaration_list_pattern(patterns, value)
            }
            _ => {
                self.errors.push(CodegenError::unsupported(
                    "variable declaration pattern matching",
                    pattern.span,
                ));
                vec![]
            }
        }
    }

    fn generate_variable_declaration_identifier(
        &mut self,
        hir_id: HirId,
        name: &str,
        value: &hir::Expr,
    ) -> Vec<Statement<'a>> {
        // Generate init expression FIRST — references in the initialiser
        // resolve to the OLD binding via their own HirIds, so no temporary
        // alias / shadow dance is needed.
        let init = if matches!(&value.kind, hir::ExprKind::Literal(l) if **l == Literal::None) {
            None
        } else {
            Some(self.generate_expr(value))
        };

        // Now register the new binding (may shadow an outer name).
        let var_name = self.name_map.register(hir_id, name);

        vec![self.let_decl(&var_name, init)]
    }

    fn generate_variable_declaration_list_pattern(
        &mut self,
        patterns: &[hir::Pat],
        value: &hir::Expr,
    ) -> Vec<Statement<'a>> {
        let mut elements = Vec::new();
        let mut rest = None;

        for pattern in patterns {
            match &pattern.kind {
                hir::PatKind::Identifier(hir_id, ident) => {
                    let var_name = self.name_map.register(*hir_id, ident.as_str());
                    elements.push(Some(self.binding_pattern_ident(&var_name)));
                }
                hir::PatKind::Rest(inner) => {
                    if let hir::PatKind::Identifier(hir_id, ident) = &inner.kind {
                        let var_name = self.name_map.register(*hir_id, ident.as_str());
                        rest = Some(self.binding_pattern_ident(&var_name));
                    }
                }
                hir::PatKind::Wildcard => {
                    elements.push(None);
                }
                _ => {
                    self.errors.push(CodegenError::unsupported(
                        "variable declaration pattern matching",
                        pattern.span,
                    ));
                    elements.push(None);
                }
            }
        }

        let init = self.generate_expr(value);

        vec![self.let_array_destructuring(self.ast.vec_from_iter(elements), rest, init)]
    }

    pub fn generate_protocol_declaration(
        &mut self,
        decl: &hir::ProtocolDeclaration,
    ) -> Vec<Statement<'a>> {
        let name = decl.name.as_str();
        self.register_protocol(name);
        let js_name = CodegenJS::protocol_js_name(name);

        // Build the definition object passed to $protocol({ method: null | fn, ... })
        let props: Vec<ObjectPropertyKind<'a>> = decl
            .methods
            .iter()
            .map(|method| {
                let method_name = method.name.as_str();
                let value = if method.body.is_some() {
                    let fn_decl = self.build_fn_decl_from_protocol_method(method);
                    self.generate_function_expression(&fn_decl)
                } else {
                    self.ast.expression_null_literal(SPAN)
                };
                ObjectPropertyKind::ObjectProperty(
                    self.ast.alloc_object_property(
                        SPAN,
                        PropertyKind::Init,
                        PropertyKey::StaticIdentifier(
                            self.ast
                                .alloc_identifier_name(SPAN, self.alloc_str(method_name)),
                        ),
                        value,
                        false,
                        false,
                        false,
                    ),
                )
            })
            .collect();

        let def_obj = self
            .ast
            .expression_object(SPAN, self.ast.vec_from_iter(props));
        let protocol_call =
            self.call_expr(self.ident_expr("$protocol"), vec![Argument::from(def_obj)]);

        vec![self.const_decl(&js_name, protocol_call)]
    }

    fn build_fn_decl_from_protocol_method(
        &self,
        method: &hir::ProtocolMethodSignature,
    ) -> hir::FunctionDeclaration {
        let name = hir::Expr {
            hir_id: method.hir_id,
            kind: hir::ExprKind::Path(Box::new(hir::Path::new(
                vec![hir::PathSegment::new(method.name)],
                method.span,
            ))),
            ty: hir::Ty::unknown(),
            span: method.span,
        };

        hir::FunctionDeclaration {
            hir_id: method.hir_id,
            visibility: Visibility::Private,
            name,
            parameters: method.parameters.clone(),
            return_type: method.return_type.clone(),
            body: method.body.clone().unwrap_or_else(|| hir::Block {
                hir_id: method.hir_id,
                stmts: vec![],
                expr: None,
                scope: Default::default(),
                span: method.span,
            }),
            span: method.span,
        }
    }

    pub fn generate_impl_block(&mut self, impl_block: &hir::ImplBlock) -> Vec<Statement<'a>> {
        let protocol_name = impl_block.protocol_name.to_string();
        let target_type = impl_block.target_type.to_string();
        let js_protocol_name = CodegenJS::protocol_js_name(&protocol_name);
        // Use the resolved HirId to distinguish user-defined types (which have a
        // HirId) from builtin/unresolved types (which do not). For builtin
        // types consult the centralized registry to obtain the correct JS
        // constructor name — e.g. tlang `List` maps to JavaScript's `Array`.
        let js_type_constructor = if let Some(hir_id) = impl_block.target_type.res.hir_id() {
            self.name_map
                .resolve(hir_id)
                .map(String::from)
                .unwrap_or_else(|| target_type.clone())
        } else {
            builtins::builtin_type_js_constructor(&target_type)
                .map(String::from)
                .unwrap_or_else(|| target_type.clone())
        };

        let mut stmts = Vec::new();

        // $impl($Protocol, Type, { method: fn, ... })
        let props: Vec<ObjectPropertyKind<'a>> = impl_block
            .methods
            .iter()
            .map(|method| {
                let method_name = match &method.name.kind {
                    hir::ExprKind::Path(path) => path.last_ident().to_string(),
                    hir::ExprKind::FieldAccess(_, ident) => ident.to_string(),
                    _ => unreachable!(),
                };
                let func = self.generate_function_expression(method);
                ObjectPropertyKind::ObjectProperty(
                    self.ast.alloc_object_property(
                        SPAN,
                        PropertyKind::Init,
                        PropertyKey::StaticIdentifier(
                            self.ast
                                .alloc_identifier_name(SPAN, self.alloc_str(&method_name)),
                        ),
                        func,
                        false,
                        false,
                        false,
                    ),
                )
            })
            .collect();

        let methods_obj = self
            .ast
            .expression_object(SPAN, self.ast.vec_from_iter(props));
        let impl_call = self.call_expr(
            self.ident_expr("$impl"),
            vec![
                Argument::from(self.ident_expr(&js_protocol_name)),
                Argument::from(self.ident_expr(&js_type_constructor)),
                Argument::from(methods_obj),
            ],
        );
        stmts.push(self.expr_stmt(impl_call));

        for apply_ident in &impl_block.apply_methods {
            let method_name = apply_ident.as_str();
            // Builtin types have no HirId — applying protocol methods to them
            // is not allowed to preserve backwards compatibility.
            assert!(
                impl_block.target_type.res.hir_id().is_some(),
                "Cannot use 'apply' for built-in type '{target_type}': \
                 applying methods to built-in types is not allowed to preserve backwards compatibility"
            );
            let proto = format!("{js_type_constructor}.prototype");

            // $installMethod(TargetType.prototype, "methodName", $Protocol.methodName);
            let install_call = self.call_expr(
                self.ident_expr("$installMethod"),
                vec![
                    Argument::from(self.ident_expr(&proto)),
                    Argument::from(self.str_expr(method_name)),
                    Argument::from(
                        self.static_member_expr(self.ident_expr(&js_protocol_name), method_name),
                    ),
                ],
            );
            stmts.push(self.expr_stmt(install_call));
        }

        stmts
    }
}
