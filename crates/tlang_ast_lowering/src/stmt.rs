use std::collections::HashMap;

use log::debug;
use tlang_ast as ast;
use tlang_ast::node::{FunctionDeclaration, Ident, LetDeclaration};
use tlang_defs::DefKind;
use tlang_hir as hir;

use crate::LoweringContext;

impl LoweringContext {
    pub(crate) fn lower_stmt(&mut self, node: &ast::node::Stmt) -> Vec<hir::Stmt> {
        debug!("Lowering statement {:?}", node.kind);

        match &node.kind {
            ast::node::StmtKind::Expr(expr) => vec![hir::Stmt {
                hir_id: self.lower_node_id(node.id),
                kind: hir::StmtKind::Expr(Box::new(self.lower_expr(expr))),
                span: node.span,
                leading_comments: node.leading_comments.clone(),
                trailing_comments: node.trailing_comments.clone(),
            }],
            ast::node::StmtKind::Let(box LetDeclaration {
                pattern,
                expression,
                type_annotation,
                ..
            }) => {
                let expr = self.lower_expr(expression);
                let ty = self.lower_ty(type_annotation.as_ref());
                let pat = self.lower_pat(pattern);

                vec![hir::Stmt {
                    hir_id: self.lower_node_id(node.id),
                    kind: hir::StmtKind::Let(Box::new(pat), Box::new(expr), Box::new(ty)),
                    span: node.span,
                    leading_comments: node.leading_comments.clone(),
                    trailing_comments: node.trailing_comments.clone(),
                }]
            }
            ast::node::StmtKind::FunctionDeclaration(box decl) => {
                let decl = if has_non_identifier_patterns(decl) {
                    let decls = std::slice::from_ref(decl);
                    let all_param_names = get_param_names(decls);
                    self.lower_fn_decl_matching(decls, &all_param_names, &node.leading_comments)
                } else {
                    self.lower_fn_decl(decl)
                };

                vec![hir::Stmt {
                    hir_id: self.lower_node_id(node.id),
                    kind: hir::StmtKind::FunctionDeclaration(Box::new(decl)),
                    span: node.span,
                    leading_comments: node.leading_comments.clone(),
                    trailing_comments: node.trailing_comments.clone(),
                }]
            }
            // For multiple function declarations with the same name (which is already grouped
            // by the parser in the AST, for now), we emit multiple function declarations whith
            // their own name.
            ast::node::StmtKind::FunctionDeclarations(decls) => self.lower_fn_decls(node, decls),
            ast::node::StmtKind::Return(expr) => {
                let expr = expr.as_deref().map(|expr| Box::new(self.lower_expr(expr)));

                vec![hir::Stmt {
                    hir_id: self.lower_node_id(node.id),
                    kind: hir::StmtKind::Return(expr),
                    span: node.span,
                    leading_comments: node.leading_comments.clone(),
                    trailing_comments: node.trailing_comments.clone(),
                }]
            }
            ast::node::StmtKind::StructDeclaration(decl) => {
                let decl = hir::StructDeclaration {
                    hir_id: self.lower_node_id(node.id),
                    visibility: decl.visibility,
                    name: decl.name,
                    fields: decl
                        .fields
                        .iter()
                        .map(|field| self.lower_struct_field(field))
                        .collect(),
                };

                vec![hir::Stmt {
                    hir_id: self.lower_node_id(node.id),
                    kind: hir::StmtKind::StructDeclaration(Box::new(decl)),
                    span: node.span,
                    leading_comments: node.leading_comments.clone(),
                    trailing_comments: node.trailing_comments.clone(),
                }]
            }
            ast::node::StmtKind::EnumDeclaration(decl) => vec![self.lower_enum_decl(node, decl)],
            ast::node::StmtKind::ProtocolDeclaration(decl) => {
                vec![self.lower_protocol_decl(node, decl)]
            }
            ast::node::StmtKind::ImplBlock(impl_block) => {
                vec![self.lower_impl_block(node, impl_block)]
            }

            // This is a no-op, we do not emit any statement for this.
            // We might be losing some comments here, but this shouldn't be a problem within
            // HIR.
            ast::node::StmtKind::None => vec![],

            // Use and mod declarations are resolved before lowering and do not produce HIR.
            ast::node::StmtKind::UseDeclaration(_) | ast::node::StmtKind::ModDeclaration(_) => {
                vec![]
            }
        }
    }

    fn lower_fn_decls(
        &mut self,
        node: &ast::node::Stmt,
        decls: &[FunctionDeclaration],
    ) -> Vec<hir::Stmt> {
        for decl in decls {
            self.lower_node_id(decl.id);
        }

        let first_declaration = decls.first().unwrap();
        let has_variadic_arguments = decls
            .iter()
            .any(|decl| decl.parameters.len() != first_declaration.parameters.len());
        let all_param_names = get_param_names(decls);

        if has_variadic_arguments {
            // Group by arguments length and emit a function for each variant.
            // Not using a hashmap here, as the amount of fn decls should be farily small.
            // Therefore we just sort the declarations and start with short argument lists
            // first.
            // Currently the easiest way is to split up
            // ast::node::SmtKind::FunctionDeclarations by argument length and create a
            // new unique name for each declaration.
            let mut fn_variants = decls
                .iter()
                .map(|decl| decl.parameters.len())
                .collect::<Vec<_>>();
            fn_variants.sort();
            fn_variants.dedup();

            let mut fn_variant_ids = vec![];

            let mut grouped_decls: Vec<hir::Stmt> = fn_variants
                .iter()
                .enumerate()
                .map(|(i, arity)| {
                    let decls = decls
                        .iter()
                        .filter(|decl| decl.parameters.len() == *arity)
                        .cloned()
                        .map(|mut decl| {
                            match &mut decl.name.kind {
                                ast::node::ExprKind::Path(path) => {
                                    path.segments.last_mut().unwrap().set_arity(*arity);
                                }
                                ast::node::ExprKind::FieldExpression(fe) => {
                                    fe.field.set_arity(*arity);
                                }
                                _ => unreachable!(),
                            }
                            decl
                        })
                        .collect::<Vec<_>>();

                    let mut leading_comments = decls
                        .iter()
                        .flat_map(|d| d.leading_comments.clone())
                        .collect::<Vec<_>>();

                    if i == 0 {
                        leading_comments.extend(node.leading_comments.clone());
                    }

                    let decl = self.lower_fn_decl_matching(
                        &decls,
                        &all_param_names,
                        if i == 0 {
                            node.leading_comments.as_slice()
                        } else {
                            &[]
                        },
                    );

                    fn_variant_ids.push((*arity, decl.hir_id));

                    hir::Stmt {
                        hir_id: self.unique_id(),
                        kind: hir::StmtKind::FunctionDeclaration(Box::new(decl)),
                        span: node.span,
                        leading_comments,
                        trailing_comments: vec![],
                    }
                })
                .collect();

            let fn_name = self.lower_expr(&first_declaration.name);

            let dyn_fn_decl = hir::DynFunctionDeclaration {
                hir_id: self.unique_id(),
                name: fn_name,
                variants: fn_variant_ids,
            };

            let last_decl_id = decls.last().map(|d| d.id);
            // Use name_or_invalid() here — any error will be reported by
            // fn_name_or_error() inside lower_fn_decl_matching for each arity variant.
            let fn_name_str = first_declaration.name_or_invalid();
            self.define_symbol_after(
                dyn_fn_decl.hir_id,
                &fn_name_str,
                DefKind::Variable, // TODO, add symbol type for dyn dispatch functions
                first_declaration.span.start,
                |symbol| symbol.node_id == last_decl_id,
            );

            grouped_decls.push(hir::Stmt {
                hir_id: self.unique_id(),
                kind: hir::StmtKind::DynFunctionDeclaration(Box::new(dyn_fn_decl)),
                span: node.span,
                leading_comments: vec![],
                trailing_comments: vec![],
            });

            grouped_decls
        } else {
            let mut leading_comments = node.leading_comments.clone();
            leading_comments.extend(decls.iter().flat_map(|d| d.leading_comments.clone()));

            let hir_fn_decl =
                self.lower_fn_decl_matching(decls, &all_param_names, &node.leading_comments);

            vec![hir::Stmt {
                hir_id: self.lower_node_id(node.id),
                kind: hir::StmtKind::FunctionDeclaration(Box::new(hir_fn_decl)),
                span: node.span,
                leading_comments,
                trailing_comments: node.trailing_comments.clone(),
            }]
        }
    }

    fn lower_enum_decl(
        &mut self,
        node: &ast::node::Stmt,
        decl: &ast::node::EnumDeclaration,
    ) -> hir::Stmt {
        let decl = hir::EnumDeclaration {
            hir_id: self.lower_node_id(node.id),
            visibility: decl.visibility,
            name: decl.name,
            variants: decl
                .variants
                .iter()
                .map(|variant| hir::EnumVariant {
                    hir_id: self.lower_node_id(variant.id),
                    name: variant.name,
                    parameters: variant
                        .parameters
                        .iter()
                        .map(|field| self.lower_struct_field(field))
                        .collect(),
                    span: variant.span,
                })
                .collect::<Vec<_>>(),
        };

        hir::Stmt {
            hir_id: self.lower_node_id(node.id),
            kind: hir::StmtKind::EnumDeclaration(Box::new(decl)),
            span: node.span,
            leading_comments: node.leading_comments.clone(),
            trailing_comments: node.trailing_comments.clone(),
        }
    }

    fn lower_protocol_decl(
        &mut self,
        node: &ast::node::Stmt,
        decl: &ast::node::ProtocolDeclaration,
    ) -> hir::Stmt {
        let methods = decl
            .methods
            .iter()
            .map(|method| {
                let params = method
                    .parameters
                    .iter()
                    .map(|param| {
                        let name = match &param.pattern.kind {
                            ast::node::PatKind::Identifier(ident) => *ident.as_ref(),
                            ast::node::PatKind::_Self => Ident::new("self", param.pattern.span),
                            _ => Ident::new("_", param.pattern.span),
                        };
                        hir::FunctionParameter {
                            hir_id: self.unique_id(),
                            name,
                            type_annotation: self.lower_ty(param.type_annotation.as_ref()),
                            span: param.pattern.span,
                        }
                    })
                    .collect();
                hir::ProtocolMethodSignature {
                    hir_id: self.unique_id(),
                    name: method.name,
                    parameters: params,
                    return_type: self.lower_ty(method.return_type_annotation.as_ref()),
                    body: method.body.as_ref().map(|b| self.lower_block(b)),
                    span: method.span,
                }
            })
            .collect();

        let protocol = hir::ProtocolDeclaration {
            hir_id: self.lower_node_id(node.id),
            visibility: decl.visibility,
            name: decl.name,
            methods,
        };

        hir::Stmt {
            hir_id: self.lower_node_id(node.id),
            kind: hir::StmtKind::ProtocolDeclaration(Box::new(protocol)),
            span: node.span,
            leading_comments: node.leading_comments.clone(),
            trailing_comments: node.trailing_comments.clone(),
        }
    }

    fn lower_impl_block(
        &mut self,
        node: &ast::node::Stmt,
        impl_block: &ast::node::ImplBlock,
    ) -> hir::Stmt {
        let protocol_name = self.lower_path(&impl_block.protocol_name);
        let target_type = self.lower_path(&impl_block.target_type);

        // Group methods by name so multi-clause methods get lowered via pattern matching.
        // Use name_or_invalid() for grouping only — any error for invalid names will be
        // reported by fn_name_or_error() inside lower_fn_decl_matching.
        let mut method_groups: Vec<(String, Vec<&ast::node::FunctionDeclaration>)> = Vec::new();
        for method in &impl_block.methods {
            let name = method.name_or_invalid();
            if let Some(group) = method_groups.iter_mut().find(|(n, _)| *n == name) {
                group.1.push(method);
            } else {
                method_groups.push((name, vec![method]));
            }
        }

        let methods = method_groups
            .into_iter()
            .map(|(_name, decls)| {
                if decls.len() == 1 && !has_non_identifier_patterns(decls[0]) {
                    self.lower_fn_decl(decls[0])
                } else {
                    let owned: Vec<_> = decls.iter().map(|d| (*d).clone()).collect();
                    let all_param_names = get_param_names(&owned);
                    self.lower_fn_decl_matching(&owned, &all_param_names, &[])
                }
            })
            .collect();

        let hir_impl = hir::ImplBlock {
            hir_id: self.lower_node_id(node.id),
            protocol_name,
            target_type,
            methods,
            apply_methods: impl_block.apply_methods.clone(),
        };

        hir::Stmt {
            hir_id: self.lower_node_id(node.id),
            kind: hir::StmtKind::ImplBlock(Box::new(hir_impl)),
            span: node.span,
            leading_comments: node.leading_comments.clone(),
            trailing_comments: node.trailing_comments.clone(),
        }
    }

    fn lower_struct_field(&mut self, field: &ast::node::StructField) -> hir::StructField {
        hir::StructField {
            hir_id: self.lower_node_id(field.id),
            name: field.name,
            ty: self.lower_ty(Some(&field.ty)),
        }
    }

    fn setup_function_declaration_metadata(
        &mut self,
        decls: &[FunctionDeclaration],
        all_param_names: &[Option<Ident>],
    ) -> (
        tlang_span::HirId,
        hir::Expr,
        Vec<hir::FunctionParameter>,
        tlang_span::Span,
    ) {
        let mut span = decls[0].span;
        span.end = decls.last().unwrap().span.end;

        let hir_id = self.unique_id();
        let first_declaration = &decls[0];
        let fn_name = self.lower_expr(&first_declaration.name);

        // Map every clause NodeId to the shared HirId so that symbols
        // referring to this function resolve to the same declaration
        // regardless of which clause they came from.  Note that each clause
        // NodeId has *already* been inserted into `node_id_to_hir_id` (with
        // its own unique HirId) by the `lower_node_id` calls at the top of
        // `lower_fn_decls`; that per-clause HirId is used for scope keying.
        // We intentionally use a separate map here to avoid overwriting those
        // scope-keying entries.  See the field documentation on
        // `LoweringContext` for the full invariant.
        for decl in decls {
            self.fn_node_id_to_hir_id.insert(decl.id, hir_id);
        }

        let param_names = get_param_names(decls)
            .iter()
            .enumerate()
            .map(|(i, param_name)| {
                if let Some(param_name) = param_name {
                    *param_name
                } else if let Some(Some(ident)) = all_param_names.get(i) {
                    *ident
                } else {
                    Ident::new(&format!("arg{i}"), Default::default())
                }
            })
            .collect::<Vec<_>>();

        let params = param_names
            .iter()
            .enumerate()
            .map(|(i, ident)| {
                // Pick up any type annotation inferred by the semantic analysis phase.
                let type_annotation = decls
                    .iter()
                    .find_map(|d| d.parameters.get(i).and_then(|p| p.type_annotation.as_ref()))
                    .map(|ty| self.lower_ty(Some(ty)))
                    .unwrap_or_default();

                hir::FunctionParameter {
                    hir_id: self.unique_id(),
                    name: *ident,
                    type_annotation,
                    span: ident.span,
                }
            })
            .collect::<Vec<_>>();

        (hir_id, fn_name, params, span)
    }

    /// Register the function self-reference symbol and parameter symbols.
    ///
    /// `fn_name` should be obtained from [`fn_name_or_error`](Self::fn_name_or_error)
    /// so that an [`InvalidFunctionName`](crate::LoweringError::InvalidFunctionName)
    /// error is collected when the declaration has an invalid name expression.
    fn define_function_symbols(
        &mut self,
        hir_id: tlang_span::HirId,
        fn_name: &str,
        first_declaration: &FunctionDeclaration,
        params: &[hir::FunctionParameter],
    ) {
        self.define_symbol(
            hir_id,
            fn_name,
            DefKind::FunctionSelfRef(params.len() as u16),
            first_declaration.span.start,
        );

        for param in params {
            self.define_symbol(
                param.hir_id,
                param.name.as_str(),
                DefKind::Parameter,
                param.span.start,
            );
        }
    }

    fn create_match_value(
        &mut self,
        params: &[hir::FunctionParameter],
        span: tlang_span::Span,
    ) -> hir::Expr {
        if params.len() > 1 {
            let argument_list = hir::ExprKind::List(
                params
                    .iter()
                    .map(|param| {
                        let mut path =
                            hir::Path::new(vec![hir::PathSegment { ident: param.name }], span);

                        path.res.set_hir_id(param.hir_id);
                        path.res.set_binding_kind(hir::BindingKind::Param);

                        self.expr(span, hir::ExprKind::Path(Box::new(path)))
                    })
                    .collect(),
            );

            self.expr(tlang_span::Span::default(), argument_list)
        } else {
            let first_param = params.first().unwrap();
            let ident = first_param.name;
            let mut path = hir::Path::new(vec![hir::PathSegment { ident }], span);

            path.res.set_hir_id(first_param.hir_id);
            path.res.set_binding_kind(hir::BindingKind::Param);

            self.expr(
                tlang_span::Span::default(),
                hir::ExprKind::Path(Box::new(path)),
            )
        }
    }

    fn create_match_arms(
        &mut self,
        decls: &[FunctionDeclaration],
        leading_comments: &[ast::token::CommentToken],
    ) -> Vec<hir::MatchArm> {
        let mut idents = HashMap::new();
        decls
            .iter()
            .enumerate()
            .map(|(i, decl)| {
                let mut match_arm = self.lower_fn_decl_to_match_arm(decl, &mut idents);

                if i == 0 {
                    match_arm.leading_comments = leading_comments
                        .iter()
                        .cloned()
                        .chain(match_arm.leading_comments.clone())
                        .collect();
                }

                match_arm
            })
            .collect()
    }

    fn lower_fn_decl_matching(
        &mut self,
        decls: &[FunctionDeclaration],
        all_param_names: &[Option<Ident>],
        leading_comments: &[ast::token::CommentToken],
    ) -> hir::FunctionDeclaration {
        if decls.len() == 1 && !has_non_identifier_patterns(&decls[0]) {
            return self.lower_fn_decl(&decls[0]);
        }

        debug!(
            "Lowering multiple function declarations with the same name: {:?} {:?}",
            decls[0].name,
            decls.iter().map(|d| d.id).collect::<Vec<_>>()
        );

        self.with_new_scope(|this, _scope| {
            let (hir_id, fn_name, params, span) =
                this.setup_function_declaration_metadata(decls, all_param_names);

            let fn_name_str = this.fn_name_or_error(&decls[0]);
            this.define_function_symbols(hir_id, &fn_name_str, &decls[0], &params);

            let match_value = this.create_match_value(&params, span);

            let match_arms = this.create_match_arms(decls, leading_comments);

            let body = hir::Block::new(
                this.unique_id(),
                vec![],
                Some(hir::Expr {
                    hir_id: this.unique_id(),
                    kind: hir::ExprKind::Match(Box::new(match_value), match_arms),
                    ty: hir::Ty::unknown(),
                    span: tlang_span::Span::default(),
                }),
                tlang_span::Span::default(),
            );

            (hir_id, {
                let mut decl = hir::FunctionDeclaration::new(hir_id, fn_name, params, body);
                // Multi-clause functions inherit visibility from the first clause.
                decl.visibility = decls[0].visibility;
                decl
            })
        })
    }

    fn lower_fn_decl_to_match_arm(
        &mut self,
        decl: &FunctionDeclaration,
        idents: &mut HashMap<String, String>,
    ) -> hir::MatchArm {
        debug!(
            "Lowering function declaration to match arm: {:?} {:?}",
            decl.id, decl.name
        );

        self.with_scope(decl.id, |this| {
            // Remove the first declaration within the current scope, as in the AST
            // this was the function refering to itself, but we are here in a match arm
            // now.
            this.current_symbol_table.borrow_mut().shift();

            // Mapping argument pattern and signature guard into a match arm
            let pat = if decl.parameters.len() > 1 {
                let params = decl
                    .parameters
                    .iter()
                    // We lose type information of each declaration here, as we
                    // lower a whole FunctionParameter into a pattern. They
                    // should probably match for each declaration and we might want
                    // to verify this now or earlier in the pipeline.
                    // Ignored for now as types are basically a NOOP everywhere.
                    .map(|param| this.lower_pat_with_idents(&param.pattern, idents))
                    .collect();

                hir::Pat {
                    kind: hir::PatKind::List(params),
                    ty: hir::Ty::unknown(),
                    span: decl.span,
                }
            } else {
                this.lower_pat(&decl.parameters[0].pattern)
            };

            let guard = decl.guard.as_ref().map(|expr| this.lower_expr(expr));
            let mut body = this.lower_block_in_current_scope(&decl.body);
            body.hir_id = this.lower_node_id(decl.id);

            hir::MatchArm {
                hir_id: body.hir_id,
                pat,
                guard,
                block: body,
                pat_locals: 0,
                leading_comments: decl.leading_comments.clone(),
                trailing_comments: decl.trailing_comments.clone(),
            }
        })
    }
}

fn get_enum_name(path: &ast::node::Path) -> String {
    path.segments[path.segments.len() - 2].to_string()
}

fn has_non_identifier_patterns(decl: &FunctionDeclaration) -> bool {
    decl.parameters.iter().any(|param| {
        !matches!(
            param.pattern.kind,
            ast::node::PatKind::Identifier(_)
                | ast::node::PatKind::_Self
                | ast::node::PatKind::Wildcard
        )
    })
}

fn get_param_names(decls: &[FunctionDeclaration]) -> Vec<Option<Ident>> {
    let num_args = decls
        .iter()
        .map(|d| d.parameters.len())
        .max()
        .unwrap_or_default();
    let mut argument_names = Vec::with_capacity(num_args);

    for i in 0..num_args {
        // If the name of this parameter is the same in all declarations, we can reuse the
        // actual defined name. Otherwise we use return None and generate a name where necessary.
        let arg_name = decls.iter().find_map(|d| {
            let param_pattern_kind = &d.parameters.get(i).map(|p| &p.pattern.kind);

            match &param_pattern_kind {
                Some(ast::node::PatKind::Identifier(ident)) => Some(ident.to_string()),
                Some(ast::node::PatKind::Enum(enum_pattern)) => {
                    Some(get_enum_name(&enum_pattern.path).to_lowercase())
                }
                _ => None,
            }
        });

        if let Some(arg_name) = arg_name
            && decls
                .iter()
                .all(|d| match &d.parameters.get(i).map(|p| &p.pattern.kind) {
                    Some(ast::node::PatKind::Identifier(ident)) => ident.to_string() == arg_name,
                    Some(ast::node::PatKind::Enum(enum_pattern)) => {
                        get_enum_name(&enum_pattern.path).to_lowercase() == arg_name
                    }
                    _ => true,
                })
        {
            argument_names.push(Some(Ident::new(&arg_name, Default::default())));
        } else {
            argument_names.push(None);
        }
    }

    argument_names
}
