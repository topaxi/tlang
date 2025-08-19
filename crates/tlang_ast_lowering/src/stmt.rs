use std::collections::HashMap;

use log::debug;
use tlang_ast as ast;
use tlang_ast::node::{FunctionDeclaration, Ident, LetDeclaration};
use tlang_hir::hir;

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
                let decl = self.lower_fn_decl(decl);

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
                    name: decl.name.clone(),
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

            // This is a no-op, we do not emit any statement for this.
            // We might be losing some comments here, but this shouldn't be a problem within
            // HIR.
            ast::node::StmtKind::None => vec![],
        }
    }

    fn lower_fn_decls(
        &mut self,
        node: &ast::node::Stmt,
        decls: &[FunctionDeclaration],
    ) -> Vec<hir::Stmt> {
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
            let mut fn_variant_ids = decls
                .iter()
                .map(|decl| (decl.parameters.len(), self.lower_node_id(decl.id)))
                .collect::<Vec<_>>();
            fn_variant_ids.sort_by_key(|(arg_len, _)| *arg_len);
            fn_variant_ids.dedup_by_key(|(arg_len, _)| *arg_len);

            let mut grouped_decls = vec![];
            for (i, (arg_len, _hir_id)) in fn_variant_ids.iter().enumerate() {
                let decls = decls
                    .iter()
                    .filter(|decl| decl.parameters.len() == *arg_len)
                    .cloned()
                    .map(|mut decl| {
                        match &mut decl.name.kind {
                            ast::node::ExprKind::Path(path) => {
                                let ident = path.segments.last_mut().unwrap();
                                ident.set_name(&format!("{}/{}", ident.as_str(), arg_len));
                            }
                            ast::node::ExprKind::FieldExpression(fe) => {
                                let ident = &mut fe.field;
                                ident.set_name(&format!("{}/{}", ident.as_str(), arg_len));
                            }
                            _ => unreachable!(),
                        };
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

                grouped_decls.push(hir::Stmt {
                    // TODO: Hope this will not mess with us in the future, we generate
                    // more statements than initially in the AST, so we are not able to
                    // lower the original node id here.
                    hir_id: self.unique_id(),
                    kind: hir::StmtKind::FunctionDeclaration(Box::new(decl)),
                    span: node.span,
                    leading_comments,
                    trailing_comments: vec![],
                });
            }

            let fn_name = self.lower_expr(&first_declaration.name);

            let dyn_fn_decl = hir::DynFunctionDeclaration {
                hir_id: self.unique_id(),
                name: fn_name,
                variants: fn_variant_ids,
            };

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
            name: decl.name.clone(),
            variants: decl
                .variants
                .iter()
                .map(|variant| hir::EnumVariant {
                    hir_id: self.lower_node_id(variant.id),
                    name: variant.name.clone(),
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

    fn lower_struct_field(&mut self, field: &ast::node::StructField) -> hir::StructField {
        hir::StructField {
            hir_id: self.lower_node_id(field.id),
            name: field.name.clone(),
            ty: self.lower_ty(Some(&field.ty)),
        }
    }

    fn lower_fn_decl_matching(
        &mut self,
        decls: &[FunctionDeclaration],
        all_param_names: &[Option<Ident>],
        leading_comments: &[ast::token::Token],
    ) -> hir::FunctionDeclaration {
        if decls.len() == 1 {
            return self.lower_fn_decl(&decls[0]);
        }

        let mut span = decls[0].span;
        span.end = decls.last().unwrap().span.end;

        let first_declaration = &decls[0];
        let hir_id = self.unique_id();

        for decl in decls {
            self.node_id_to_hir_id.insert(decl.id, hir_id);
        }

        let param_names = get_param_names(decls)
            .iter()
            .enumerate()
            .map(|(i, param_name)| {
                if let Some(param_name) = param_name {
                    param_name.clone()
                } else if let Some(Some(ident)) = all_param_names.get(i) {
                    ident.clone()
                } else {
                    Ident::new(&format!("arg{i}"), Default::default())
                }
            })
            .collect::<Vec<_>>();

        let params = param_names
            .iter()
            .map(|ident| {
                let hir_id = self.unique_id();

                hir::FunctionParameter {
                    hir_id,
                    name: ident.clone(),
                    type_annotation: hir::Ty::default(),
                    span: ident.span,
                }
            })
            .collect::<Vec<_>>();

        let match_value = if params.len() > 1 {
            let argument_list = hir::ExprKind::List(
                params
                    .iter()
                    .map(|param| {
                        let mut path = hir::Path::new(
                            vec![hir::PathSegment {
                                ident: param.name.clone(),
                            }],
                            span,
                        );

                        path.res.set_hir_id(param.hir_id);

                        self.expr(span, hir::ExprKind::Path(Box::new(path)))
                    })
                    .collect(),
            );

            self.expr(tlang_span::Span::default(), argument_list)
        } else {
            let first_param = params.first().unwrap();
            let ident = first_param.name.clone();
            let mut path = hir::Path::new(vec![hir::PathSegment { ident }], span);

            path.res.set_hir_id(first_param.hir_id);

            self.expr(
                tlang_span::Span::default(),
                hir::ExprKind::Path(Box::new(path)),
            )
        };

        // Create hir::FunctionDeclaration with an empty block, and fill out the
        // parameters, for each parameter/argument we reuse the existing plain
        // identifier if it exists, otherwise we create a new one which will be reused
        // in a match expression in the resulting block.
        let mut idents = HashMap::new();
        let match_arms = decls
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
            .collect();

        hir::FunctionDeclaration::new(
            hir_id,
            self.lower_expr(&first_declaration.name),
            params,
            hir::Block::new(
                self.unique_id(),
                vec![],
                Some(hir::Expr {
                    hir_id: self.unique_id(),
                    kind: hir::ExprKind::Match(Box::new(match_value), match_arms),
                    span: tlang_span::Span::default(),
                }),
                tlang_span::Span::default(),
            ),
        )
    }

    fn lower_fn_decl_to_match_arm(
        &mut self,
        decl: &FunctionDeclaration,
        idents: &mut HashMap<String, String>,
    ) -> hir::MatchArm {
        self.with_scope(decl.id, |this| {
            // Mapping argument pattern and signature guard into a match arm
            let pat = if decl.parameters.len() > 1 {
                hir::Pat {
                    kind: hir::PatKind::List(
                        decl.parameters
                            .iter()
                            // We lose type information of each declaration here, as we
                            // lower a whole FunctionParameter into a pattern. They
                            // should probably match for each declaration and we might want
                            // to verify this now or earlier in the pipeline.
                            // Ignored for now as types are basically a NOOP everywhere.
                            .map(|param| this.lower_pat_with_idents(&param.pattern, idents))
                            .collect(),
                    ),
                    span: decl.span,
                }
            } else {
                this.lower_pat(&decl.parameters[0].pattern)
            };

            let guard = decl.guard.as_ref().map(|expr| this.lower_expr(expr));

            let body = this.lower_block_in_current_scope(&decl.body);

            hir::MatchArm {
                pat,
                guard,
                block: body,
                leading_comments: decl.leading_comments.clone(),
                trailing_comments: decl.trailing_comments.clone(),
            }
        })
    }
}

fn get_enum_name(path: &ast::node::Path) -> String {
    path.segments[path.segments.len() - 2].to_string()
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

        if arg_name.is_some()
            && decls
                .iter()
                .all(|d| match &d.parameters.get(i).map(|p| &p.pattern.kind) {
                    Some(ast::node::PatKind::Identifier(ident)) => {
                        Some(ident.to_string()) == arg_name
                    }
                    Some(ast::node::PatKind::Enum(enum_pattern)) => {
                        Some(get_enum_name(&enum_pattern.path).to_lowercase()) == arg_name
                    }
                    _ => true,
                })
        {
            #[allow(clippy::unnecessary_unwrap)]
            argument_names.push(Some(Ident::new(&arg_name.unwrap(), Default::default())));
        } else {
            argument_names.push(None);
        };
    }

    argument_names
}
