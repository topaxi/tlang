use std::collections::HashMap;

use log::debug;
use tlang_ast as ast;
use tlang_ast::node::{ConstDeclaration, FunctionDeclaration, Ident, LetDeclaration};
use tlang_defs::DefKind;
use tlang_hir as hir;

use crate::{LoweringContext, ProtocolDispatchContext};

fn same_ast_ty_shape(lhs: &ast::node::Ty, rhs: &ast::node::Ty) -> bool {
    if lhs.parameters.len() != rhs.parameters.len() {
        return false;
    }

    if !lhs
        .parameters
        .iter()
        .zip(rhs.parameters.iter())
        .all(|(lhs, rhs)| same_ast_ty_shape(lhs, rhs))
    {
        return false;
    }

    match (&lhs.kind, &rhs.kind) {
        (ast::node::TyKind::Unknown, ast::node::TyKind::Unknown) => true,
        (ast::node::TyKind::Path(lhs_path), ast::node::TyKind::Path(rhs_path)) => {
            lhs_path.join("::") == rhs_path.join("::")
        }
        (ast::node::TyKind::Union(lhs_paths), ast::node::TyKind::Union(rhs_paths)) => {
            lhs_paths.len() == rhs_paths.len()
                && lhs_paths
                    .iter()
                    .zip(rhs_paths.iter())
                    .all(|(lhs_path, rhs_path)| lhs_path.join("::") == rhs_path.join("::"))
        }
        (
            ast::node::TyKind::Fn(lhs_params, lhs_ret),
            ast::node::TyKind::Fn(rhs_params, rhs_ret),
        ) => {
            lhs_params.len() == rhs_params.len()
                && lhs_params
                    .iter()
                    .zip(rhs_params.iter())
                    .all(|(lhs_param, rhs_param)| same_ast_ty_shape(&lhs_param.ty, &rhs_param.ty))
                && same_ast_ty_shape(lhs_ret, rhs_ret)
        }
        _ => false,
    }
}

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
            ast::node::StmtKind::Const(box ConstDeclaration {
                id: const_id,
                visibility,
                name,
                expression,
                type_annotation,
                ..
            }) => {
                let expr = self.lower_expr(expression);
                let ty = self.lower_ty(type_annotation.as_ref());
                let pat = self.lower_ident_pat(*const_id, name);

                vec![hir::Stmt {
                    hir_id: self.lower_node_id(node.id),
                    kind: hir::StmtKind::Const(
                        *visibility,
                        Box::new(pat),
                        Box::new(expr),
                        Box::new(ty),
                    ),
                    span: node.span,
                    leading_comments: node.leading_comments.clone(),
                    trailing_comments: node.trailing_comments.clone(),
                }]
            }
            ast::node::StmtKind::FunctionDeclaration(box decl) => {
                if has_complex_param_patterns(decl) || decl.guard.is_some() {
                    return self.lower_fn_decls(node, std::slice::from_ref(decl));
                }

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
                vec![self.lower_struct_decl(node, decl)]
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
            // Preserve source order (order of first occurrence per arity) so that
            // HIR emit order matches the symbol-table insertion order that
            // SlotAllocator uses. Sorting by arity here would invert the two
            // orderings and cause slot-index mismatches at runtime.
            let mut seen_arities = std::collections::HashSet::new();
            let fn_variants: Vec<usize> = decls
                .iter()
                .map(|decl| decl.parameters.len())
                .filter(|arity| seen_arities.insert(*arity))
                .collect();

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
            // FIXME: `scope_start = 0` is a workaround for a span-unit mismatch.
            //
            // `get_closest_by_name` compares `scope_start < span.start_lc.line` (line
            // numbers), but `define_symbol_after` historically received byte offsets from
            // `first_declaration.span.start`.  Passing 0 makes the DynFunctionDeclaration
            // visible from any line, so callers can always find it — but it also means a
            // reference that textually precedes the first clause would resolve to the dyn
            // dispatch node rather than being rejected, which is semantically wrong.
            //
            // The correct value to pass would be the **end** of the last declaration
            // (i.e. `last_declaration.span.end_lc.line`): a DynFunctionDeclaration
            // logically spans from the first clause to the last, so any reference after
            // that final clause should find it, but references before it should not.
            // `get_closest_*` uses start spans for proximity comparisons, and the dyn
            // node's own span already covers first-to-last, so using the end line of the
            // last declaration as `scope_start` would be semantically accurate.
            self.define_symbol_after(
                dyn_fn_decl.hir_id,
                &fn_name_str,
                DefKind::Variable, // TODO, add symbol type for dyn dispatch functions
                0,
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

    fn lower_struct_decl(
        &mut self,
        node: &ast::node::Stmt,
        decl: &ast::node::StructDeclaration,
    ) -> hir::Stmt {
        let type_params = self.lower_type_params(&decl.type_params);
        let hir_decl = hir::StructDeclaration {
            hir_id: self.lower_node_id(node.id),
            visibility: decl.visibility,
            name: decl.name,
            type_params,
            fields: decl
                .fields
                .iter()
                .map(|field| self.lower_struct_field(field))
                .collect(),
            consts: decl
                .consts
                .iter()
                .map(|c| self.lower_const_item(c))
                .collect(),
        };
        self.pop_type_param_scope();

        hir::Stmt {
            hir_id: self.lower_node_id(node.id),
            kind: hir::StmtKind::StructDeclaration(Box::new(hir_decl)),
            span: node.span,
            leading_comments: node.leading_comments.clone(),
            trailing_comments: node.trailing_comments.clone(),
        }
    }

    fn lower_enum_decl(
        &mut self,
        node: &ast::node::Stmt,
        decl: &ast::node::EnumDeclaration,
    ) -> hir::Stmt {
        let type_params = self.lower_type_params(&decl.type_params);
        let decl = hir::EnumDeclaration {
            hir_id: self.lower_node_id(node.id),
            visibility: decl.visibility,
            name: decl.name,
            type_params,
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
                    discriminant: variant
                        .discriminant
                        .as_deref()
                        .map(|expr| Box::new(self.lower_expr(expr))),
                    span: variant.span,
                })
                .collect::<Vec<_>>(),
            consts: decl
                .consts
                .iter()
                .map(|c| self.lower_const_item(c))
                .collect(),
        };
        self.pop_type_param_scope();

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
        // Push type parameter scope before lowering methods/constraints.
        let type_params = self.lower_type_params(&decl.type_params);

        // Build the method dispatch map for this protocol.  It maps method name →
        // the Ident of the protocol that declares it, covering both this protocol's
        // own methods and all methods from transitively-reachable constraint
        // protocols.  This is used by the implicit self-dispatch rewrite in
        // `lower_call_expr` (see `ProtocolDispatchContext`).
        let current_method_names: Vec<String> = decl
            .methods
            .iter()
            .map(|m| m.name.as_str().to_string())
            .collect();
        let constraint_names: Vec<String> =
            decl.constraints.iter().map(|c| c.to_string()).collect();
        let method_dispatch_map =
            self.build_protocol_dispatch_map(decl.name, &current_method_names, &constraint_names);

        let methods = decl
            .methods
            .iter()
            .map(|method| self.lower_protocol_method_signature(method, &method_dispatch_map))
            .collect();

        let protocol = hir::ProtocolDeclaration {
            hir_id: self.lower_node_id(node.id),
            visibility: decl.visibility,
            name: decl.name,
            type_params,
            constraints: decl
                .constraints
                .iter()
                .map(|c| self.lower_path(c))
                .collect(),
            associated_types: decl
                .associated_types
                .iter()
                .map(|at| self.lower_associated_type_decl(at))
                .collect(),
            methods,
            consts: decl
                .consts
                .iter()
                .map(|c| self.lower_const_item(c))
                .collect(),
        };
        self.pop_type_param_scope();

        hir::Stmt {
            hir_id: self.lower_node_id(node.id),
            kind: hir::StmtKind::ProtocolDeclaration(Box::new(protocol)),
            span: node.span,
            leading_comments: node.leading_comments.clone(),
            trailing_comments: node.trailing_comments.clone(),
        }
    }

    fn lower_protocol_method_signature(
        &mut self,
        method: &ast::node::ProtocolMethodSignature,
        method_dispatch_map: &HashMap<String, Ident>,
    ) -> hir::ProtocolMethodSignature {
        let type_params = self.lower_type_params(&method.type_params);
        let has_body = method.body.is_some();
        let params: Vec<hir::FunctionParameter> = method
            .parameters
            .iter()
            .map(|param| self.lower_protocol_method_param(param, has_body))
            .collect();

        let self_param = method
            .parameters
            .iter()
            .find(|p| matches!(p.pattern.kind, ast::node::PatKind::_Self));

        let method_hir_id = if method.body.is_some() {
            self.lower_node_id(method.id)
        } else {
            self.unique_id()
        };

        let body = if let Some(self_param) = self_param {
            method.body.as_ref().map(|b| {
                let previous_ctx = self.protocol_dispatch_ctx.take();
                self.protocol_dispatch_ctx = Some(ProtocolDispatchContext {
                    method_dispatch_map: method_dispatch_map.clone(),
                    self_param_node_id: self_param.pattern.id,
                });
                let result = self.with_scope(method.id, |this| this.lower_block(b));
                self.protocol_dispatch_ctx = previous_ctx;
                result
            })
        } else {
            method.body.as_ref().map(|b| self.lower_block(b))
        };

        let signature = hir::ProtocolMethodSignature {
            hir_id: method_hir_id,
            name: method.name,
            type_params,
            parameters: params,
            return_type: self.lower_ty(method.return_type_annotation.as_ref()),
            body,
            span: method.span,
        };
        self.pop_type_param_scope();
        signature
    }

    fn lower_protocol_method_param(
        &mut self,
        param: &ast::node::FunctionParameter,
        has_body: bool,
    ) -> hir::FunctionParameter {
        let name = match &param.pattern.kind {
            ast::node::PatKind::Identifier(ident) => *ident.as_ref(),
            ast::node::PatKind::_Self => Ident::new("self", param.pattern.span),
            _ => Ident::new("_", param.pattern.span),
        };
        // For default implementations, use `lower_node_id` to create
        // the NodeId→HirId mapping. `symbol_tables()` will then
        // assign HirIds to the semantic analyzer's existing symbols
        // in this scope, which avoids creating duplicate entries.
        let hir_id = if has_body {
            self.lower_node_id(param.pattern.id)
        } else {
            self.unique_id()
        };
        let has_type_annotation = param.type_annotation.is_some();
        hir::FunctionParameter {
            hir_id,
            name,
            type_annotation: self.lower_ty(param.type_annotation.as_ref()),
            has_type_annotation,
            span: param.pattern.span,
        }
    }

    fn lower_impl_block(
        &mut self,
        node: &ast::node::Stmt,
        impl_block: &ast::node::ImplBlock,
    ) -> hir::Stmt {
        // Lower impl-level type parameters. `lower_type_params` pushes a
        // type param scope automatically; the matching `pop_type_param_scope`
        // call is at the end of this function (after lowering methods).
        let type_params = self.lower_type_params(&impl_block.type_params);

        let protocol_name = self.lower_path(&impl_block.protocol_name);
        let type_arguments: Vec<hir::Ty> = impl_block
            .type_arguments
            .iter()
            .map(|ty| {
                assert!(
                    ty.parameters.is_empty(),
                    "parameterized impl-block protocol type arguments are not yet supported: \
                     lowering would drop nested type arguments from `{:?}` \
                     in `impl {} for {}`",
                    ty.kind,
                    impl_block.protocol_name,
                    impl_block.target_type,
                );
                self.lower_ty(Some(ty))
            })
            .collect();
        let target_type = self.lower_path(&impl_block.target_type);
        let target_type_arguments: Vec<hir::Ty> = impl_block
            .target_type_arguments
            .iter()
            .map(|ty| self.lower_ty(Some(ty)))
            .collect();

        let where_clause = impl_block
            .where_clause
            .as_ref()
            .map(|wc| self.lower_where_clause(wc));

        let associated_types: Vec<hir::AssociatedTypeBinding> = impl_block
            .associated_types
            .iter()
            .map(|at| self.lower_associated_type_binding(at))
            .collect();

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
                if decls.len() == 1 && !has_complex_param_patterns(decls[0]) {
                    self.lower_fn_decl(decls[0])
                } else {
                    let owned: Vec<_> = decls.iter().map(|d| (*d).clone()).collect();
                    let all_param_names = get_param_names(&owned);
                    self.lower_fn_decl_matching(&owned, &all_param_names, &[])
                }
            })
            .collect();

        self.pop_type_param_scope();

        let hir_impl = hir::ImplBlock {
            hir_id: self.lower_node_id(node.id),
            type_params,
            protocol_name,
            type_arguments,
            target_type,
            target_type_arguments,
            where_clause,
            associated_types,
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

    fn lower_associated_type_decl(
        &mut self,
        at: &ast::node::AssociatedTypeDeclaration,
    ) -> hir::AssociatedTypeDeclaration {
        let type_params = self.lower_type_params(&at.type_params);
        let decl = hir::AssociatedTypeDeclaration {
            hir_id: self.lower_node_id(at.id),
            name: at.name,
            type_params,
            span: at.span,
        };
        self.pop_type_param_scope();
        decl
    }

    fn lower_associated_type_binding(
        &mut self,
        at: &ast::node::AssociatedTypeBinding,
    ) -> hir::AssociatedTypeBinding {
        let type_params = self.lower_type_params(&at.type_params);
        let binding = hir::AssociatedTypeBinding {
            hir_id: self.lower_node_id(at.id),
            name: at.name,
            type_params,
            ty: self.lower_ty(Some(&at.ty)),
            span: at.span,
        };
        self.pop_type_param_scope();
        binding
    }

    fn lower_where_clause(&mut self, wc: &ast::node::WhereClause) -> hir::WhereClause {
        hir::WhereClause {
            predicates: wc
                .predicates
                .iter()
                .map(|pred| hir::WherePredicate {
                    name: pred.name,
                    bounds: pred.bounds.iter().map(|b| self.lower_ty(Some(b))).collect(),
                    span: pred.span,
                })
                .collect(),
            span: wc.span,
        }
    }

    fn lower_struct_field(&mut self, field: &ast::node::StructField) -> hir::StructField {
        hir::StructField {
            hir_id: self.lower_node_id(field.id),
            name: field.name,
            ty: self.lower_ty(Some(&field.ty)),
        }
    }

    pub(crate) fn lower_const_item(
        &mut self,
        const_decl: &ast::node::ConstDeclaration,
    ) -> hir::ConstItem {
        hir::ConstItem {
            hir_id: self.lower_node_id(const_decl.id),
            visibility: const_decl.visibility,
            name: const_decl.name,
            value: self.lower_expr(&const_decl.expression),
            ty: self.lower_ty(const_decl.type_annotation.as_ref()),
            span: const_decl.span,
        }
    }

    /// Lower an identifier to a simple identifier pattern (not destructuring).
    /// Used for `const NAME = expr` where the name is always a simple identifier.
    pub(crate) fn lower_ident_pat(
        &mut self,
        node_id: tlang_span::NodeId,
        name: &ast::node::Ident,
    ) -> hir::Pat {
        let hir_id = self.lower_node_id(node_id);
        hir::Pat {
            kind: hir::PatKind::Identifier(hir_id, Box::new(*name)),
            ty: hir::Ty::unknown(),
            span: name.span,
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
                    // Synthesised placeholder — use the source span of the corresponding
                    // parameter in the first clause that has one, so that any inlay hints
                    // derived from this parameter land at the right position rather than
                    // falling back to the default (0, 0) span.
                    let source_span = decls
                        .iter()
                        .find_map(|d| d.parameters.get(i).map(|p| p.span))
                        .unwrap_or_default();
                    Ident::new(&format!("arg{i}"), source_span)
                }
            })
            .collect::<Vec<_>>();

        let params = param_names
            .iter()
            .enumerate()
            .map(|(i, ident)| {
                // Preserve a concrete synthesized parameter type only when every
                // clause agrees on the same annotation/inferred type. Mixed
                // typed/untyped clauses must stay permissive so catch-all arms
                // (e.g. `fn render(v)`) continue to participate in dispatch.
                let type_annotations = decls
                    .iter()
                    .map(|d| d.parameters.get(i).and_then(|p| p.type_annotation.as_ref()))
                    .collect::<Vec<_>>();
                let type_annotation = if type_annotations.len() == decls.len()
                    && let Some(first_annotation) = type_annotations.first().copied().flatten()
                    && type_annotations.iter().all(|annotation| {
                        annotation.is_some_and(|annotation| {
                            same_ast_ty_shape(annotation, first_annotation)
                        })
                    }) {
                    self.lower_ty(Some(first_annotation))
                } else {
                    hir::Ty::default()
                };
                let has_type_annotation = !matches!(type_annotation.kind, hir::TyKind::Unknown);

                hir::FunctionParameter {
                    hir_id: self.unique_id(),
                    name: *ident,
                    type_annotation,
                    has_type_annotation,
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
        if decls.len() == 1 && decls[0].guard.is_none() && !has_complex_param_patterns(&decls[0]) {
            return self.lower_fn_decl(&decls[0]);
        }

        debug!(
            "Lowering multiple function declarations with the same name: {:?} {:?}",
            decls[0].name,
            decls.iter().map(|d| d.id).collect::<Vec<_>>()
        );

        self.with_new_scope(|this, _scope| {
            // Push method-level type parameter scope (e.g. `<U>` from `fn map<U>`)
            // before lowering any type annotations so that references like
            // `fn(T) -> U` in parameter types resolve `U` to `TyKind::Var`.
            // Type params are taken from the first clause — subsequent clauses
            // must use identical params (enforced by convention, not yet validated).
            let type_params = this.lower_type_params(&decls[0].type_params);

            let (hir_id, fn_name, params, span) =
                this.setup_function_declaration_metadata(decls, all_param_names);

            let fn_name_str = this.fn_name_or_error(&decls[0]);
            this.define_function_symbols(hir_id, &fn_name_str, &decls[0], &params);

            let match_value = this.create_match_value(&params, span);

            let match_arms = this.create_match_arms(decls, leading_comments);

            // Inherit the return type from the first clause that has an
            // explicit annotation.  Must happen before `pop_type_param_scope`
            // so that type-parameter references (e.g. `List<T>`) resolve.
            let return_type = decls
                .iter()
                .find_map(|d| d.return_type_annotation.as_ref())
                .map(|ty| this.lower_ty(Some(ty)))
                .unwrap_or_default();

            // Use actual source spans for the synthesised body so that inlay hints
            // (return-type hints in particular) land at the real `{` position rather
            // than the default (0, 0).
            let body_span =
                tlang_span::Span::from_spans(&decls[0].body.span, &decls.last().unwrap().body.span);

            let body = hir::Block::new(
                this.unique_id(),
                vec![],
                Some(hir::Expr {
                    hir_id: this.unique_id(),
                    kind: hir::ExprKind::Match(Box::new(match_value), match_arms),
                    ty: hir::Ty::unknown(),
                    span: body_span,
                }),
                body_span,
            );

            this.pop_type_param_scope();

            (hir_id, {
                let mut decl = hir::FunctionDeclaration::new(hir_id, fn_name, params, body);
                decl.type_params = type_params;
                decl.return_type = return_type;
                decl.has_return_type = decls.iter().any(|d| d.return_type_annotation.is_some());
                decl.return_hint_spans = decls
                    .iter()
                    .filter(|d| d.return_type_annotation.is_none())
                    .map(|d| d.params_span)
                    .collect();
                decl.return_hint_arm_indices = decls
                    .iter()
                    .enumerate()
                    .filter(|(_, d)| d.return_type_annotation.is_none())
                    .map(|(i, _)| i)
                    .collect();
                // Multi-clause functions inherit visibility from the first clause.
                decl.visibility = decls[0].visibility;
                // Use the first clause's params_span so return-type hints land
                // right after the `)` of the representative parameter list.
                decl.params_span = decls[0].params_span;
                // Carry the combined source span so position-based features (e.g. inlay
                // hints) can locate the declaration in the editor.
                decl.span = span;
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
            this.current_symbol_table.write().unwrap().shift();

            // Mapping argument pattern and signature guard into a match arm.
            // Propagate any type annotations inferred by FnParamTypeInference
            // onto the lowered pattern's `ty` field so that the information
            // survives into the HIR.
            let pat = if decl.parameters.len() > 1 {
                let params = decl
                    .parameters
                    .iter()
                    .map(|param| {
                        let mut pat = this.lower_pat_with_idents(&param.pattern, idents);
                        if let Some(ty) = param.type_annotation.as_ref() {
                            pat.ty = this.lower_ty(Some(ty));
                        }
                        pat
                    })
                    .collect();

                hir::Pat {
                    kind: hir::PatKind::List(params),
                    ty: hir::Ty::unknown(),
                    span: decl.span,
                }
            } else {
                let param = &decl.parameters[0];
                let mut pat = this.lower_pat(&param.pattern);
                if let Some(ty) = param.type_annotation.as_ref() {
                    pat.ty = this.lower_ty(Some(ty));
                }
                pat
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
    let n = path.segments.len();
    path.segments[n.saturating_sub(2)].to_string()
}

/// Returns `true` if the function declaration has any parameter with a
/// non-trivial pattern (e.g. enum variant, struct destructuring, literal,
/// list, or tuple patterns). Such patterns require match-dispatch lowering
/// rather than the simple `lower_fn_decl` fast path which only handles
/// identifiers, `self`, and wildcards.
fn has_complex_param_patterns(decl: &FunctionDeclaration) -> bool {
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
            // Use the source span of the identifier from whichever clause defines it, so
            // that any inlay hints derived from this parameter land at the right location.
            // For Enum/struct patterns (e.g. `Page { title }`) use the full parameter
            // span so the synthesised name ident's end position is after the closing `}`.
            let source_span = decls
                .iter()
                .find_map(
                    |d| match d.parameters.get(i).map(|p| (&p.pattern.kind, p.span)) {
                        Some((ast::node::PatKind::Identifier(ident), _)) => Some(ident.span),
                        Some((ast::node::PatKind::Enum(_), param_span)) => Some(param_span),
                        _ => None,
                    },
                )
                .unwrap_or_default();
            argument_names.push(Some(Ident::new(&arg_name, source_span)));
        } else {
            argument_names.push(None);
        }
    }

    argument_names
}
