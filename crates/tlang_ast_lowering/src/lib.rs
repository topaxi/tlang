#![feature(box_patterns)]
#![feature(if_let_guard)]
use std::collections::HashMap;

use log::debug;
use tlang_ast as ast;
use tlang_ast::node::{
    EnumPattern, FunctionDeclaration, Ident, LetDeclaration,
};
use tlang_ast::token::kw;
use tlang_hir::hir::{self, HirId};

use self::scope::Scope;

mod expr;
mod scope;

// TODO: Add scopes and variable resolutions. There should be two kinds of bindings, one for a
// whole block (declarations) and one for variable definitions (from definition forward).
// See: https://github.com/rust-lang/rust/blob/de7cef75be8fab7a7e1b4d5bb01b51b4bac925c3/compiler/rustc_resolve/src/lib.rs#L408
pub struct LoweringContext {
    unique_id: HirId,
    node_id_to_hir_id: HashMap<ast::node_id::NodeId, HirId>,
    scopes: Vec<Scope>,
}

impl LoweringContext {
    pub fn new() -> Self {
        Self {
            unique_id: HirId::new(1),
            node_id_to_hir_id: HashMap::default(),
            scopes: vec![Scope::default()],
        }
    }

    #[inline(always)]
    pub(crate) fn scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn has_binding(&self, name: &str) -> bool {
        self.scopes
            .iter()
            .rev()
            .any(|scope| scope.lookup(name).is_some())
    }

    pub(crate) fn lookup_name(&mut self, name: &str) -> String {
        self.lookup(name)
            .map_or(name.to_string(), |binding| binding.name().to_string())
    }

    pub(crate) fn lookup<'a>(&'a mut self, name: &'a str) -> Option<scope::Binding> {
        let (scope_index, binding) = self
            .scopes
            .iter()
            .enumerate()
            .rev()
            .find_map(|(i, scope)| scope.lookup(name).map(|binding| (i, binding.clone())))
            .or_else(|| {
                Some((
                    usize::MAX,
                    self.scopes
                        .iter()
                        .rev()
                        .find_map(|scope| scope.lookup_definition(name))?
                        .clone(),
                ))
            })?;

        if scope_index < self.scopes.len() - 1 {
            let relative_scope_index = self.scopes.len() - 1 - scope_index;
            let slot_index = binding.res().slot_index().unwrap();

            return Some(self.scopes.last_mut().unwrap().def_upvar(
                binding.name(),
                relative_scope_index,
                slot_index,
            ));
        }

        Some(binding)
    }

    pub(crate) fn with_new_scope<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
        R: hir::HirScope,
    {
        debug!("Entering new scope");
        self.scopes.push(Scope::new());
        let mut result = f(self);
        result.set_locals(self.scope().locals());
        result.set_upvars(self.scope().upvars());
        debug!("Leaving scope");
        self.scopes.pop();
        result
    }

    fn unique_id(&mut self) -> HirId {
        let id = self.unique_id;
        self.unique_id = self.unique_id.next();
        id
    }

    #[inline(always)]
    pub(crate) fn expr(&mut self, span: ast::span::Span, kind: hir::ExprKind) -> hir::Expr {
        hir::Expr {
            hir_id: self.unique_id(),
            kind,
            span,
        }
    }

    fn lower_node_id(&mut self, id: ast::node_id::NodeId) -> HirId {
        debug_assert!(id != ast::node_id::NodeId::new(0));

        if let Some(hir_id) = self.node_id_to_hir_id.get(&id) {
            *hir_id
        } else {
            let hir_id = self.unique_id();
            self.node_id_to_hir_id.insert(id, hir_id);
            hir_id
        }
    }

    pub fn lower_module(&mut self, module: &ast::node::Module) -> hir::Module {
        self.with_new_scope(|this| this.lower_module_in_current_scope(module))
    }

    pub fn lower_module_in_current_scope(&mut self, module: &ast::node::Module) -> hir::Module {
        debug!(
            "Lowering module with {} statements",
            module.statements.len()
        );

        hir::Module {
            block: self.lower_block_in_current_scope(&module.statements, None, module.span),
            span: module.span,
        }
    }

    fn lower_block(
        &mut self,
        stmts: &[ast::node::Stmt],
        expr: Option<&ast::node::Expr>,
        span: ast::span::Span,
    ) -> hir::Block {
        self.with_new_scope(|this| this.lower_block_in_current_scope(stmts, expr, span))
    }

    fn lower_block_in_current_scope(
        &mut self,
        stmts: &[ast::node::Stmt],
        expr: Option<&ast::node::Expr>,
        span: ast::span::Span,
    ) -> hir::Block {
        debug!("Lowering block with {} statements", stmts.len());

        let stmts = stmts
            .iter()
            .flat_map(|stmt| self.lower_stmt(stmt))
            .collect();
        let expr = expr.as_ref().map(|expr| self.lower_expr(expr));
        hir::Block::new(stmts, expr, span)
    }

    fn lower_stmt(&mut self, node: &ast::node::Stmt) -> Vec<hir::Stmt> {
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
                let hir_id = self.lower_node_id(decl.id);

                self.def_fn_local(&decl.name, hir_id, None);

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
            ast::node::StmtKind::FunctionDeclarations(decls) => {
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

                    for (arg_len, hir_id) in &fn_variant_ids {
                        self.def_fn_local(&first_declaration.name, *hir_id, Some(*arg_len));
                    }

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
                                        ident.set_name(&format!("{}$${}", ident.as_str(), arg_len));
                                    }
                                    ast::node::ExprKind::FieldExpression(fe) => {
                                        let ident = &mut fe.field;
                                        ident.set_name(&format!("{}$${}", ident.as_str(), arg_len));
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

                    let hir_id = self.lower_node_id(first_declaration.id);

                    self.def_fn_local(&first_declaration.name, hir_id, None);

                    let hir_fn_decl = self.lower_fn_decl_matching(
                        decls,
                        &all_param_names,
                        &node.leading_comments,
                    );

                    vec![hir::Stmt {
                        hir_id: self.lower_node_id(node.id),
                        kind: hir::StmtKind::FunctionDeclaration(Box::new(hir_fn_decl)),
                        span: node.span,
                        leading_comments,
                        trailing_comments: node.trailing_comments.clone(),
                    }]
                }
            }
            ast::node::StmtKind::Return(box expr) => {
                let expr = expr.as_ref().map(|expr| self.lower_expr(expr));

                vec![hir::Stmt {
                    hir_id: self.lower_node_id(node.id),
                    kind: hir::StmtKind::Return(Box::new(expr)),
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

                self.scope()
                    .def_struct_local(decl.name.as_str(), decl.hir_id);

                vec![hir::Stmt {
                    hir_id: self.lower_node_id(node.id),
                    kind: hir::StmtKind::StructDeclaration(Box::new(decl)),
                    span: node.span,
                    leading_comments: node.leading_comments.clone(),
                    trailing_comments: node.trailing_comments.clone(),
                }]
            }
            ast::node::StmtKind::EnumDeclaration(decl) => {
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

                self.scope().def_enum_local(decl.name.as_str(), decl.hir_id);

                for variant in &decl.variants {
                    // Variants with no params are values, they need a slot in the current scope.
                    if variant.parameters.is_empty() {
                        self.scope().def_untagged_enum_variant_local(
                            decl.name.as_str(),
                            variant.name.as_str(),
                            variant.hir_id,
                        );
                    } else {
                        self.scope().def_tagged_enum_variant_local(
                            decl.name.as_str(),
                            variant.name.as_str(),
                            variant.hir_id,
                        );
                    }
                }

                vec![hir::Stmt {
                    hir_id: self.lower_node_id(node.id),
                    kind: hir::StmtKind::EnumDeclaration(Box::new(decl)),
                    span: node.span,
                    leading_comments: node.leading_comments.clone(),
                    trailing_comments: node.trailing_comments.clone(),
                }]
            }
            ast::node::StmtKind::None => {
                vec![hir::Stmt {
                    hir_id: self.lower_node_id(node.id),
                    kind: hir::StmtKind::None,
                    span: node.span,
                    leading_comments: node.leading_comments.clone(),
                    trailing_comments: node.trailing_comments.clone(),
                }]
            }
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

        self.with_new_scope(|this| {
            let mut span = decls[0].span;
            span.end = decls.last().unwrap().span.end;

            let first_declaration = &decls[0];
            let hir_id = this.lower_node_id(first_declaration.id);

            this.def_fn_local(&first_declaration.name, hir_id, None);

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

            let mut idents = HashMap::new();

            // Create hir::FunctionDeclaration with an empty block, and fill out the
            // parameters, for each parameter/argument we reuse the existing plain
            // identifier if it exists, otherwise we create a new one which will be reused
            // in a match expression in the resulting block.
            let mut hir_fn_decl = hir::FunctionDeclaration::new_empty_fn(
                hir_id,
                this.lower_expr(&first_declaration.name),
                param_names
                    .iter()
                    .map(|ident| {
                        let hir_id = this.unique_id();

                        this.create_fn_param(hir_id, ident.clone(), hir::Ty::default(), ident.span)
                    })
                    .collect(),
            );

            let mut match_arms = Vec::with_capacity(decls.len());

            for (i, decl) in decls.iter().enumerate() {
                let match_arm = this.with_new_scope(|this| {
                    // All declarations with the same amount of arguments refer to the same
                    // function now, we map this in our symbol_id_to_hir_id table.
                    this.node_id_to_hir_id.insert(decl.id, hir_id);

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
                                    .map(|param| {
                                        this.lower_pat_with_idents(&param.pattern, &mut idents)
                                    })
                                    .collect(),
                            ),
                            span: decl.span,
                        }
                    } else {
                        this.lower_pat(&decl.parameters[0].pattern)
                    };

                    let guard = decl.guard.as_ref().map(|expr| this.lower_expr(expr));

                    let body = this.lower_block_in_current_scope(
                        &decl.body.statements,
                        decl.body.expression.as_ref(),
                        decl.body.span,
                    );

                    let mut arm_leading_comments = vec![];
                    if i == 0 {
                        arm_leading_comments.extend(leading_comments.iter().cloned());
                    }
                    arm_leading_comments.extend(decl.leading_comments.clone());

                    hir::MatchArm {
                        pat,
                        guard,
                        block: body,
                        leading_comments: arm_leading_comments,
                        trailing_comments: decl.trailing_comments.clone(),
                    }
                });

                match_arms.push(match_arm);
            }

            let match_value = if param_names.len() > 1 {
                let argument_list = hir::ExprKind::List(
                    param_names
                        .iter()
                        .map(|ident| {
                            let res = this.lookup(ident.as_str()).unwrap().res();
                            let path = hir::Path::new(
                                vec![hir::PathSegment {
                                    ident: ident.clone(),
                                }],
                                span,
                            )
                            .with_res(res);

                            this.expr(span, hir::ExprKind::Path(Box::new(path)))
                        })
                        .collect(),
                );

                this.expr(ast::span::Span::default(), argument_list)
            } else {
                let ident = param_names.first().unwrap().clone();
                let res = this.lookup(ident.as_str()).unwrap().res();
                let path = hir::Path::new(vec![hir::PathSegment { ident }], span).with_res(res);

                this.expr(
                    ast::span::Span::default(),
                    hir::ExprKind::Path(Box::new(path)),
                )
            };

            hir_fn_decl.body.expr = Some(hir::Expr {
                hir_id: this.unique_id(),
                kind: hir::ExprKind::Match(Box::new(match_value), match_arms),
                span: ast::span::Span::default(),
            });

            hir_fn_decl
        })
    }

    fn lower_fn_param_pat(&mut self, node: &ast::node::FunctionParameter) -> Ident {
        match &node.pattern.kind {
            ast::node::PatKind::Identifier(box ident) => ident.clone(),
            ast::node::PatKind::_Self => Ident::new(kw::_Self, node.span),
            ast::node::PatKind::Wildcard => Ident::new(kw::Underscore, node.span),
            _ => {
                unimplemented!(
                    "Only identifier patterns are supported for function parameters, found {:?}",
                    node.pattern.kind
                )
            }
        }
    }

    fn create_fn_param(
        &mut self,
        hir_id: HirId,
        name: Ident,
        ty: hir::Ty,
        span: ast::span::Span,
    ) -> hir::FunctionParameter {
        self.scope().def_local(name.as_str(), hir_id);

        hir::FunctionParameter {
            hir_id,
            name,
            type_annotation: ty,
            span,
        }
    }

    fn lower_fn_param(&mut self, node: &ast::node::FunctionParameter) -> hir::FunctionParameter {
        let hir_id = self.lower_node_id(node.pattern.id);
        let name = self.lower_fn_param_pat(node);
        let ty = self.lower_ty(node.type_annotation.as_ref());

        self.create_fn_param(hir_id, name, ty, node.span)
    }

    fn def_fn_local(&mut self, name_expr: &ast::node::Expr, hir_id: HirId, arity: Option<usize>) {
        let mut fn_name = match &name_expr.kind {
            ast::node::ExprKind::Path(path) => path.join("::"),
            ast::node::ExprKind::FieldExpression(fe) => {
                let ast::node::ExprKind::Path(path) = &fe.base.kind else {
                    unreachable!("FieldExpression base should be a path");
                };

                path.join("::") + fe.field.as_str()
            }
            _ => unreachable!(),
        };

        if let Some(arity) = arity {
            fn_name += &format!("$${}", arity);
        }

        self.scope().def_fn_local(fn_name.as_str(), hir_id);
    }

    fn lower_fn_decl(&mut self, decl: &FunctionDeclaration) -> hir::FunctionDeclaration {
        self.with_new_scope(|this| {
            let hir_id = this.lower_node_id(decl.id);

            this.def_fn_local(&decl.name, hir_id, None);

            let name = this.lower_expr(&decl.name);

            let parameters = decl
                .parameters
                .iter()
                .map(|param| this.lower_fn_param(param))
                .collect();
            let body = this.lower_block_in_current_scope(
                &decl.body.statements,
                decl.body.expression.as_ref(),
                decl.body.span,
            );
            let return_type = this.lower_ty(decl.return_type_annotation.as_ref());

            hir::FunctionDeclaration {
                hir_id,
                name,
                parameters,
                return_type,
                body,
                span: decl.span,
            }
        })
    }

    fn lower_path(&mut self, path: &ast::node::Path) -> hir::Path {
        let res = self
            .lookup(&path.join("::"))
            .map_or(hir::Res::Unknown, |binding| binding.res());

        if path.segments.len() == 1 {
            let segment = path.segments.first().unwrap();
            let segment =
                hir::PathSegment::from_str(&self.lookup_name(segment.as_str()), segment.span);

            return hir::Path::new(vec![segment], path.span).with_res(res);
        }

        let segments = path
            .segments
            .iter()
            .map(|seg| self.lower_path_segment(seg))
            .collect();

        hir::Path::new(segments, path.span).with_res(res)
    }

    fn lower_path_segment(&mut self, seg: &Ident) -> hir::PathSegment {
        hir::PathSegment { ident: seg.clone() }
    }

    /// Lower a pattern into a HIR pattern. Only use for single patterns, not match arms or
    /// function matching.
    fn lower_pat(&mut self, node: &ast::node::Pat) -> hir::Pat {
        debug!("Lowering pattern {:?}", node.kind);

        self.lower_pat_with_idents(node, &mut HashMap::new())
    }

    // We only create one binding for each identifier found, especially when used multiple
    // times. As then each of them have to match the same pattern/value.
    fn lower_pat_with_idents(
        &mut self,
        node: &ast::node::Pat,
        _idents: &mut HashMap<String, String>,
    ) -> hir::Pat {
        match &node.kind {
            ast::node::PatKind::Wildcard => hir::Pat {
                kind: hir::PatKind::Wildcard,
                span: node.span,
            },
            ast::node::PatKind::Literal(box literal) => hir::Pat {
                kind: hir::PatKind::Literal(Box::new(literal.clone())),
                span: node.span,
            },
            ast::node::PatKind::Identifier(box ident) => {
                let hir_id = self.lower_node_id(node.id);

                self.scope().def_local(ident.as_str(), hir_id);
                // TODO: As mentioned above, create_unique_binding was supposed to help with
                // resolving shadowed variables by giving them unique names. Due to some
                // regressions as this was a too generic solution, this is disabled. As a first
                // step we might only apply this for let declarations.
                //let ident = if let Some(binding) = idents.get(ident.as_str()) {
                //    Ident::new(binding, ident.span)
                //} else {
                //    let binding = self.create_unique_binding(ident.as_str());
                //    idents.insert(ident.to_string(), binding.clone());
                //    Ident::new(&binding, ident.span)
                //};

                hir::Pat {
                    kind: hir::PatKind::Identifier(hir_id, Box::new(ident.clone())),
                    span: node.span,
                }
            }
            ast::node::PatKind::List(patterns) => hir::Pat {
                kind: hir::PatKind::List(patterns.iter().map(|pat| self.lower_pat(pat)).collect()),
                span: node.span,
            },
            ast::node::PatKind::Rest(pattern) => hir::Pat {
                kind: hir::PatKind::Rest(Box::new(self.lower_pat(pattern))),
                span: node.span,
            },
            ast::node::PatKind::Enum(box EnumPattern { path, elements }) => {
                let path = self.lower_path(path);
                let elements = elements
                    .iter()
                    .map(|(ident, pat)| (ident.clone(), self.lower_pat(pat)))
                    .collect();

                hir::Pat {
                    kind: hir::PatKind::Enum(Box::new(path), elements),
                    span: node.span,
                }
            }
            ast::node::PatKind::_Self => hir::Pat {
                kind: hir::PatKind::Identifier(
                    self.lower_node_id(node.id),
                    Box::new(Ident::new(kw::_Self, node.span)),
                ),
                span: node.span,
            },
            ast::node::PatKind::None => {
                unreachable!("PatternKind::None should not be encountered, validate AST first")
            }
        }
    }

    fn lower_ty(&mut self, node: Option<&ast::node::Ty>) -> hir::Ty {
        debug!("Lowering type {:?}", node);

        if let Some(node) = node {
            hir::Ty {
                kind: hir::TyKind::Path(self.lower_path(&node.name)),
                span: node.span,
            }
        } else {
            hir::Ty {
                kind: hir::TyKind::Unknown,
                span: ast::span::Span::default(),
            }
        }
    }
}

impl Default for LoweringContext {
    fn default() -> Self {
        Self::new()
    }
}

fn get_enum_name(path: &ast::node::Path) -> String {
    path.segments[path.segments.len() - 2].to_string()
}

pub fn lower_to_hir(tlang_ast: &ast::node::Module) -> hir::Module {
    let mut ctx = LoweringContext::new();
    ctx.lower_module(tlang_ast)
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
