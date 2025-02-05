#![feature(box_patterns)]
#![feature(if_let_guard)]
use std::collections::HashMap;

use log::debug;
use tlang_ast as ast;
use tlang_ast::node::{
    BinaryOpExpression, BinaryOpKind, EnumPattern, FunctionDeclaration, Ident, LetDeclaration,
};
use tlang_ast::token::kw;
use tlang_hir::hir::{self, HirId};

use self::scope::Scope;

mod scope;

// TODO: Add scopes and variable resolutions. There should be two kinds of bindings, one for a
// whole block (declarations) and one for variable definitions (from definition forward).
// See: https://github.com/rust-lang/rust/blob/de7cef75be8fab7a7e1b4d5bb01b51b4bac925c3/compiler/rustc_resolve/src/lib.rs#L408
pub struct LoweringContext {
    unique_id: HirId,
    node_id_to_hir_id: std::collections::HashMap<ast::node_id::NodeId, HirId>,
    scopes: Vec<Scope>,
}

impl LoweringContext {
    pub fn new() -> Self {
        Self {
            unique_id: HirId::new(0),
            node_id_to_hir_id: std::collections::HashMap::default(),
            scopes: vec![Scope::default()],
        }
    }

    #[inline(always)]
    pub(crate) fn push_scope(&mut self) {
        debug!("Entering new scope");

        self.scopes.push(Scope::new());
    }

    #[inline(always)]
    pub(crate) fn pop_scope(&mut self) {
        debug!("Leaving scope");

        self.scopes.pop();
    }

    #[inline(always)]
    pub(crate) fn scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    #[inline(always)]
    pub(crate) fn create_binding(&mut self, name: &str) {
        debug!("Creating binding for {}", name);

        self.scope().insert(name, name)
    }

    /// Create a unique binding for the given name. If the name already exists, a new unique
    /// name is created by appending a number to the name. This was used to distinguish shadowed
    /// names to make variables more explicit, which should also help in codegen. Due to some
    /// awkward regressions and edge cases, this function is currently not used anymore.
    #[must_use]
    #[allow(dead_code)]
    pub(crate) fn create_unique_binding(&mut self, name: &str) -> String {
        if !self.has_binding(name) {
            self.create_binding(name);
            return name.to_string();
        }

        let mut prefix: usize = 0;
        let mut binding = format!("{}${}", name, prefix);
        while self.has_binding(&binding) {
            prefix += 1;
            binding = format!("{}${}", name, prefix);
        }

        self.create_binding(&binding);
        // Alias the binding to the new explicit name.
        self.scope().insert(name, &binding);
        binding
    }

    fn has_binding(&self, name: &str) -> bool {
        for scope in self.scopes.iter().rev() {
            if scope.lookup(name).is_some() {
                return true;
            }
        }

        false
    }

    pub(crate) fn lookup<'a>(&'a self, name: &'a str) -> &'a str {
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.lookup(name) {
                return binding;
            }
        }

        name
    }

    pub(crate) fn with_new_scope<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        self.push_scope();
        let result = f(self);
        self.pop_scope();
        result
    }

    fn unique_id(&mut self) -> HirId {
        let id = self.unique_id;
        self.unique_id = self.unique_id.next();
        id
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
        hir::Block { stmts, expr, span }
    }

    fn lower_exprs(&mut self, exprs: &[ast::node::Expr]) -> Vec<hir::Expr> {
        exprs.iter().map(|expr| self.lower_expr(expr)).collect()
    }

    fn lower_expr(&mut self, node: &ast::node::Expr) -> hir::Expr {
        debug!("Lowering expression {:?}", node.kind);

        let kind = match &node.kind {
            ast::node::ExprKind::BinaryOp(binary_expr) => self.lower_binary_expr(binary_expr),
            ast::node::ExprKind::Block(box ast::node::Block {
                id: _,
                statements,
                expression,
                span,
            }) => hir::ExprKind::Block(Box::new(self.lower_block(
                statements,
                expression.as_ref(),
                *span,
            ))),
            ast::node::ExprKind::Call(call_expr) => {
                hir::ExprKind::Call(Box::new(self.lower_call_expr(call_expr)))
            }
            ast::node::ExprKind::RecursiveCall(call_expr) => {
                hir::ExprKind::TailCall(Box::new(self.lower_call_expr(call_expr)))
            }
            ast::node::ExprKind::UnaryOp(op, expr) => {
                let expr = self.lower_expr(expr);
                hir::ExprKind::Unary(*op, Box::new(expr))
            }
            ast::node::ExprKind::Path(path) => {
                let path = self.lower_path(path);
                hir::ExprKind::Path(Box::new(path))
            }
            ast::node::ExprKind::FunctionExpression(decl) => {
                let decl = self.lower_fn_decl(decl);
                hir::ExprKind::FunctionExpression(Box::new(decl))
            }
            ast::node::ExprKind::List(exprs) => {
                let exprs = self.lower_exprs(exprs);
                hir::ExprKind::List(exprs)
            }
            ast::node::ExprKind::Dict(entries) => {
                let entries = entries
                    .iter()
                    .map(|(key, value)| (self.lower_expr(key), self.lower_expr(value)))
                    .collect();
                hir::ExprKind::Dict(entries)
            }
            ast::node::ExprKind::Let(pat, expr) => {
                let expression = self.lower_expr(expr);
                let pattern = self.lower_pat(pat);
                hir::ExprKind::Let(Box::new(pattern), Box::new(expression))
            }
            ast::node::ExprKind::FieldExpression(box ast::node::FieldAccessExpression {
                base,
                field,
            }) => {
                let expr = self.lower_expr(base);
                hir::ExprKind::FieldAccess(Box::new(expr), field.clone())
            }
            ast::node::ExprKind::IndexExpression(box ast::node::IndexAccessExpression {
                base,
                index,
            }) => {
                let expr = self.lower_expr(base);
                let index = self.lower_expr(index);
                hir::ExprKind::IndexAccess(Box::new(expr), Box::new(index))
            }
            ast::node::ExprKind::IfElse(box ast::node::IfElseExpression {
                condition,
                then_branch,
                else_branches,
            }) if let ast::node::ExprKind::Let(pat, expr) = &condition.kind => {
                let expr = self.lower_expr(expr);
                let mut arms = Vec::with_capacity(else_branches.len() + 1);
                let block = self.lower_block(
                    &then_branch.statements,
                    then_branch.expression.as_ref(),
                    node.span,
                );

                arms.push(hir::MatchArm {
                    pat: self.lower_pat(pat),
                    guard: None,
                    expr: self.expr(node.span, hir::ExprKind::Block(Box::new(block))),
                    leading_comments: condition.leading_comments.clone(),
                    trailing_comments: condition.trailing_comments.clone(),
                });

                for else_branch in else_branches {
                    let condition = else_branch
                        .condition
                        .as_ref()
                        .map(|expr| self.lower_expr(expr));
                    let block = self.lower_block(
                        &else_branch.consequence.statements,
                        else_branch.consequence.expression.as_ref(),
                        node.span,
                    );
                    let consequence = self.expr(node.span, hir::ExprKind::Block(Box::new(block)));

                    arms.push(hir::MatchArm {
                        pat: hir::Pat {
                            kind: hir::PatKind::Wildcard,
                            span: Default::default(),
                        },
                        guard: condition,
                        expr: consequence,
                        leading_comments: vec![],
                        trailing_comments: vec![],
                    });
                }

                hir::ExprKind::Match(Box::new(expr), arms)
            }
            ast::node::ExprKind::IfElse(box ast::node::IfElseExpression {
                condition,
                then_branch,
                else_branches,
            }) => {
                let condition = self.lower_expr(condition);

                let consequence = self.lower_block(
                    &then_branch.statements,
                    then_branch.expression.as_ref(),
                    then_branch.span,
                );

                let else_branches = else_branches
                    .iter()
                    .map(|clause| hir::ElseClause {
                        condition: clause.condition.as_ref().map(|expr| self.lower_expr(expr)),
                        consequence: self.lower_block(
                            &clause.consequence.statements,
                            clause.consequence.expression.as_ref(),
                            clause.consequence.span,
                        ),
                    })
                    .collect();

                hir::ExprKind::IfElse(Box::new(condition), Box::new(consequence), else_branches)
            }
            ast::node::ExprKind::Literal(box literal) => {
                hir::ExprKind::Literal(Box::new(literal.clone()))
            }
            ast::node::ExprKind::Match(box ast::node::MatchExpression { expression, arms }) => {
                let mut idents = HashMap::new();
                let expr = self.lower_expr(expression);
                let arms = arms
                    .iter()
                    .map(|arm| {
                        self.with_new_scope(|this| hir::MatchArm {
                            pat: this.lower_pat_with_idents(&arm.pattern, &mut idents),
                            guard: arm.guard.as_ref().map(|expr| this.lower_expr(expr)),
                            expr: this.lower_expr(&arm.expression),
                            leading_comments: vec![],
                            trailing_comments: vec![],
                        })
                    })
                    .collect();
                hir::ExprKind::Match(Box::new(expr), arms)
            }
            ast::node::ExprKind::Range(box ast::node::RangeExpression {
                start,
                end,
                inclusive,
            }) => {
                let start = self.lower_expr(start);
                let end = self.lower_expr(end);
                hir::ExprKind::Range(Box::new(hir::RangeExpression {
                    start,
                    end,
                    inclusive: *inclusive,
                }))
            }
            ast::node::ExprKind::Wildcard => hir::ExprKind::Wildcard,
            ast::node::ExprKind::None => {
                unreachable!("ExprKind::None should not be encountered, validate AST first")
            }
        };

        hir::Expr {
            hir_id: self.lower_node_id(node.id),
            kind,
            span: node.span,
        }
    }

    #[inline(always)]
    fn expr(&mut self, span: ast::span::Span, kind: hir::ExprKind) -> hir::Expr {
        hir::Expr {
            hir_id: self.unique_id(),
            kind,
            span,
        }
    }

    fn lower_callee(&mut self, callee: &ast::node::Expr, arg_len: usize) -> hir::Expr {
        match &callee.kind {
            ast::node::ExprKind::Path(path) => {
                let mut path_with_argnum = path.clone();
                let new_name = format!(
                    "{}$${}",
                    path_with_argnum.segments.last().unwrap().as_str(),
                    arg_len
                );
                path_with_argnum
                    .segments
                    .last_mut()
                    .unwrap()
                    .set_name(&new_name);

                if self.has_binding(&path_with_argnum.join("::")) {
                    self.lower_expr(&ast::node::Expr {
                        id: callee.id,
                        kind: ast::node::ExprKind::Path(path_with_argnum),
                        span: callee.span,
                        leading_comments: callee.leading_comments.clone(),
                        trailing_comments: callee.trailing_comments.clone(),
                    })
                } else {
                    self.lower_expr(callee)
                }
            }
            _ => self.lower_expr(callee),
        }
    }

    fn lower_call_expr(&mut self, node: &ast::node::CallExpression) -> hir::CallExpression {
        let arguments = self.lower_exprs(&node.arguments);
        let callee = self.lower_callee(&node.callee, node.arguments.len());

        hir::CallExpression { callee, arguments }
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
                    let mut args_lengths = decls
                        .iter()
                        .map(|decl| decl.parameters.len())
                        .collect::<Vec<_>>();
                    args_lengths.sort();
                    args_lengths.dedup();

                    // Currently the easiest way is to split up
                    // ast::node::SmtKind::FunctionDeclarations by argument length and create a
                    // new unique name for each declaration.
                    let mut grouped_decls = vec![];
                    let mut fn_variant_ids = vec![];

                    for arg_len in &args_lengths {
                        let function_name = get_function_name(&first_declaration.name);
                        let function_name = function_name + "$$" + &arg_len.to_string();

                        self.scope().insert(&function_name, &function_name);
                    }

                    for (i, arg_len) in args_lengths.into_iter().enumerate() {
                        let decls = decls
                            .iter()
                            .filter(|decl| decl.parameters.len() == arg_len)
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

                        fn_variant_ids.push((arg_len, decl.hir_id));

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

        let mut span = decls[0].span;
        span.end = decls.last().unwrap().span.end;

        let first_declaration = &decls[0];
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
        let hir_id = self.lower_node_id(first_declaration.id);
        let mut hir_fn_decl = hir::FunctionDeclaration::new_empty_fn(
            hir_id,
            self.lower_expr(&first_declaration.name),
            param_names
                .iter()
                .map(|ident| hir::FunctionParameter {
                    name: ident.clone(),
                    type_annotation: hir::Ty {
                        kind: hir::TyKind::Unknown,
                        span: Default::default(),
                    },
                    span: ident.span,
                })
                .collect(),
        );

        let mut match_arms = Vec::with_capacity(decls.len());

        for (i, decl) in decls.iter().enumerate() {
            self.with_new_scope(|this| {
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

                let expr = if decl.body.statements.is_empty() && decl.body.expression.is_some() {
                    this.lower_expr(decl.body.expression.as_ref().unwrap())
                } else {
                    let body = this.lower_block(
                        &decl.body.statements,
                        decl.body.expression.as_ref(),
                        decl.body.span,
                    );
                    this.expr(body.span, hir::ExprKind::Block(Box::new(body)))
                };

                let mut arm_leading_comments = vec![];
                if i == 0 {
                    arm_leading_comments.extend(leading_comments.iter().cloned());
                }
                arm_leading_comments.extend(decl.leading_comments.clone());

                match_arms.push(hir::MatchArm {
                    pat,
                    guard,
                    expr,
                    leading_comments: arm_leading_comments,
                    trailing_comments: decl.trailing_comments.clone(),
                });
            });
        }

        let match_value = if param_names.len() > 1 {
            let argument_list = hir::ExprKind::List(
                param_names
                    .iter()
                    .map(|ident| {
                        self.expr(
                            span,
                            hir::ExprKind::Path(Box::new(hir::Path::new(
                                vec![hir::PathSegment {
                                    ident: ident.clone(),
                                }],
                                span,
                            ))),
                        )
                    })
                    .collect(),
            );

            self.expr(ast::span::Span::default(), argument_list)
        } else {
            self.expr(
                ast::span::Span::default(),
                hir::ExprKind::Path(Box::new(hir::Path::new(
                    vec![hir::PathSegment {
                        ident: param_names.first().unwrap().clone(),
                    }],
                    span,
                ))),
            )
        };

        hir_fn_decl.body.expr = Some(hir::Expr {
            hir_id: self.unique_id(),
            kind: hir::ExprKind::Match(Box::new(match_value), match_arms),
            span: ast::span::Span::default(),
        });

        hir_fn_decl
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

    fn lower_fn_param(&mut self, node: &ast::node::FunctionParameter) -> hir::FunctionParameter {
        hir::FunctionParameter {
            name: self.lower_fn_param_pat(node),
            type_annotation: self.lower_ty(node.type_annotation.as_ref()),
            span: node.span,
        }
    }

    fn lower_fn_decl(&mut self, decl: &FunctionDeclaration) -> hir::FunctionDeclaration {
        self.with_new_scope(|this| {
            let name = this.lower_expr(&decl.name);
            let parameters = decl
                .parameters
                .iter()
                .map(|param| this.lower_fn_param(param))
                .collect();
            let body = this.lower_block(
                &decl.body.statements,
                decl.body.expression.as_ref(),
                decl.body.span,
            );
            let return_type = this.lower_ty(decl.return_type_annotation.as_ref());

            hir::FunctionDeclaration {
                hir_id: this.lower_node_id(decl.id),
                name,
                parameters,
                variadic: false,
                return_type,
                body,
                span: decl.span,
            }
        })
    }

    fn lower_path(&mut self, path: &ast::node::Path) -> hir::Path {
        if path.segments.len() == 1 {
            let segment = path.segments.first().unwrap();
            let segment = hir::PathSegment::from_str(self.lookup(segment.as_str()), segment.span);

            return hir::Path::new(vec![segment], path.span);
        }

        let segments = path
            .segments
            .iter()
            .map(|seg| self.lower_path_segment(seg))
            .collect();

        hir::Path::new(segments, path.span)
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
                self.create_binding(ident.as_str());
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
                    kind: hir::PatKind::Identifier(
                        self.lower_node_id(node.id),
                        Box::new(ident.clone()),
                    ),
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

    fn lower_binary_expr(&mut self, node: &BinaryOpExpression) -> hir::ExprKind {
        let binary_op_kind = match node.op {
            BinaryOpKind::Assign => hir::BinaryOpKind::Assign,
            BinaryOpKind::Add => hir::BinaryOpKind::Add,
            BinaryOpKind::Subtract => hir::BinaryOpKind::Sub,
            BinaryOpKind::Multiply => hir::BinaryOpKind::Mul,
            BinaryOpKind::Divide => hir::BinaryOpKind::Div,
            BinaryOpKind::Modulo => hir::BinaryOpKind::Mod,
            BinaryOpKind::Exponentiation => hir::BinaryOpKind::Exp,
            BinaryOpKind::And => hir::BinaryOpKind::And,
            BinaryOpKind::Or => hir::BinaryOpKind::Or,
            BinaryOpKind::Equal => hir::BinaryOpKind::Eq,
            BinaryOpKind::NotEqual => hir::BinaryOpKind::NotEq,
            BinaryOpKind::LessThan => hir::BinaryOpKind::Less,
            BinaryOpKind::LessThanOrEqual => hir::BinaryOpKind::LessEq,
            BinaryOpKind::GreaterThan => hir::BinaryOpKind::Greater,
            BinaryOpKind::GreaterThanOrEqual => hir::BinaryOpKind::GreaterEq,
            BinaryOpKind::BitwiseAnd => hir::BinaryOpKind::BitwiseAnd,
            BinaryOpKind::BitwiseOr => hir::BinaryOpKind::BitwiseOr,
            BinaryOpKind::BitwiseXor => hir::BinaryOpKind::BitwiseXor,
            BinaryOpKind::Pipeline => {
                return match &node.rhs.kind {
                    ast::node::ExprKind::Path(_) => {
                        let lhs = self.lower_expr(&node.lhs);
                        let rhs = self.lower_callee(&node.rhs, 1);

                        hir::ExprKind::Call(Box::new(hir::CallExpression {
                            callee: rhs,
                            arguments: vec![lhs],
                        }))
                    }
                    ast::node::ExprKind::Call(call_expr) => {
                        let arguments = if call_expr.has_wildcard() {
                            call_expr
                                .arguments
                                .iter()
                                .map(|arg| {
                                    if matches!(arg.kind, ast::node::ExprKind::Wildcard) {
                                        self.lower_expr(&node.lhs)
                                    } else {
                                        self.lower_expr(arg)
                                    }
                                })
                                .collect()
                        } else {
                            let mut arguments = Vec::with_capacity(call_expr.arguments.len() + 1);
                            arguments.push(self.lower_expr(&node.lhs));
                            arguments.extend(self.lower_exprs(&call_expr.arguments));
                            arguments
                        };
                        let callee = self.lower_callee(&call_expr.callee, arguments.len());

                        hir::ExprKind::Call(Box::new(hir::CallExpression { callee, arguments }))
                    }
                    _ => unreachable!("Validate AST before lowering"),
                }
            }
        };

        let lhs = self.lower_expr(&node.lhs);
        let rhs = self.lower_expr(&node.rhs);

        hir::ExprKind::Binary(binary_op_kind, Box::new(lhs), Box::new(rhs))
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

fn get_function_name(expr: &ast::node::Expr) -> String {
    match &expr.kind {
        ast::node::ExprKind::Path(path) => path.join("::"),
        ast::node::ExprKind::FieldExpression(field_expr) => {
            get_function_name(&field_expr.base) + "." + field_expr.field.as_str()
        }
        _ => unreachable!(),
    }
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
