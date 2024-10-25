#![feature(box_patterns)]
use tlang_ast as ast;
use tlang_ast::node::{
    BinaryOpExpression, CallExpression, EnumPattern, FunctionDeclaration, Ident, IdentifierPattern,
    LetDeclaration,
};
use tlang_ast::token::kw;
use tlang_hir::hir::{self, HirId};

struct LoweringContext {
    unique_id: HirId,
    symbol_id_to_hir_id: std::collections::HashMap<ast::symbols::SymbolId, HirId>,
}

impl LoweringContext {
    pub fn new() -> Self {
        Self {
            unique_id: HirId::new(0),
            symbol_id_to_hir_id: std::collections::HashMap::default(),
        }
    }

    fn unique_id(&mut self) -> HirId {
        let id = self.unique_id;
        self.unique_id = self.unique_id.next();
        id
    }

    fn lower_symbol_id(&mut self, id: &ast::symbols::SymbolId) -> HirId {
        if let Some(hir_id) = self.symbol_id_to_hir_id.get(id) {
            *hir_id
        } else {
            let hir_id = self.unique_id();
            self.symbol_id_to_hir_id.insert(*id, hir_id);
            hir_id
        }
    }

    fn lower_module(&mut self, module: &ast::node::Module) -> hir::Module {
        hir::Module {
            block: self.lower_block(&module.statements, &None, module.span),
            span: module.span,
        }
    }

    fn lower_block(
        &mut self,
        stmts: &[ast::node::Stmt],
        expr: &Option<ast::node::Expr>,
        span: ast::span::Span,
    ) -> hir::Block {
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
        match &node.kind {
            ast::node::ExprKind::BinaryOp(box BinaryOpExpression { op, lhs, rhs }) => {
                let lhs = self.lower_expr(lhs);
                let rhs = self.lower_expr(rhs);
                self.expr(
                    node.span,
                    hir::ExprKind::Binary(*op, Box::new(lhs), Box::new(rhs)),
                )
            }
            ast::node::ExprKind::Block(box ast::node::Block {
                statements,
                expression,
                span,
                symbol_table: _,
            }) => {
                let kind =
                    hir::ExprKind::Block(Box::new(self.lower_block(statements, expression, *span)));
                self.expr(*span, kind)
            }
            ast::node::ExprKind::Call(box CallExpression { callee, arguments }) => {
                let callee = self.lower_expr(callee);
                let arguments = self.lower_exprs(arguments);
                self.expr(node.span, hir::ExprKind::Call(Box::new(callee), arguments))
            }
            ast::node::ExprKind::RecursiveCall(box CallExpression { callee, arguments }) => {
                let callee = self.lower_expr(callee);
                let arguments = self.lower_exprs(arguments);
                self.expr(
                    node.span,
                    hir::ExprKind::TailCall(Box::new(callee), arguments),
                )
            }
            ast::node::ExprKind::UnaryOp(op, expr) => {
                let expr = self.lower_expr(expr);
                self.expr(node.span, hir::ExprKind::Unary(*op, Box::new(expr)))
            }
            ast::node::ExprKind::Path(path) => {
                let path = self.lower_path(path);
                self.expr(node.span, hir::ExprKind::Path(Box::new(path)))
            }
            ast::node::ExprKind::FunctionExpression(decl) => {
                let decl = self.lower_fn_decl(decl);
                self.expr(node.span, hir::ExprKind::FunctionExpression(Box::new(decl)))
            }
            ast::node::ExprKind::List(exprs) => {
                let exprs = self.lower_exprs(exprs);
                self.expr(node.span, hir::ExprKind::List(exprs))
            }
            ast::node::ExprKind::Dict(entries) => {
                let entries = entries
                    .iter()
                    .map(|(key, value)| (self.lower_expr(key), self.lower_expr(value)))
                    .collect();
                self.expr(node.span, hir::ExprKind::Dict(entries))
            }
            ast::node::ExprKind::Let(pat, expr) => {
                let pattern = self.lower_pat(pat);
                let expression = self.lower_expr(expr);
                self.expr(
                    node.span,
                    hir::ExprKind::Let(Box::new(pattern), Box::new(expression)),
                )
            }
            ast::node::ExprKind::FieldExpression(box ast::node::FieldAccessExpression {
                base,
                field,
            }) => {
                let expr = self.lower_expr(base);
                self.expr(
                    node.span,
                    hir::ExprKind::FieldAccess(Box::new(expr), field.clone()),
                )
            }
            ast::node::ExprKind::IndexExpression(box ast::node::IndexAccessExpression {
                base,
                index,
            }) => {
                let expr = self.lower_expr(base);
                let index = self.lower_expr(index);
                self.expr(
                    node.span,
                    hir::ExprKind::IndexAccess(Box::new(expr), Box::new(index)),
                )
            }
            ast::node::ExprKind::IfElse(box ast::node::IfElseExpression {
                condition,
                then_branch,
                else_branches,
            }) => {
                let condition = self.lower_expr(condition);

                let consequence = if let ast::node::ExprKind::Block(block) = &then_branch.kind {
                    self.lower_block(&block.statements, &block.expression, block.span)
                } else {
                    // TODO: Defensive, technically we do not have then or else branches which are
                    //       not blocks. We might want to just change the AST instead.
                    self.lower_block(&[], &Some(then_branch.clone()), then_branch.span)
                };

                let else_branches = else_branches
                    .iter()
                    .map(|clause| hir::ElseClause {
                        condition: clause.condition.as_ref().map(|expr| self.lower_expr(expr)),
                        consequence: if let ast::node::ExprKind::Block(block) =
                            &clause.consequence.kind
                        {
                            self.lower_block(&block.statements, &block.expression, block.span)
                        } else {
                            // TODO: Defensive, technically we do not have then or else branches which are
                            //       not blocks. We might want to just change the AST instead.
                            self.lower_block(&[], &Some(clause.consequence.clone()), node.span)
                        },
                    })
                    .collect();

                self.expr(
                    node.span,
                    hir::ExprKind::IfElse(
                        Box::new(condition),
                        Box::new(consequence),
                        else_branches,
                    ),
                )
            }
            ast::node::ExprKind::Literal(box literal) => {
                self.expr(node.span, hir::ExprKind::Literal(Box::new(literal.clone())))
            }
            ast::node::ExprKind::Match(box ast::node::MatchExpression { expression, arms }) => {
                let expr = self.lower_expr(expression);
                let arms = arms
                    .iter()
                    .map(|arm| hir::MatchArm {
                        pat: self.lower_pat(&arm.pattern),
                        guard: arm.guard.as_ref().map(|expr| self.lower_expr(expr)),
                        expr: self.lower_expr(&arm.expression),
                    })
                    .collect();
                self.expr(node.span, hir::ExprKind::Match(Box::new(expr), arms))
            }
            ast::node::ExprKind::Range(box ast::node::RangeExpression {
                start,
                end,
                inclusive,
            }) => {
                let start = self.lower_expr(start);
                let end = self.lower_expr(end);
                self.expr(
                    node.span,
                    hir::ExprKind::Range(Box::new(hir::RangeExpression {
                        start,
                        end,
                        inclusive: *inclusive,
                    })),
                )
            }
            ast::node::ExprKind::Wildcard => self.expr(node.span, hir::ExprKind::Wildcard),
            ast::node::ExprKind::None => {
                unreachable!("ExprKind::None should not be encountered, validate AST first")
            }
        }
    }

    fn expr(&mut self, span: ast::span::Span, kind: hir::ExprKind) -> hir::Expr {
        hir::Expr { kind, span }
    }

    fn lower_stmt(&mut self, node: &ast::node::Stmt) -> Vec<hir::Stmt> {
        match &node.kind {
            ast::node::StmtKind::Expr(expr) => vec![hir::Stmt {
                kind: hir::StmtKind::Expr(Box::new(self.lower_expr(expr))),
                span: node.span,
            }],
            ast::node::StmtKind::Let(box LetDeclaration {
                pattern,
                expression,
                type_annotation,
            }) => vec![hir::Stmt {
                kind: hir::StmtKind::Let(
                    Box::new(self.lower_pat(pattern)),
                    Box::new(self.lower_expr(expression)),
                    Box::new(self.lower_ty(type_annotation)),
                ),
                span: node.span,
            }],
            ast::node::StmtKind::FunctionDeclaration(box decl) => {
                let decl = self.lower_fn_decl(decl);

                vec![hir::Stmt {
                    kind: hir::StmtKind::FunctionDeclaration(Box::new(decl)),
                    span: node.span,
                }]
            }
            // For multiple function declarations with the same name (which is already grouped
            // by the parser in the AST, for now), we emit multiple function declarations whith
            // their own name.
            ast::node::StmtKind::FunctionDeclarations(decls) => {
                let first_declaration = decls.first().unwrap();
                let has_variadic_arguments = decls
                    .iter()
                    .all(|decl| decl.parameters.len() == first_declaration.parameters.len());

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

                    // TODO!
                    // Currently the best way might be to create a new
                    // ast::node::SmtKind::FunctionDeclarations split up by argument length
                    // create a new unique name for the declarations and recurse into lower_stmt
                    // again.
                    // At last, we emit a wrapping function using the original name with a new
                    // hir id which wraps and dynamically dispatches the function based on
                    // argument length.
                    let mut grouped_decls = vec![];

                    for arg_len in args_lengths {
                        let decls = decls
                            .iter()
                            .filter(|decl| decl.parameters.len() == arg_len)
                            .cloned()
                            .collect::<Vec<_>>();

                        let stmts = self.lower_stmt(&ast::node::Stmt {
                            kind: ast::node::StmtKind::FunctionDeclarations(decls),
                            span: node.span,
                            ..Default::default()
                        });

                        grouped_decls.extend(stmts);
                    }

                    grouped_decls
                } else {
                    // Create hir::FunctionDeclaration with an empty block, and fill out the
                    // parameters, for each parameter/argument we reuse the existing plain
                    // identifier if it exists, otherwise we create a new one which will be reused
                    // in a match expression in the resulting block.
                    let hir_id = self.lower_symbol_id(&first_declaration.id);
                    let mut hir_fn_decl = hir::FunctionDeclaration::new_empty_fn(
                        hir_id,
                        self.lower_expr(&first_declaration.name),
                    );

                    // TODO: As a starting point, we generate all argument names manually, later we
                    //       might want to try to keep given names if possible.
                    let argument_names = (0..first_declaration.parameters.len())
                        .map(|i| Ident::new(&format!("arg{}", i), node.span))
                        .collect::<Vec<_>>();

                    let mut match_arms = Vec::with_capacity(decls.len());

                    for decl in decls {
                        // All declarations with the same amount of arguments refer to the same
                        // function now, we map this in our symbol_id_to_hir_id table.
                        self.symbol_id_to_hir_id.insert(decl.id, hir_id);

                        // Mapping argument pattern and signature guard into a match arm
                        let pat = hir::Pat {
                            kind: hir::PatKind::List(
                                decl.parameters
                                    .iter()
                                    // We lose type information of each declaration here, as we
                                    // lower a whole FunctionParameter into a pattern. They
                                    // should probably match for each declaration and we might want
                                    // to verify this now or earlier in the pipeline.
                                    // Ignored for now as types are basically a NOOP everywhere.
                                    .map(|param| self.lower_pat(&param.pattern))
                                    .collect(),
                            ),
                            span: decl.span,
                        };

                        let guard = decl.guard.as_ref().map(|expr| self.lower_expr(expr));

                        let body = self.lower_block(
                            &decl.body.statements,
                            &decl.body.expression,
                            decl.body.span,
                        );

                        let expr = self.expr(decl.body.span, hir::ExprKind::Block(Box::new(body)));

                        match_arms.push(hir::MatchArm { pat, guard, expr });
                    }

                    let match_value = self.expr(
                        ast::span::Span::default(),
                        hir::ExprKind::List(
                            argument_names
                                .iter()
                                .map(|ident| hir::Expr {
                                    kind: hir::ExprKind::Path(Box::new(hir::Path {
                                        segments: vec![hir::PathSegment {
                                            ident: ident.clone(),
                                        }],
                                        span: node.span,
                                    })),
                                    span: node.span,
                                })
                                .collect(),
                        ),
                    );

                    hir_fn_decl.body.expr = Some(hir::Expr {
                        kind: hir::ExprKind::Match(Box::new(match_value), match_arms),
                        span: ast::span::Span::default(),
                    });

                    vec![hir::Stmt {
                        kind: hir::StmtKind::FunctionDeclaration(Box::new(hir_fn_decl)),
                        span: node.span,
                    }]
                }
            }
            ast::node::StmtKind::Return(box expr) => {
                let expr = expr.as_ref().map(|expr| self.lower_expr(expr));

                vec![hir::Stmt {
                    kind: hir::StmtKind::Return(Box::new(expr)),
                    span: node.span,
                }]
            }
            ast::node::StmtKind::StructDeclaration(decl) => {
                let decl = hir::StructDeclaration {
                    hir_id: self.lower_symbol_id(&decl.id),
                    name: decl.name.clone(),
                    fields: decl
                        .fields
                        .iter()
                        .map(|field| hir::StructField {
                            // TODO: We might want/need an id on the AST already.
                            hir_id: self.unique_id(),
                            name: field.0.clone(),
                            ty: self.lower_ty(&Some(field.1.clone())),
                        })
                        .collect(),
                };

                vec![hir::Stmt {
                    kind: hir::StmtKind::StructDeclaration(Box::new(decl)),
                    span: node.span,
                }]
            }
            ast::node::StmtKind::EnumDeclaration(decl) => {
                let decl = hir::EnumDeclaration {
                    hir_id: self.lower_symbol_id(&decl.id),
                    name: decl.name.clone(),
                    variants: decl
                        .variants
                        .iter()
                        .map(|variant| hir::EnumVariant {
                            hir_id: self.unique_id(),
                            name: variant.name.clone(),
                            parameters: variant.parameters.clone(),
                            span: variant.span,
                        })
                        .collect::<Vec<_>>(),
                };

                vec![hir::Stmt {
                    kind: hir::StmtKind::EnumDeclaration(Box::new(decl)),
                    span: node.span,
                }]
            }
            ast::node::StmtKind::None => {
                unreachable!("StmtKind::None should not be encountered, validate AST first")
            }
        }
    }

    fn lower_fn_param(&mut self, node: &ast::node::FunctionParameter) -> hir::FunctionParameter {
        hir::FunctionParameter {
            pattern: self.lower_pat(&node.pattern),
            type_annotation: self.lower_ty(&node.type_annotation),
            span: node.span,
        }
    }

    fn lower_fn_decl(&mut self, decl: &FunctionDeclaration) -> hir::FunctionDeclaration {
        let name = self.lower_expr(&decl.name);
        let parameters = decl
            .parameters
            .iter()
            .map(|param| self.lower_fn_param(param))
            .collect();
        let body = self.lower_block(&decl.body.statements, &decl.body.expression, decl.body.span);
        let return_type = self.lower_ty(&decl.return_type_annotation);

        hir::FunctionDeclaration {
            hir_id: self.lower_symbol_id(&decl.id),
            name,
            parameters,
            return_type,
            body,
            span: decl.span,
        }
    }

    fn lower_path(&mut self, path: &ast::node::Path) -> hir::Path {
        let segments = path
            .segments
            .iter()
            .map(|seg| self.lower_path_segment(seg))
            .collect();

        hir::Path {
            segments,
            span: path.span,
        }
    }

    fn lower_path_segment(&mut self, seg: &Ident) -> hir::PathSegment {
        hir::PathSegment { ident: seg.clone() }
    }

    fn lower_pat(&mut self, node: &ast::node::Pattern) -> hir::Pat {
        match &node.kind {
            ast::node::PatternKind::Wildcard => hir::Pat {
                kind: hir::PatKind::Wildcard,
                span: node.span,
            },
            ast::node::PatternKind::Literal(box literal) => hir::Pat {
                kind: hir::PatKind::Literal(Box::new(literal.clone())),
                span: node.span,
            },
            ast::node::PatternKind::Identifier(box IdentifierPattern { id, name }) => hir::Pat {
                kind: hir::PatKind::Identifier(self.lower_symbol_id(id), Box::new(name.clone())),
                span: node.span,
            },
            ast::node::PatternKind::List(patterns) => hir::Pat {
                kind: hir::PatKind::List(patterns.iter().map(|pat| self.lower_pat(pat)).collect()),
                span: node.span,
            },
            ast::node::PatternKind::Rest(pattern) => hir::Pat {
                kind: hir::PatKind::Rest(Box::new(self.lower_pat(pattern))),
                span: node.span,
            },
            ast::node::PatternKind::Enum(box EnumPattern {
                // TODO: We lower the identifier to a path, in the AST this is an Expr, but it is
                //       always a path, we should update the AST to reflect this.
                identifier,
                elements,
                named_fields: _, // In HIR, we no longer care whether it's a named field or not
            }) => {
                let path = if let ast::node::ExprKind::Path(path) = &identifier.kind {
                    path
                } else {
                    unreachable!("EnumPattern identifier should be a path, validate AST first")
                };

                let path = self.lower_path(path);
                let elements = elements.iter().map(|pat| self.lower_pat(pat)).collect();

                hir::Pat {
                    kind: hir::PatKind::Enum(Box::new(path), elements),
                    span: node.span,
                }
            }
            ast::node::PatternKind::_Self(id) => hir::Pat {
                kind: hir::PatKind::Identifier(
                    self.lower_symbol_id(id),
                    Box::new(Ident::new(kw::_Self, node.span)),
                ),
                span: node.span,
            },
            ast::node::PatternKind::None => {
                unreachable!("PatternKind::None should not be encountered, validate AST first")
            }
        }
    }

    fn lower_ty(&mut self, _node: &Option<ast::node::Ty>) -> hir::Ty {
        hir::Ty {
            kind: hir::TyKind::Unknown,
            span: ast::span::Span::default(),
        }
    }
}

pub fn lower_to_hir(tlang_ast: &ast::node::Module) -> hir::Module {
    let mut ctx = LoweringContext::new();
    ctx.lower_module(tlang_ast)
}
