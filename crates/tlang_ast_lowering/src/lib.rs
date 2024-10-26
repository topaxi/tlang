#![feature(box_patterns)]
use tlang_ast as ast;
use tlang_ast::node::{
    BinaryOpExpression, CallExpression, EnumPattern, FunctionDeclaration, Ident, LetDeclaration,
};
use tlang_ast::token::kw;
use tlang_hir::hir::{self, HirId};

struct LoweringContext {
    unique_id: HirId,
    node_id_to_hir_id: std::collections::HashMap<ast::node_id::NodeId, HirId>,
}

impl LoweringContext {
    pub fn new() -> Self {
        Self {
            unique_id: HirId::new(0),
            node_id_to_hir_id: std::collections::HashMap::default(),
        }
    }

    fn unique_id(&mut self) -> HirId {
        let id = self.unique_id;
        self.unique_id = self.unique_id.next();
        id
    }

    fn lower_node_id(&mut self, id: ast::node_id::NodeId) -> HirId {
        // TODO: We need this as we lower function declarations by creating new AST nodes and
        //       lowering those. Instead we should either build the AST different or transform on
        //       the HIR itself or both.
        if id == ast::node_id::NodeId::new(0) {
            return self.unique_id();
        }

        if let Some(hir_id) = self.node_id_to_hir_id.get(&id) {
            *hir_id
        } else {
            let hir_id = self.unique_id();
            self.node_id_to_hir_id.insert(id, hir_id);
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
        let kind = match &node.kind {
            ast::node::ExprKind::BinaryOp(box BinaryOpExpression { op, lhs, rhs }) => {
                let lhs = self.lower_expr(lhs);
                let rhs = self.lower_expr(rhs);
                hir::ExprKind::Binary(*op, Box::new(lhs), Box::new(rhs))
            }
            ast::node::ExprKind::Block(box ast::node::Block {
                id: _,
                statements,
                expression,
                span,
            }) => hir::ExprKind::Block(Box::new(self.lower_block(statements, expression, *span))),
            ast::node::ExprKind::Call(box CallExpression { callee, arguments }) => {
                let callee = self.lower_expr(callee);
                let arguments = self.lower_exprs(arguments);
                hir::ExprKind::Call(Box::new(callee), arguments)
            }
            ast::node::ExprKind::RecursiveCall(box CallExpression { callee, arguments }) => {
                let callee = self.lower_expr(callee);
                let arguments = self.lower_exprs(arguments);
                hir::ExprKind::TailCall(Box::new(callee), arguments)
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
                let pattern = self.lower_pat(pat);
                let expression = self.lower_expr(expr);
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

                hir::ExprKind::IfElse(Box::new(condition), Box::new(consequence), else_branches)
            }
            ast::node::ExprKind::Literal(box literal) => {
                hir::ExprKind::Literal(Box::new(literal.clone()))
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

    fn expr(&mut self, span: ast::span::Span, kind: hir::ExprKind) -> hir::Expr {
        hir::Expr {
            hir_id: self.unique_id(),
            kind,
            span,
        }
    }

    fn lower_stmt(&mut self, node: &ast::node::Stmt) -> Vec<hir::Stmt> {
        match &node.kind {
            ast::node::StmtKind::Expr(expr) => vec![hir::Stmt {
                hir_id: self.lower_node_id(node.id),
                kind: hir::StmtKind::Expr(Box::new(self.lower_expr(expr))),
                span: node.span,
            }],
            ast::node::StmtKind::Let(box LetDeclaration {
                pattern,
                expression,
                type_annotation,
            }) => vec![hir::Stmt {
                hir_id: self.lower_node_id(node.id),
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
                    hir_id: self.lower_node_id(node.id),
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
                    .any(|decl| decl.parameters.len() != first_declaration.parameters.len());

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
                    // TODO: At last, we should  emit a wrapping function using the original name
                    // with a new hir id which wraps and dynamically dispatches the function based
                    // on argument length.
                    let mut grouped_decls = vec![];

                    for arg_len in args_lengths {
                        let decls = decls
                            .iter()
                            .filter(|decl| decl.parameters.len() == arg_len)
                            .cloned()
                            .map(|mut decl| {
                                match &mut decl.name.kind {
                                    ast::node::ExprKind::Path(path) => {
                                        let ident = path.segments.last_mut().unwrap();
                                        ident.name = format!("{}$${}", ident.name, arg_len);
                                    }
                                    ast::node::ExprKind::FieldExpression(fe) => {
                                        let ident = &mut fe.field;
                                        ident.name = format!("{}$${}", ident.name, arg_len);
                                    }
                                    _ => unreachable!(),
                                };
                                decl
                            })
                            .collect::<Vec<_>>();

                        let decl = self.lower_fn_decl_matching(&decls);

                        grouped_decls.push(hir::Stmt {
                            // TODO: Hope this will not mess with us in the future, we generate
                            // more statements than initially in the AST, so we are not able to
                            // lower the original node id here.
                            hir_id: self.unique_id(),
                            kind: hir::StmtKind::FunctionDeclaration(Box::new(decl)),
                            span: node.span,
                        });
                    }

                    grouped_decls
                } else {
                    let hir_fn_decl = self.lower_fn_decl_matching(decls);
                    vec![hir::Stmt {
                        hir_id: self.lower_node_id(node.id),
                        kind: hir::StmtKind::FunctionDeclaration(Box::new(hir_fn_decl)),
                        span: node.span,
                    }]
                }
            }
            ast::node::StmtKind::Return(box expr) => {
                let expr = expr.as_ref().map(|expr| self.lower_expr(expr));

                vec![hir::Stmt {
                    hir_id: self.lower_node_id(node.id),
                    kind: hir::StmtKind::Return(Box::new(expr)),
                    span: node.span,
                }]
            }
            ast::node::StmtKind::StructDeclaration(decl) => {
                let decl = hir::StructDeclaration {
                    hir_id: self.lower_node_id(node.id),
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
                    hir_id: self.lower_node_id(node.id),
                    kind: hir::StmtKind::StructDeclaration(Box::new(decl)),
                    span: node.span,
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
                            parameters: variant.parameters.clone(),
                            span: variant.span,
                        })
                        .collect::<Vec<_>>(),
                };

                vec![hir::Stmt {
                    hir_id: self.lower_node_id(node.id),
                    kind: hir::StmtKind::EnumDeclaration(Box::new(decl)),
                    span: node.span,
                }]
            }
            ast::node::StmtKind::None => {
                unreachable!("StmtKind::None should not be encountered, validate AST first")
            }
        }
    }

    fn lower_fn_decl_matching(
        &mut self,
        decls: &[FunctionDeclaration],
    ) -> hir::FunctionDeclaration {
        if decls.len() == 1 {
            return self.lower_fn_decl(&decls[0]);
        }

        let mut span = decls[0].span;
        span.end = decls.last().unwrap().span.end;

        let first_declaration = decls.first().unwrap();

        // Create hir::FunctionDeclaration with an empty block, and fill out the
        // parameters, for each parameter/argument we reuse the existing plain
        // identifier if it exists, otherwise we create a new one which will be reused
        // in a match expression in the resulting block.
        let hir_id = self.lower_node_id(first_declaration.id);
        let mut hir_fn_decl = hir::FunctionDeclaration::new_empty_fn(
            hir_id,
            self.lower_expr(&first_declaration.name),
        );

        // TODO: As a starting point, we generate all argument names manually, later we
        //       might want to try to keep given names if possible.
        let argument_names = (0..first_declaration.parameters.len())
            .map(|i| Ident::new(&format!("arg{}", i), span))
            .collect::<Vec<_>>();

        let mut match_arms = Vec::with_capacity(decls.len());

        for decl in decls {
            // All declarations with the same amount of arguments refer to the same
            // function now, we map this in our symbol_id_to_hir_id table.
            self.node_id_to_hir_id.insert(decl.id, hir_id);

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

            let body =
                self.lower_block(&decl.body.statements, &decl.body.expression, decl.body.span);

            let expr = self.expr(decl.body.span, hir::ExprKind::Block(Box::new(body)));

            match_arms.push(hir::MatchArm { pat, guard, expr });
        }

        let argument_list = hir::ExprKind::List(
            argument_names
                .iter()
                .map(|ident| {
                    self.expr(
                        span,
                        hir::ExprKind::Path(Box::new(hir::Path {
                            segments: vec![hir::PathSegment {
                                ident: ident.clone(),
                            }],
                            span,
                        })),
                    )
                })
                .collect(),
        );
        let match_value = self.expr(ast::span::Span::default(), argument_list);

        hir_fn_decl.body.expr = Some(hir::Expr {
            hir_id: self.unique_id(),
            kind: hir::ExprKind::Match(Box::new(match_value), match_arms),
            span: ast::span::Span::default(),
        });

        hir_fn_decl
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
            hir_id: self.lower_node_id(decl.id),
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
            ast::node::PatternKind::Identifier(box ident) => hir::Pat {
                kind: hir::PatKind::Identifier(
                    self.lower_node_id(node.id),
                    Box::new(ident.clone()),
                ),
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
            ast::node::PatternKind::_Self => hir::Pat {
                kind: hir::PatKind::Identifier(
                    self.lower_node_id(node.id),
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
