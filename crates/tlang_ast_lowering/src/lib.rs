#![feature(box_patterns)]
#![feature(if_let_guard)]
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use log::debug;
use tlang_ast as ast;
use tlang_ast::keyword::kw;
use tlang_ast::node::{EnumPattern, FunctionDeclaration, Ident};
use tlang_hir::hir::{self, HirId};

use self::scope::Scope;

mod expr;
mod r#loop;
mod scope;
mod stmt;

// TODO: Add scopes and variable resolutions. There should be two kinds of bindings, one for a
// whole block (declarations) and one for variable definitions (from definition forward).
// See: https://github.com/rust-lang/rust/blob/de7cef75be8fab7a7e1b4d5bb01b51b4bac925c3/compiler/rustc_resolve/src/lib.rs#L408
pub struct LoweringContext {
    unique_id: HirId,
    node_id_to_hir_id: HashMap<ast::NodeId, HirId>,
    symbol_tables: HashMap<ast::NodeId, Rc<RefCell<ast::symbols::SymbolTable>>>,
    scopes: Vec<Scope>,
}

impl LoweringContext {
    pub fn new(
        symbol_tables: HashMap<ast::NodeId, Rc<RefCell<ast::symbols::SymbolTable>>>,
    ) -> Self {
        Self {
            unique_id: HirId::new(1),
            node_id_to_hir_id: HashMap::default(),
            scopes: vec![Scope::default()],
            symbol_tables,
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
            .map_or(name.to_string(), |binding| binding.to_string())
    }

    pub(crate) fn lookup<'a>(&'a mut self, name: &'a str) -> Option<&str> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.lookup(name))
    }

    pub(crate) fn with_new_scope<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
        R: hir::HirScope,
    {
        debug!("Entering new scope");
        self.scopes.push(Scope::new());
        let result = f(self);
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
        self.scope().def_local(name.as_str());

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

    fn def_fn_local(&mut self, name_expr: &ast::node::Expr, arity: Option<usize>) {
        let mut fn_name = match &name_expr.kind {
            ast::node::ExprKind::Path(path) => path.to_string(),
            ast::node::ExprKind::FieldExpression(fe) => {
                let ast::node::ExprKind::Path(path) = &fe.base.kind else {
                    unreachable!("FieldExpression base should be a path");
                };

                path.join_with(fe.field.as_str())
            }
            _ => unreachable!(),
        };

        if let Some(arity) = arity {
            fn_name += "$$";
            fn_name += &arity.to_string();
        }

        self.scope().def_local(&fn_name);
    }

    fn lower_fn_decl(&mut self, decl: &FunctionDeclaration) -> hir::FunctionDeclaration {
        self.with_new_scope(|this| {
            let hir_id = this.lower_node_id(decl.id);

            this.def_fn_local(&decl.name, None);

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
        if path.segments.len() == 1 {
            let segment = path.segments.first().unwrap();
            let segment =
                hir::PathSegment::from_str(&self.lookup_name(segment.as_str()), segment.span);

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
                let hir_id = self.lower_node_id(node.id);

                self.scope().def_local(ident.as_str());
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
        debug!("Lowering type {node:?}");

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

pub fn lower_to_hir(tlang_ast: &ast::node::Module) -> hir::Module {
    let mut ctx = LoweringContext::new();
    ctx.lower_module(tlang_ast)
}
