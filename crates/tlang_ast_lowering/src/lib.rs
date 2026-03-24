#![feature(box_patterns)]
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use log::debug;
use tlang_ast as ast;
use tlang_ast::keyword::kw;
use tlang_ast::node::{EnumPattern, FunctionDeclaration, Ident};
use tlang_defs::{Def, DefIdAllocator, DefKind, DefScope};
use tlang_hir as hir;
use tlang_span::{HirId, HirIdAllocator, NodeId, Span};

/// Errors that can occur during AST-to-HIR lowering.
#[derive(Debug, Clone)]
pub enum LoweringError {
    /// A `NodeId` could not be mapped to a `HirId` during symbol table translation.
    UnmappedNodeId {
        node_id: NodeId,
        context: &'static str,
    },
    /// A function declaration has an invalid name expression (not a path or field expression).
    InvalidFunctionName { span: Span },
}

impl fmt::Display for LoweringError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoweringError::UnmappedNodeId { node_id, context } => {
                write!(f, "unable to map {node_id:?} to HirId: {context}")
            }
            LoweringError::InvalidFunctionName { span } => {
                write!(
                    f,
                    "invalid function name expression at {}:{}",
                    span.start_lc.line + 1,
                    span.start_lc.column + 1
                )
            }
        }
    }
}

mod expr;
mod r#loop;
mod stmt;

#[derive(Debug)]
pub struct LoweringContext {
    node_id_to_hir_id: HashMap<NodeId, HirId>,
    fn_node_id_to_hir_id: HashMap<NodeId, HirId>,
    hir_id_allocator: HirIdAllocator,
    symbol_id_allocator: DefIdAllocator,
    symbol_tables: HashMap<NodeId, Rc<RefCell<DefScope>>>,
    new_symbol_tables: HashMap<HirId, Rc<RefCell<DefScope>>>,
    current_symbol_table: Rc<RefCell<DefScope>>,
    errors: Vec<LoweringError>,
}

impl LoweringContext {
    pub fn new(
        symbol_id_allocator: DefIdAllocator,
        root_symbol_table: Rc<RefCell<DefScope>>,
        symbol_tables: HashMap<NodeId, Rc<RefCell<DefScope>>>,
    ) -> Self {
        Self {
            hir_id_allocator: HirIdAllocator::default(),
            node_id_to_hir_id: HashMap::default(),
            fn_node_id_to_hir_id: HashMap::default(),
            symbol_id_allocator,
            symbol_tables,
            new_symbol_tables: HashMap::default(),
            current_symbol_table: root_symbol_table,
            errors: Vec::new(),
        }
    }

    pub fn symbol_tables(&self) -> HashMap<HirId, Rc<RefCell<DefScope>>> {
        debug!("Translating symbol tables to HirIds");

        let mut symbol_tables = self.new_symbol_tables.clone();

        for (node_id, symbol_table) in &self.symbol_tables {
            if let Some(hir_id) = self.node_id_to_hir_id.get(node_id) {
                let symbol_table = symbol_table.clone();

                let fn_node_id_to_hir_id = &self.fn_node_id_to_hir_id;
                let node_id_to_hir_id = &self.node_id_to_hir_id;

                symbol_table
                    .borrow_mut()
                    .get_all_local_symbols_mut()
                    .iter_mut()
                    .for_each(|symbol| {
                        if let Some(node_id) = symbol.node_id {
                            if let Some(hir_id) = fn_node_id_to_hir_id
                                .get(&node_id)
                                .or_else(|| node_id_to_hir_id.get(&node_id))
                            {
                                symbol.hir_id = Some(*hir_id);

                                debug!(
                                    "Assigning {:?} to symbol {} on line {} from {:?}",
                                    hir_id, symbol.name, symbol.defined_at.start, node_id
                                );
                            } else {
                                debug!(
                                    "Unable to map {:?} to HirId for symbol {} \
                                     (expected for builtin/external symbols)",
                                    node_id, symbol.name
                                );
                            }
                        }
                    });

                symbol_tables.insert(*hir_id, symbol_table);
            } else {
                debug!(
                    "No HirId found for NodeId: {node_id:?} \
                     (expected for builtin/external symbol tables)"
                );
            }
        }

        debug!("NodeId to HirId mapping: {:#?}", self.node_id_to_hir_id);

        symbol_tables
    }

    #[inline(always)]
    pub(crate) fn scope(&self) -> Rc<RefCell<DefScope>> {
        self.current_symbol_table.clone()
    }

    /// Extract the function name from `decl`, pushing an
    /// [`InvalidFunctionName`](LoweringError::InvalidFunctionName) error and
    /// returning `"<invalid>"` when the name expression is not a recognised
    /// form.
    pub(crate) fn fn_name_or_error(&mut self, decl: &ast::node::FunctionDeclaration) -> String {
        decl.name().unwrap_or_else(|| {
            self.errors.push(LoweringError::InvalidFunctionName {
                span: decl.name.span,
            });
            "<invalid>".to_string()
        })
    }

    fn has_multi_arity_fn(&self, name: &str, arity: usize) -> bool {
        self.scope().borrow().has_multi_arity_fn(name, arity)
    }

    pub(crate) fn with_scope<F, R>(&mut self, node_id: NodeId, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
        R: hir::HirScope,
    {
        debug!("Entering scope for node_id: {node_id:?}");

        let previous_symbol_table = self.current_symbol_table.clone();
        if let Some(symbol_table) = self.symbol_tables.get(&node_id).cloned() {
            self.current_symbol_table = symbol_table;
            // Reparent the current symbol table to the previous one, in case we created a new
            // scope while lowering.
            self.current_symbol_table
                .borrow_mut()
                .set_parent(previous_symbol_table.clone());
        }

        let result = f(self);

        if self.symbol_tables.contains_key(&node_id) {
            debug!("Leaving scope for node_id: {node_id:?}");
        }
        self.current_symbol_table = previous_symbol_table;

        result
    }

    pub(crate) fn with_new_scope<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self, Rc<RefCell<DefScope>>) -> (HirId, R),
        R: hir::HirScope,
    {
        let previous_symbol_table = self.current_symbol_table.clone();
        self.current_symbol_table =
            Rc::new(RefCell::new(DefScope::new(previous_symbol_table.clone())));
        let (hir_id, result) = f(self, self.current_symbol_table.clone());
        self.new_symbol_tables
            .insert(hir_id, self.current_symbol_table.clone());
        self.current_symbol_table = previous_symbol_table;
        result
    }

    fn unique_id(&mut self) -> HirId {
        self.hir_id_allocator.next_id()
    }

    pub(crate) fn define_symbol(
        &mut self,
        hir_id: HirId,
        name: &str,
        kind: DefKind,
        scope_start: u32,
    ) {
        let symbol_info = Def::new(
            self.symbol_id_allocator.next_id(),
            name,
            kind,
            Default::default(),
            scope_start,
        )
        .with_hir_id(hir_id)
        .as_temp();

        self.scope().borrow_mut().insert(symbol_info);
    }

    pub(crate) fn define_symbol_at(
        &mut self,
        index: usize,
        hir_id: HirId,
        name: &str,
        kind: DefKind,
        scope_start: u32,
    ) {
        let symbol_info = Def::new(
            self.symbol_id_allocator.next_id(),
            name,
            kind,
            Default::default(),
            scope_start,
        )
        .with_hir_id(hir_id)
        .as_temp();

        self.scope().borrow_mut().insert_at(index, symbol_info);
    }

    pub(crate) fn define_symbol_after(
        &mut self,
        hir_id: HirId,
        name: &str,
        kind: DefKind,
        scope_start: u32,
        predicate: impl Fn(&Def) -> bool,
    ) {
        let symbol_info = Def::new(
            self.symbol_id_allocator.next_id(),
            name,
            kind,
            Default::default(),
            scope_start,
        )
        .with_hir_id(hir_id)
        .as_temp();

        self.scope()
            .borrow_mut()
            .insert_after(symbol_info, predicate);
    }

    #[inline(always)]
    pub(crate) fn expr(&mut self, span: tlang_span::Span, kind: hir::ExprKind) -> hir::Expr {
        hir::Expr {
            hir_id: self.unique_id(),
            kind,
            ty: hir::Ty::unknown(),
            span,
        }
    }

    fn lower_node_id(&mut self, id: NodeId) -> HirId {
        if let Some(hir_id) = self.node_id_to_hir_id.get(&id) {
            *hir_id
        } else {
            let hir_id = self.unique_id();
            debug!("Lowering {id:?} to {hir_id:?}");
            self.node_id_to_hir_id.insert(id, hir_id);
            hir_id
        }
    }

    pub fn lower_module(&mut self, module: &ast::node::Module) -> hir::Module {
        debug!(
            "Lowering module with {} statements",
            module.statements.len()
        );

        self.with_scope(module.id, |this| {
            let hir_id = this.lower_node_id(module.id);

            hir::Module {
                hir_id,
                block: hir::Block::new(
                    this.unique_id(),
                    this.lower_stmts(&module.statements),
                    None,
                    module.span,
                ),
                span: module.span,
            }
        })
    }

    #[inline(always)]
    fn lower_stmts(&mut self, stmts: &[ast::node::Stmt]) -> Vec<hir::Stmt> {
        stmts
            .iter()
            .flat_map(|stmt| self.lower_stmt(stmt))
            .collect()
    }

    fn lower_block(&mut self, block: &ast::node::Block) -> hir::Block {
        self.with_scope(block.id, |this| this.lower_block_in_current_scope(block))
    }

    fn lower_block_in_current_scope(&mut self, block: &ast::node::Block) -> hir::Block {
        let hir_id = self.lower_node_id(block.id);
        let stmts = self.lower_stmts(&block.statements);
        let expr = block.expression.as_ref().map(|expr| self.lower_expr(expr));
        hir::Block::new(hir_id, stmts, expr, block.span)
    }

    fn lower_fn_param_pat(&mut self, node: &ast::node::FunctionParameter) -> Ident {
        match &node.pattern.kind {
            ast::node::PatKind::Identifier(box ident) => *ident,
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
        let hir_id = self.lower_node_id(node.pattern.id);
        let name = self.lower_fn_param_pat(node);
        let ty = self.lower_ty(node.type_annotation.as_ref());

        hir::FunctionParameter {
            hir_id,
            name,
            type_annotation: ty,
            span: node.span,
        }
    }

    fn lower_fn_decl(&mut self, decl: &FunctionDeclaration) -> hir::FunctionDeclaration {
        self.with_scope(decl.id, |this| {
            let hir_id = this.lower_node_id(decl.id);
            let name = this.lower_expr(&decl.name);

            let parameters = decl
                .parameters
                .iter()
                .map(|param| this.lower_fn_param(param))
                .collect();
            let body = this.lower_block_in_current_scope(&decl.body);
            let return_type = this.lower_ty(decl.return_type_annotation.as_ref());

            hir::FunctionDeclaration {
                hir_id,
                visibility: decl.visibility,
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
            let segment = hir::PathSegment::from_str(segment.as_str(), segment.span);

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
        hir::PathSegment { ident: *seg }
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
                ty: hir::Ty::unknown(),
                span: node.span,
            },
            ast::node::PatKind::Literal(box literal) => hir::Pat {
                kind: hir::PatKind::Literal(Box::new(*literal)),
                ty: hir::Ty::unknown(),
                span: node.span,
            },
            ast::node::PatKind::Identifier(box ident) => {
                let hir_id = self.lower_node_id(node.id);
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
                    kind: hir::PatKind::Identifier(hir_id, Box::new(*ident)),
                    ty: hir::Ty::unknown(),
                    span: node.span,
                }
            }
            ast::node::PatKind::List(patterns) => hir::Pat {
                kind: hir::PatKind::List(patterns.iter().map(|pat| self.lower_pat(pat)).collect()),
                ty: hir::Ty::unknown(),
                span: node.span,
            },
            ast::node::PatKind::Rest(pattern) => hir::Pat {
                kind: hir::PatKind::Rest(Box::new(self.lower_pat(pattern))),
                ty: hir::Ty::unknown(),
                span: node.span,
            },
            ast::node::PatKind::Enum(box EnumPattern { path, elements }) => {
                let path = self.lower_path(path);
                let elements = elements
                    .iter()
                    .map(|(ident, pat)| (*ident, self.lower_pat(pat)))
                    .collect();

                hir::Pat {
                    kind: hir::PatKind::Enum(Box::new(path), elements),
                    ty: hir::Ty::unknown(),
                    span: node.span,
                }
            }
            ast::node::PatKind::_Self => hir::Pat {
                kind: hir::PatKind::Identifier(
                    self.lower_node_id(node.id),
                    Box::new(Ident::new(kw::_Self, node.span)),
                ),
                ty: hir::Ty::unknown(),
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
            let kind = match &node.kind {
                ast::node::TyKind::Path(path) => hir::TyKind::Path(self.lower_path(path)),
                ast::node::TyKind::Union(paths) => {
                    hir::TyKind::Union(paths.iter().map(|p| self.lower_path(p)).collect())
                }
                ast::node::TyKind::Unknown => hir::TyKind::Unknown,
            };
            hir::Ty {
                kind,
                span: node.span,
            }
        } else {
            hir::Ty {
                kind: hir::TyKind::Unknown,
                span: tlang_span::Span::default(),
            }
        }
    }
}

pub fn lower_to_hir(
    tlang_ast: &ast::node::Module,
    constant_pool_node_ids: &[NodeId],
    symbol_id_allocator: DefIdAllocator,
    root_symbol_table: Rc<RefCell<DefScope>>,
    symbol_tables: HashMap<NodeId, Rc<RefCell<DefScope>>>,
) -> Result<hir::LowerResult, Vec<LoweringError>> {
    lower(
        &mut LoweringContext::new(symbol_id_allocator, root_symbol_table, symbol_tables),
        tlang_ast,
        constant_pool_node_ids,
    )
}

/// Like [`lower_to_hir`], but starts HirId allocation at `hir_id_start`
/// to avoid collisions when lowering multiple modules into the same program.
pub fn lower_to_hir_with_offset(
    tlang_ast: &ast::node::Module,
    constant_pool_node_ids: &[NodeId],
    symbol_id_allocator: DefIdAllocator,
    root_symbol_table: Rc<RefCell<DefScope>>,
    symbol_tables: HashMap<NodeId, Rc<RefCell<DefScope>>>,
    hir_id_start: usize,
) -> Result<hir::LowerResult, Vec<LoweringError>> {
    let mut ctx = LoweringContext::new(symbol_id_allocator, root_symbol_table, symbol_tables);
    ctx.hir_id_allocator = HirIdAllocator::new(hir_id_start);
    lower(&mut ctx, tlang_ast, constant_pool_node_ids)
}

pub fn lower(
    ctx: &mut LoweringContext,
    tlang_ast: &ast::node::Module,
    constant_pool_node_ids: &[NodeId],
) -> Result<hir::LowerResult, Vec<LoweringError>> {
    let root_symbol_table = ctx.lower_node_id(tlang_span::NodeId::new(1));
    let module = ctx.lower_module(tlang_ast);
    let symbol_tables = ctx.symbol_tables();
    let symbol_id_allocator = ctx.symbol_id_allocator;
    let hir_id_allocator = ctx.hir_id_allocator;

    let constant_pool_ids = constant_pool_node_ids
        .iter()
        .filter_map(|node_id| ctx.node_id_to_hir_id.get(node_id).copied())
        .collect();

    if !ctx.errors.is_empty() {
        return Err(std::mem::take(&mut ctx.errors));
    }

    Ok((
        module,
        hir::LowerResultMeta {
            root_symbol_table,
            symbol_tables,
            hir_id_allocator,
            symbol_id_allocator,
            constant_pool_ids,
        },
    ))
}
