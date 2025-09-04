#![feature(box_patterns)]
#![feature(if_let_guard)]
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use log::{debug, warn};
use tlang_ast as ast;
use tlang_ast::keyword::kw;
use tlang_ast::node::{EnumPattern, FunctionDeclaration, Ident};
use tlang_hir::hir;
use tlang_span::{HirId, HirIdAllocator, LineColumn, NodeId};
use tlang_symbols::{SymbolIdAllocator, SymbolInfo, SymbolTable, SymbolType};

mod expr;
mod r#loop;
mod stmt;

#[derive(Debug)]
pub struct LoweringContext {
    node_id_to_hir_id: HashMap<NodeId, HirId>,
    fn_node_id_to_hir_id: HashMap<NodeId, HirId>,
    hir_id_allocator: HirIdAllocator,
    symbol_id_allocator: SymbolIdAllocator,
    symbol_tables: HashMap<NodeId, Rc<RefCell<SymbolTable>>>,
    new_symbol_tables: HashMap<HirId, Rc<RefCell<SymbolTable>>>,
    current_symbol_table: Rc<RefCell<SymbolTable>>,
}

impl LoweringContext {
    pub fn new(
        symbol_id_allocator: SymbolIdAllocator,
        root_symbol_table: Rc<RefCell<SymbolTable>>,
        symbol_tables: HashMap<NodeId, Rc<RefCell<SymbolTable>>>,
    ) -> Self {
        Self {
            hir_id_allocator: HirIdAllocator::default(),
            node_id_to_hir_id: HashMap::default(),
            fn_node_id_to_hir_id: HashMap::default(),
            symbol_id_allocator,
            symbol_tables,
            new_symbol_tables: HashMap::default(),
            current_symbol_table: root_symbol_table,
        }
    }

    pub fn symbol_tables(&self) -> HashMap<HirId, Rc<RefCell<SymbolTable>>> {
        debug!("Translating symbol tables to HirIds");

        let mut symbol_tables = self.new_symbol_tables.clone();

        for (node_id, symbol_table) in &self.symbol_tables {
            if let Some(hir_id) = self.node_id_to_hir_id.get(node_id) {
                let symbol_table = symbol_table.clone();

                // Capture variables needed in the closure
                let fn_node_id_to_hir_id = &self.fn_node_id_to_hir_id;
                let node_id_to_hir_id = &self.node_id_to_hir_id;

                symbol_table
                    .borrow()
                    .for_each_local_symbol_mut(|symbol| {
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
                                warn!(
                                    "Unable to map {:?} to HirId for symbol {}",
                                    node_id, symbol.name
                                );
                            }
                        }
                    });

                symbol_tables.insert(*hir_id, symbol_table);
            } else {
                warn!("No HirId found for NodeId: {:?}", node_id);
            }
        }

        debug!("NodeId to HirId mapping: {:#?}", self.node_id_to_hir_id);

        symbol_tables
    }

    #[inline(always)]
    pub(crate) fn scope(&self) -> Rc<RefCell<SymbolTable>> {
        self.current_symbol_table.clone()
    }

    fn has_multi_arity_fn(&self, name: &str, arity: usize) -> bool {
        self.scope().borrow().has_multi_arity_fn(name, arity)
    }

    pub(crate) fn with_scope<F, R>(&mut self, node_id: NodeId, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
        R: hir::HirScope,
    {
        debug!("Entering scope for node_id: {:?}", node_id);

        let previous_symbol_table = self.current_symbol_table.clone();
        if let Some(symbol_table) = self.symbol_tables.get(&node_id).cloned() {
            self.current_symbol_table = symbol_table;
            // Note: With the new storage architecture, parent relationships are 
            // encoded in scope indices and no longer need explicit re-parenting
        }

        let result = f(self);

        if self.symbol_tables.contains_key(&node_id) {
            debug!("Leaving scope for node_id: {:?}", node_id);
        }
        self.current_symbol_table = previous_symbol_table;

        result
    }

    pub(crate) fn with_new_scope<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self, Rc<RefCell<SymbolTable>>) -> (HirId, R),
        R: hir::HirScope,
    {
        let previous_symbol_table = self.current_symbol_table.clone();
        self.current_symbol_table = Rc::new(RefCell::new(SymbolTable::new_child(
            previous_symbol_table.clone(),
        )));
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
        symbol_type: SymbolType,
        scope_start: LineColumn,
    ) {
        let symbol_info = SymbolInfo::new(
            self.symbol_id_allocator.next_id(),
            name,
            symbol_type,
            Default::default(),
            scope_start,
        )
        .with_hir_id(hir_id)
        .as_temp();

        self.scope().borrow_mut().insert(symbol_info);
    }

    pub(crate) fn define_symbol_at(
        &mut self,
        _index: usize,
        hir_id: HirId,
        name: &str,
        symbol_type: SymbolType,
        scope_start: LineColumn,
    ) {
        let symbol_info = SymbolInfo::new(
            self.symbol_id_allocator.next_id(),
            name,
            symbol_type,
            Default::default(),
            scope_start,
        )
        .with_hir_id(hir_id)
        .as_temp();

        self.scope().borrow_mut().insert(symbol_info);
    }

    pub(crate) fn define_symbol_after(
        &mut self,
        hir_id: HirId,
        name: &str,
        symbol_type: SymbolType,
        scope_start: LineColumn,
        _predicate: impl Fn(&SymbolInfo) -> bool,
    ) {
        let symbol_info = SymbolInfo::new(
            self.symbol_id_allocator.next_id(),
            name,
            symbol_type,
            Default::default(),
            scope_start,
        )
        .with_hir_id(hir_id)
        .as_temp();

        self.scope()
            .borrow_mut()
            .insert(symbol_info);
    }

    #[inline(always)]
    pub(crate) fn expr(&mut self, span: tlang_span::Span, kind: hir::ExprKind) -> hir::Expr {
        hir::Expr {
            hir_id: self.unique_id(),
            kind,
            span,
        }
    }

    fn lower_node_id(&mut self, id: NodeId) -> HirId {
        if let Some(hir_id) = self.node_id_to_hir_id.get(&id) {
            *hir_id
        } else {
            let hir_id = self.unique_id();
            debug!("Lowering {:?} to {:?}", id, hir_id);
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
                span: tlang_span::Span::default(),
            }
        }
    }
}

pub fn lower_to_hir(
    tlang_ast: &ast::node::Module,
    symbol_id_allocator: SymbolIdAllocator,
    root_symbol_table: Rc<RefCell<SymbolTable>>,
    symbol_tables: HashMap<NodeId, Rc<RefCell<SymbolTable>>>,
) -> hir::LowerResult {
    lower(
        &mut LoweringContext::new(symbol_id_allocator, root_symbol_table, symbol_tables),
        tlang_ast,
    )
}

pub fn lower(ctx: &mut LoweringContext, tlang_ast: &ast::node::Module) -> hir::LowerResult {
    let root_symbol_table = ctx.lower_node_id(tlang_span::NodeId::new(1));
    let module = ctx.lower_module(tlang_ast);
    let symbol_tables = ctx.symbol_tables();
    let symbol_id_allocator = ctx.symbol_id_allocator;
    let hir_id_allocator = ctx.hir_id_allocator;

    (
        module,
        hir::LowerResultMeta {
            root_symbol_table,
            symbol_tables,
            hir_id_allocator,
            symbol_id_allocator,
        },
    )
}
