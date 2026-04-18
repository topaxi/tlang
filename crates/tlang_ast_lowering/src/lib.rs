#![feature(box_patterns)]
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt;
use std::sync::{Arc, RwLock};

use log::debug;
use tlang_ast as ast;
use tlang_ast::keyword::kw;
use tlang_ast::node::{EnumPattern, FunctionDeclaration, Ident};
use tlang_defs::{Def, DefIdAllocator, DefKind, DefScope};
use tlang_hir as hir;
use tlang_span::{HirId, HirIdAllocator, NodeId, Span, TypeVarId, TypeVarIdAllocator};

/// Context for implicit self-dispatch in protocol default method implementations.
///
/// When lowering a protocol default method body, this context tracks a dispatch
/// map and the `NodeId` of the `self` parameter.  Any `self.method(args…)` call
/// where `method` is in `method_dispatch_map` is automatically rewritten to
/// `OwningProtocol::method(self, args…)`.
///
/// The dispatch map includes methods from the protocol itself **and** from all
/// transitively-reachable constraint protocols, so that default bodies can freely
/// call `self.eq(other)` when the protocol is `Ord : Eq`, for example.
#[derive(Debug, Clone)]
pub(crate) struct ProtocolDispatchContext {
    /// Maps method name → the `Ident` of the protocol that declares that method.
    /// Includes the current protocol's own methods and all transitively-inherited
    /// constraint protocol methods (nearest/direct protocol wins on conflict).
    pub(crate) method_dispatch_map: HashMap<String, Ident>,
    /// The `NodeId` of the `self` parameter's pattern node in the current default
    /// method.  Stored here so that later passes can cross-check the identity of the
    /// `self` receiver without relying on string comparison alone.
    pub(crate) self_param_node_id: NodeId,
}

/// Pre-scanned information about a protocol declaration, used to build
/// `ProtocolDispatchContext` dispatch maps that include constraint methods.
#[derive(Debug, Clone)]
pub(crate) struct ProtocolRegistryEntry {
    /// The protocol's name identifier.
    pub(crate) ident: Ident,
    /// Names of methods directly declared in this protocol.
    pub(crate) methods: Vec<String>,
    /// Names of direct constraint protocols (e.g. `["Eq"]` for `Ord : Eq`).
    pub(crate) constraints: Vec<String>,
}

/// Errors that can occur during AST-to-HIR lowering.
#[derive(Debug, Clone)]
pub enum LoweringError {
    /// A function declaration has an invalid name expression (not a path or field expression).
    InvalidFunctionName { span: Span },
}

impl LoweringError {
    /// Return the source span associated with this error, if any.
    pub fn span(&self) -> Span {
        match self {
            LoweringError::InvalidFunctionName { span } => *span,
        }
    }
}

impl fmt::Display for LoweringError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
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
    /// Maps every AST `NodeId` to its corresponding `HirId`.
    ///
    /// Created lazily via [`Self::lower_node_id`]: the first call for a given
    /// `NodeId` allocates a fresh `HirId`; subsequent calls return the same
    /// one.  This map drives two things:
    ///
    /// 1. **Scope keying** – parser-created scopes (keyed by `NodeId`) are
    ///    translated to `HirId`-keyed scopes in [`Self::symbol_tables`].  Each
    ///    function *clause* gets its own unique `HirId` so that its parameter
    ///    scope is reachable independently.
    /// 2. **Constant-pool translation** – `NodeId`s of constant-pool nodes
    ///    are converted to `HirId`s after lowering.
    ///
    /// **Invariant**: a function clause `NodeId` is always present in *both*
    /// this map (unique per-clause `HirId`, established by the
    /// `lower_node_id` calls at the top of [`Self::lower_fn_decls`]) *and* in
    /// [`Self::fn_node_id_to_hir_id`] (shared `HirId` for the whole function
    /// group).  Symbol-resolution lookups therefore check
    /// `fn_node_id_to_hir_id` first so that symbols that *refer* to the
    /// function get the canonical group `HirId`, while the per-clause `HirId`
    /// in this map is used only for scope indexing.
    node_id_to_hir_id: HashMap<NodeId, HirId>,
    /// Maps every function-clause `NodeId` to the *shared* `HirId` of the
    /// logical function group it belongs to.
    ///
    /// A function defined with multiple pattern-matching clauses (e.g.
    /// `fn fib(0) { … }` / `fn fib(n) { … }`) is lowered to a **single**
    /// HIR `FunctionDeclaration`.  All clauses therefore share one `HirId`
    /// so that call-site symbols resolve to the same declaration regardless
    /// of which clause was matched.
    ///
    /// This map is populated in `setup_function_declaration_metadata` and
    /// is queried *before* `node_id_to_hir_id` in symbol-resolution lookups
    /// (see [`Self::symbol_tables`]).  Never collapse this map into
    /// `node_id_to_hir_id`: doing so would overwrite the unique per-clause
    /// `HirId`s that are needed for scope keying, causing parameter scopes
    /// of multi-clause functions to collide.
    fn_node_id_to_hir_id: HashMap<NodeId, HirId>,
    hir_id_allocator: HirIdAllocator,
    symbol_id_allocator: DefIdAllocator,
    symbol_tables: HashMap<NodeId, Arc<RwLock<DefScope>>>,
    new_symbol_tables: HashMap<HirId, Arc<RwLock<DefScope>>>,
    current_symbol_table: Arc<RwLock<DefScope>>,
    errors: Vec<LoweringError>,
    /// Active protocol self-dispatch context, set while lowering a protocol
    /// default method body.  `None` outside of such bodies.
    pub(crate) protocol_dispatch_ctx: Option<ProtocolDispatchContext>,
    /// Pre-scanned registry of all protocol declarations in the module being
    /// lowered.  Populated by `lower_module` before any statement is lowered,
    /// so that `lower_protocol_decl` can look up constraint protocols' method
    /// lists when building a `ProtocolDispatchContext`.
    protocol_registry: HashMap<String, ProtocolRegistryEntry>,
    /// HirIds of `CallExpression` nodes that were desugared from the `|>`
    /// pipeline operator.  Collected during lowering and forwarded to
    /// [`hir::LowerResultMeta`] so that downstream passes can identify
    /// pipeline-originated calls without modifying the HIR structure.
    pub(crate) pipeline_call_ids: HashSet<HirId>,
    type_var_id_allocator: TypeVarIdAllocator,
    /// Stack of active type parameter scopes. Each entry maps type parameter
    /// names to their `TypeVarId`s. Pushed when entering a generic declaration
    /// (function, struct, enum, protocol) and popped when leaving.
    type_param_scopes: Vec<HashMap<String, TypeVarId>>,
}

impl LoweringContext {
    pub fn new(
        symbol_id_allocator: DefIdAllocator,
        root_symbol_table: Arc<RwLock<DefScope>>,
        symbol_tables: HashMap<NodeId, Arc<RwLock<DefScope>>>,
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
            protocol_dispatch_ctx: None,
            protocol_registry: HashMap::default(),
            pipeline_call_ids: HashSet::default(),
            type_var_id_allocator: TypeVarIdAllocator::new(1),
            type_param_scopes: Vec::new(),
        }
    }

    /// # Panics
    ///
    /// Panics if any symbol table lock is poisoned.
    pub fn symbol_tables(&self) -> HashMap<HirId, Arc<RwLock<DefScope>>> {
        debug!("Translating symbol tables to HirIds");

        let mut symbol_tables = self.new_symbol_tables.clone();

        for (node_id, symbol_table) in &self.symbol_tables {
            if let Some(hir_id) = self.node_id_to_hir_id.get(node_id) {
                let symbol_table = symbol_table.clone();

                let fn_node_id_to_hir_id = &self.fn_node_id_to_hir_id;
                let node_id_to_hir_id = &self.node_id_to_hir_id;

                symbol_table
                    .write()
                    .unwrap()
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
    pub(crate) fn scope(&self) -> Arc<RwLock<DefScope>> {
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
        self.scope().read().unwrap().has_multi_arity_fn(name, arity)
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
                .write()
                .unwrap()
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
        F: FnOnce(&mut Self, Arc<RwLock<DefScope>>) -> (HirId, R),
        R: hir::HirScope,
    {
        let previous_symbol_table = self.current_symbol_table.clone();
        self.current_symbol_table =
            Arc::new(RwLock::new(DefScope::new(previous_symbol_table.clone())));
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

        self.scope().write().unwrap().insert(symbol_info);
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

        self.scope().write().unwrap().insert_at(index, symbol_info);
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
            .write()
            .unwrap()
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

        // Pre-scan all protocol declarations so that `lower_protocol_decl` can
        // look up constraint protocol methods when building dispatch contexts.
        for stmt in &module.statements {
            if let ast::node::StmtKind::ProtocolDeclaration(decl) = &stmt.kind {
                self.protocol_registry.insert(
                    decl.name.to_string(),
                    ProtocolRegistryEntry {
                        ident: decl.name,
                        methods: decl.methods.iter().map(|m| m.name.to_string()).collect(),
                        constraints: decl.constraints.iter().map(|c| c.to_string()).collect(),
                    },
                );
            }
        }

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

    /// Builds a method-dispatch map for a protocol that includes methods from
    /// transitively-reachable constraint protocols.
    ///
    /// The map value is the `Ident` of the protocol that *directly* declares the
    /// method.  When two constraint paths both expose the same method name (diamond
    /// case), the first one encountered wins (de-duplication via `entry().or_insert`).
    ///
    /// The `current_protocol_ident` and `current_protocol_methods` arguments
    /// represent the protocol being lowered itself — its methods are added first so
    /// they always take priority over any identically-named constraint method.
    pub(crate) fn build_protocol_dispatch_map(
        &self,
        current_protocol_ident: Ident,
        current_protocol_methods: &[String],
        constraints: &[String],
    ) -> HashMap<String, Ident> {
        let mut map: HashMap<String, Ident> = HashMap::new();

        // Current protocol's own methods (highest priority).
        for method_name in current_protocol_methods {
            map.insert(method_name.clone(), current_protocol_ident);
        }

        // Transitively add constraint protocol methods using FIFO traversal so
        // that left-to-right declaration order is preserved.  Because `or_insert`
        // keeps the first-seen owner, earlier (left-hand) constraints win on
        // method-name conflicts, which matches the documented priority.
        let mut to_visit: VecDeque<String> = constraints.iter().cloned().collect();
        let mut visited: HashSet<String> = HashSet::new();

        while let Some(constraint_name) = to_visit.pop_front() {
            if !visited.insert(constraint_name.clone()) {
                continue; // Already processed — handles diamond constraints.
            }
            if let Some(entry) = self.protocol_registry.get(&constraint_name) {
                for method_name in &entry.methods {
                    // or_insert: current protocol and earlier constraints win.
                    map.entry(method_name.clone()).or_insert(entry.ident);
                }
                // Queue this constraint's own constraints for transitive lookup.
                for nested in &entry.constraints {
                    to_visit.push_back(nested.clone());
                }
            }
        }

        map
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
        let has_type_annotation = node.type_annotation.is_some();
        let ty = self.lower_ty(node.type_annotation.as_ref());

        hir::FunctionParameter {
            hir_id,
            name,
            type_annotation: ty,
            has_type_annotation,
            span: node.span,
        }
    }

    fn seed_dot_method_receiver_type(
        &self,
        name: &hir::Expr,
        owner_type_params: &[hir::TypeParam],
        parameters: &mut [hir::FunctionParameter],
    ) {
        let hir::ExprKind::FieldAccess(base, _) = &name.kind else {
            return;
        };
        let Some(receiver_path) = base.path() else {
            return;
        };
        let Some(receiver_param) = parameters.first_mut() else {
            return;
        };
        if !matches!(receiver_param.type_annotation.kind, hir::TyKind::Unknown) {
            return;
        }

        receiver_param.type_annotation = hir::Ty {
            kind: hir::TyKind::Path(
                receiver_path.clone(),
                owner_type_params
                    .iter()
                    .map(|type_param| hir::Ty {
                        kind: hir::TyKind::Var(type_param.type_var_id),
                        ..hir::Ty::default()
                    })
                    .collect(),
            ),
            ..hir::Ty::default()
        };
    }

    fn lower_fn_decl(&mut self, decl: &FunctionDeclaration) -> hir::FunctionDeclaration {
        self.with_scope(decl.id, |this| {
            let hir_id = this.lower_node_id(decl.id);
            let name = this.lower_expr(&decl.name);

            // Push type parameter scope *before* lowering parameters, return
            // type and body so that type annotations can resolve `T` → Var(id).
            let owner_type_params = this.lower_type_params(&decl.owner_type_params);
            let type_params = this.lower_type_params(&decl.type_params);

            let mut parameters = decl
                .parameters
                .iter()
                .map(|param| this.lower_fn_param(param))
                .collect::<Vec<_>>();
            this.seed_dot_method_receiver_type(&name, &owner_type_params, &mut parameters);
            let body = this.lower_block_in_current_scope(&decl.body);
            let return_type = this.lower_ty(decl.return_type_annotation.as_ref());

            this.pop_type_param_scope();
            this.pop_type_param_scope();

            hir::FunctionDeclaration {
                hir_id,
                visibility: decl.visibility,
                name,
                owner_type_params,
                type_params,
                parameters,
                params_span: decl.params_span,
                return_hint_spans: decl
                    .return_type_annotation
                    .is_none()
                    .then_some(decl.params_span)
                    .into_iter()
                    .collect(),
                return_hint_arm_indices: Vec::new(),
                return_type,
                has_return_type: decl.return_type_annotation.is_some(),
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

    /// Lower AST type parameters to HIR, allocating a fresh `TypeVarId` for
    /// each. Also pushes a type parameter scope so that `lower_ty_path` can
    /// resolve type parameter names (e.g. `T`) to `TyKind::Var(id)`.
    ///
    /// Constraint bounds (e.g. `T: Display + Clone`) are lowered after the
    /// scope is pushed, so that bounds referencing other type parameters
    /// (e.g. `<A, B: From<A>>`) can resolve correctly.
    ///
    /// **Must** be paired with a call to [`pop_type_param_scope`] after lowering
    /// the body that uses these type parameters.
    fn lower_type_params(&mut self, params: &[ast::node::TypeParam]) -> Vec<hir::TypeParam> {
        let mut scope = HashMap::new();
        // First pass: allocate type variable IDs and build scope.
        let mut hir_params: Vec<_> = params
            .iter()
            .map(|p| {
                let hir_id = self.lower_node_id(p.id);
                let type_var_id = self.type_var_id_allocator.next_id();
                scope.insert(p.name.as_str().to_string(), type_var_id);
                hir::TypeParam {
                    hir_id,
                    name: p.name,
                    type_var_id,
                    bounds: Vec::new(),
                    span: p.span,
                }
            })
            .collect();
        self.type_param_scopes.push(scope);

        // Second pass: lower bounds with the scope active so that
        // references to sibling type params (e.g. `B: From<A>`) resolve.
        // Safety: `hir_params` and `params` have the same length since
        // `hir_params` was built from iterating `params` above.
        for (hir_param, ast_param) in hir_params.iter_mut().zip(params.iter()) {
            hir_param.bounds = ast_param
                .bounds
                .iter()
                .map(|b| self.lower_ty(Some(b)))
                .collect();
        }

        hir_params
    }

    /// Pop the most recently pushed type parameter scope. Must be called after
    /// lowering the body that was covered by the corresponding
    /// [`lower_type_params`] call.
    fn pop_type_param_scope(&mut self) {
        self.type_param_scopes.pop();
    }

    /// Look up a single-segment name in the active type parameter scopes.
    /// Returns the `TypeVarId` if found (innermost scope wins).
    fn resolve_type_param(&self, name: &str) -> Option<TypeVarId> {
        for scope in self.type_param_scopes.iter().rev() {
            if let Some(&id) = scope.get(name) {
                return Some(id);
            }
        }
        None
    }

    fn lower_ty(&mut self, node: Option<&ast::node::Ty>) -> hir::Ty {
        debug!("Lowering type {node:?}");

        if let Some(node) = node {
            let kind = match &node.kind {
                ast::node::TyKind::Path(path) => {
                    self.lower_ty_path_with_params(path, &node.parameters)
                }
                ast::node::TyKind::Union(paths) => hir::TyKind::Union(
                    paths
                        .iter()
                        .map(|p| {
                            let kind = self.lower_ty_path(p);
                            hir::Ty {
                                res: None,
                                kind,
                                span: p.span,
                            }
                        })
                        .collect(),
                ),
                ast::node::TyKind::Fn(params, ret) => {
                    let param_tys = params.iter().map(|p| self.lower_ty(Some(&p.ty))).collect();
                    let ret_ty = self.lower_ty(Some(ret));
                    hir::TyKind::Fn(param_tys, Box::new(ret_ty))
                }
                ast::node::TyKind::Unknown => hir::TyKind::Unknown,
            };
            let res = self.node_id_to_hir_id.get(&node.id).copied();
            hir::Ty {
                res,
                kind,
                span: node.span,
            }
        } else {
            hir::Ty {
                res: None,
                kind: hir::TyKind::Unknown,
                span: tlang_span::Span::default(),
            }
        }
    }

    /// Lower a type path, mapping known primitive names to `TyKind::Primitive`
    /// and type parameter names to `TyKind::Var`.
    fn lower_ty_path(&mut self, path: &ast::node::Path) -> hir::TyKind {
        self.lower_ty_path_with_params(path, &[])
    }

    /// Lower a type path together with its type parameters, recognising
    /// built-in generic collection types:
    ///
    /// - `List<T>` → `TyKind::List(T)`
    /// - `Slice<T>` → `TyKind::Slice(T)`
    /// - `Dict<K, V>` → `TyKind::Dict(K, V)`
    fn lower_ty_path_with_params(
        &mut self,
        path: &ast::node::Path,
        params: &[ast::node::Ty],
    ) -> hir::TyKind {
        if path.segments.len() == 1 {
            let name = path.segments[0].as_str();

            // List<T> → TyKind::List(T)
            if name == "List" && params.len() == 1 {
                let elem_ty = self.lower_ty(Some(&params[0]));
                return hir::TyKind::List(Box::new(elem_ty));
            }

            // Slice<T> → TyKind::Slice(T)
            if name == "Slice" && params.len() == 1 {
                let elem_ty = self.lower_ty(Some(&params[0]));
                return hir::TyKind::Slice(Box::new(elem_ty));
            }

            // Dict<K, V> → TyKind::Dict(K, V)
            if name == "Dict" && params.len() == 2 {
                let key_ty = self.lower_ty(Some(&params[0]));
                let val_ty = self.lower_ty(Some(&params[1]));
                return hir::TyKind::Dict(Box::new(key_ty), Box::new(val_ty));
            }

            if let Some(prim) = Self::name_to_prim_ty(name) {
                return hir::TyKind::Primitive(prim);
            }
            if let Some(type_var_id) = self.resolve_type_param(name) {
                return hir::TyKind::Var(type_var_id);
            }
        }
        hir::TyKind::Path(
            self.lower_path(path),
            params
                .iter()
                .map(|param| self.lower_ty(Some(param)))
                .collect(),
        )
    }

    fn name_to_prim_ty(name: &str) -> Option<hir::PrimTy> {
        match name {
            "bool" => Some(hir::PrimTy::Bool),
            "i8" => Some(hir::PrimTy::I8),
            "i16" => Some(hir::PrimTy::I16),
            "i32" => Some(hir::PrimTy::I32),
            "i64" => Some(hir::PrimTy::I64),
            "isize" => Some(hir::PrimTy::Isize),
            "u8" => Some(hir::PrimTy::U8),
            "u16" => Some(hir::PrimTy::U16),
            "u32" => Some(hir::PrimTy::U32),
            "u64" => Some(hir::PrimTy::U64),
            "usize" => Some(hir::PrimTy::Usize),
            "f32" => Some(hir::PrimTy::F32),
            "f64" => Some(hir::PrimTy::F64),
            "char" => Some(hir::PrimTy::Char),
            "String" => Some(hir::PrimTy::String),
            "nil" => Some(hir::PrimTy::Nil),
            _ => None,
        }
    }
}

pub fn lower_to_hir(
    tlang_ast: &ast::node::Module,
    constant_pool_node_ids: &[NodeId],
    symbol_id_allocator: DefIdAllocator,
    root_symbol_table: Arc<RwLock<DefScope>>,
    symbol_tables: HashMap<NodeId, Arc<RwLock<DefScope>>>,
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
    root_symbol_table: Arc<RwLock<DefScope>>,
    symbol_tables: HashMap<NodeId, Arc<RwLock<DefScope>>>,
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
            pipeline_call_ids: std::mem::take(&mut ctx.pipeline_call_ids),
        },
    ))
}
