use tlang_span::{HirId, LineColumn, Span};

use crate::hir;

/// Identifies the kind of HIR node stored in a [`SpanIndex`] entry.
///
/// Each variant carries the [`HirId`] of the referenced node so callers can
/// look the node up in their own data structures (e.g. symbol tables or type
/// maps).
///
/// Note: [`Pat`](SpanNode::Pat) uses the [`HirId`] that is embedded inside
/// [`hir::PatKind::Identifier`]; only identifier-binding patterns are
/// recorded.  [`Path`](SpanNode::Path) uses the resolved definition
/// [`HirId`] from [`hir::Res::hir_id`]; paths that have no resolved
/// definition (e.g. unresolved or primitive-type references) are not
/// recorded.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SpanNode {
    Expr(HirId),
    Stmt(HirId),
    /// Only emitted for [`hir::PatKind::Identifier`] patterns.
    Pat(HirId),
    /// Carries the *definition* [`HirId`] that the path resolves to
    /// (`path.res.hir_id()`).  Paths without a definition HirId are omitted.
    Path(HirId),
}

/// A flat, sorted index of (span, HIR node) pairs built from a lowered
/// [`hir::Module`].
///
/// # Building
///
/// ```ignore
/// let index = SpanIndex::build(&module);
/// ```
///
/// # Querying
///
/// ```ignore
/// // By byte offset
/// if let Some(node) = index.node_at_offset(42) { ... }
///
/// // By 0-indexed line / column
/// if let Some(node) = index.node_at_lc(3, 15) { ... }
/// ```
pub struct SpanIndex {
    /// Flat list of (span, node) sorted by span start ascending, then by span
    /// length *descending* (so that outer/larger spans precede inner/smaller
    /// spans within the same start position).  This ordering lets
    /// [`node_at_offset`](SpanIndex::node_at_offset) scan backwards from the
    /// binary-search partition point and find the innermost node first.
    entries: Vec<(Span, SpanNode)>,
}

impl SpanIndex {
    /// Build the index by performing a single depth-first walk of `module`.
    ///
    /// Time complexity: O(n) where n is the number of HIR nodes.
    pub fn build(module: &hir::Module) -> Self {
        let mut entries = Vec::new();
        collect_module(&mut entries, module);
        // Sort by span start ascending, then by length descending so that
        // outer (larger) spans appear before inner (smaller) spans that share
        // the same start position.  This is important for the backward scan in
        // `node_at_offset`.
        entries.sort_unstable_by(|(a, _), (b, _)| {
            a.start.cmp(&b.start).then_with(|| {
                let len_a = a.end - a.start;
                let len_b = b.end - b.start;
                len_b.cmp(&len_a) // descending: longer (outer) spans first
            })
        });
        SpanIndex { entries }
    }

    /// Return the innermost node whose span contains `offset` (exclusive end).
    ///
    /// "Innermost" means the node with the *smallest* span that still
    /// contains the offset.  Returns `None` if no node covers the offset.
    pub fn node_at_offset(&self, offset: u32) -> Option<&SpanNode> {
        // All entries with `span.start <= offset` form a prefix.  Among those,
        // scanning backwards finds the shortest (innermost) span first because
        // of the sort order (start asc, length desc).
        let partition = self
            .entries
            .partition_point(|(span, _)| span.start <= offset);
        self.entries[..partition]
            .iter()
            .rev()
            .find(|(span, _)| offset < span.end)
            .map(|(_, node)| node)
    }

    /// Return the innermost node whose span contains the given (0-indexed)
    /// line/column position.
    ///
    /// Because every [`Span`] stores pre-computed [`LineColumn`] values, this
    /// method does not require access to the source text.  Comparison uses the
    /// `start_lc ≤ position < end_lc` half-open interval.
    ///
    /// **Performance:** O(n) linear scan, where n is the number of index
    /// entries.  The entries are sorted by byte offset, not by line/column, so
    /// binary search is not applicable here.  For typical source files (up to
    /// ~10 000 nodes) this is fast enough for interactive LSP use.
    pub fn node_at_lc(&self, line: u32, column: u32) -> Option<&SpanNode> {
        let lc = LineColumn::new(line, column);
        // Linear scan: the entries are sorted by byte offset, not LC, so we
        // cannot binary-search here.  For typical source files (thousands of
        // nodes) this is fast enough.
        self.entries
            .iter()
            .filter(|(span, _)| span.start_lc <= lc && lc < span.end_lc)
            .min_by_key(|(span, _)| span.end - span.start)
            .map(|(_, node)| node)
    }

    /// Return all (span, node) entries in the index.
    pub fn entries(&self) -> &[(Span, SpanNode)] {
        &self.entries
    }
}

// ---------------------------------------------------------------------------
// Internal recursive collectors (immutable walk of the HIR)
// ---------------------------------------------------------------------------

fn collect_module(entries: &mut Vec<(Span, SpanNode)>, module: &hir::Module) {
    collect_block(entries, &module.block);
}

fn collect_block(entries: &mut Vec<(Span, SpanNode)>, block: &hir::Block) {
    for stmt in &block.stmts {
        collect_stmt(entries, stmt);
    }
    if let Some(expr) = &block.expr {
        collect_expr(entries, expr);
    }
}

fn collect_stmt(entries: &mut Vec<(Span, SpanNode)>, stmt: &hir::Stmt) {
    entries.push((stmt.span, SpanNode::Stmt(stmt.hir_id)));
    match &stmt.kind {
        hir::StmtKind::Expr(expr) => collect_expr(entries, expr),
        hir::StmtKind::Let(pat, expr, _) => {
            collect_expr(entries, expr);
            collect_pat(entries, pat);
        }
        hir::StmtKind::Const(_, pat, expr, _) => {
            collect_expr(entries, expr);
            collect_pat(entries, pat);
        }
        hir::StmtKind::Return(expr) => {
            if let Some(expr) = expr.as_ref() {
                collect_expr(entries, expr);
            }
        }
        hir::StmtKind::FunctionDeclaration(decl) => {
            collect_fn_decl(entries, decl);
        }
        hir::StmtKind::DynFunctionDeclaration(_) => {}
        hir::StmtKind::EnumDeclaration(decl) => {
            for variant in &decl.variants {
                if let Some(discriminant) = &variant.discriminant {
                    collect_expr(entries, discriminant);
                }
            }
            for const_item in &decl.consts {
                collect_expr(entries, &const_item.value);
            }
        }
        hir::StmtKind::StructDeclaration(decl) => {
            for const_item in &decl.consts {
                collect_expr(entries, &const_item.value);
            }
        }
        hir::StmtKind::ProtocolDeclaration(decl) => {
            for constraint in &decl.constraints {
                collect_path(entries, constraint);
            }
            for method in &decl.methods {
                if let Some(body) = &method.body {
                    collect_block(entries, body);
                }
            }
            for const_item in &decl.consts {
                collect_expr(entries, &const_item.value);
            }
        }
        hir::StmtKind::ImplBlock(impl_block) => {
            collect_path(entries, &impl_block.protocol_name);
            collect_path(entries, &impl_block.target_type);
            for decl in &impl_block.methods {
                collect_fn_decl(entries, decl);
            }
        }
    }
}

fn collect_fn_decl(entries: &mut Vec<(Span, SpanNode)>, decl: &hir::FunctionDeclaration) {
    collect_expr(entries, &decl.name);
    collect_block(entries, &decl.body);
}

fn collect_expr(entries: &mut Vec<(Span, SpanNode)>, expr: &hir::Expr) {
    entries.push((expr.span, SpanNode::Expr(expr.hir_id)));
    match &expr.kind {
        hir::ExprKind::Path(path) => collect_path(entries, path),
        hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
            collect_block(entries, block);
        }
        hir::ExprKind::Call(call) | hir::ExprKind::TailCall(call) => {
            collect_expr(entries, &call.callee);
            for arg in &call.arguments {
                collect_expr(entries, arg);
            }
        }
        hir::ExprKind::Binary(_, lhs, rhs) => {
            collect_expr(entries, lhs);
            collect_expr(entries, rhs);
        }
        hir::ExprKind::Unary(_, inner) => collect_expr(entries, inner),
        hir::ExprKind::Let(pat, inner) => {
            collect_expr(entries, inner);
            collect_pat(entries, pat);
        }
        hir::ExprKind::IfElse(cond, then_block, else_clauses) => {
            collect_expr(entries, cond);
            collect_block(entries, then_block);
            for clause in else_clauses {
                if let Some(cond) = &clause.condition {
                    collect_expr(entries, cond);
                }
                collect_block(entries, &clause.consequence);
            }
        }
        hir::ExprKind::FunctionExpression(decl) => collect_fn_decl(entries, decl),
        hir::ExprKind::FieldAccess(base, _) => collect_expr(entries, base),
        hir::ExprKind::IndexAccess(base, idx) => {
            collect_expr(entries, base);
            collect_expr(entries, idx);
        }
        hir::ExprKind::Match(scrutinee, arms) => {
            collect_expr(entries, scrutinee);
            for arm in arms {
                collect_pat(entries, &arm.pat);
                if let Some(guard) = &arm.guard {
                    collect_expr(entries, guard);
                }
                collect_block(entries, &arm.block);
            }
        }
        hir::ExprKind::List(exprs) => {
            for e in exprs {
                collect_expr(entries, e);
            }
        }
        hir::ExprKind::Dict(pairs) => {
            for (key, val) in pairs {
                collect_expr(entries, key);
                collect_expr(entries, val);
            }
        }
        hir::ExprKind::Cast(inner, _) => collect_expr(entries, inner),
        hir::ExprKind::Implements(inner, _) => collect_expr(entries, inner),
        hir::ExprKind::TaggedString { tag, exprs, .. } => {
            collect_expr(entries, tag);
            for e in exprs {
                collect_expr(entries, e);
            }
        }
        hir::ExprKind::Range(range) => {
            collect_expr(entries, &range.start);
            collect_expr(entries, &range.end);
        }
        hir::ExprKind::Break(Some(inner)) => collect_expr(entries, inner),
        hir::ExprKind::Literal(_)
        | hir::ExprKind::Break(None)
        | hir::ExprKind::Continue
        | hir::ExprKind::Wildcard => {}
    }
}

fn collect_pat(entries: &mut Vec<(Span, SpanNode)>, pat: &hir::Pat) {
    match &pat.kind {
        hir::PatKind::Identifier(hir_id, _) => {
            entries.push((pat.span, SpanNode::Pat(*hir_id)));
        }
        hir::PatKind::List(pats) => {
            for p in pats {
                collect_pat(entries, p);
            }
        }
        hir::PatKind::Rest(inner) => collect_pat(entries, inner),
        hir::PatKind::Enum(path, fields) => {
            collect_path(entries, path);
            for (_, p) in fields {
                collect_pat(entries, p);
            }
        }
        hir::PatKind::Literal(_) | hir::PatKind::Wildcard => {}
    }
}

fn collect_path(entries: &mut Vec<(Span, SpanNode)>, path: &hir::Path) {
    if let Some(hir_id) = path.res.hir_id() {
        entries.push((path.span, SpanNode::Path(hir_id)));
    }
}
