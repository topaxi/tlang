use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::sync::{Arc, RwLock};

#[cfg(feature = "serde")]
use serde::Serialize;
use tlang_ast::node::{Ident, UnaryOp, Visibility};
use tlang_ast::token::{CommentToken, Literal};
use tlang_defs::{DefIdAllocator, DefKind, DefScope};
use tlang_span::{HirId, HirIdAllocator, Span, TypeVarId};

pub trait HirScope {
    // fn hir_id(&self) -> HirId;

    fn locals(&self) -> usize;
    fn upvars(&self) -> usize;

    fn set_locals(&mut self, locals: usize);
    fn set_upvars(&mut self, upvars: usize);
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct HirScopeData {
    // How many slots to allocate for local variables.
    locals: usize,
    // How many slots to allocate for variables from parent scopes.
    upvars: usize,
    // Explicit capture list populated by FreeVariableAnalysis.
    // Each entry maps to a free variable the closure needs from a parent scope.
    captures: Vec<CaptureInfo>,
}

impl HirScopeData {
    pub fn captures(&self) -> &[CaptureInfo] {
        &self.captures
    }

    pub fn set_captures(&mut self, captures: Vec<CaptureInfo>) {
        self.captures = captures;
    }
}

impl HirScope for HirScopeData {
    fn locals(&self) -> usize {
        self.locals
    }

    fn upvars(&self) -> usize {
        self.upvars
    }

    fn set_locals(&mut self, locals: usize) {
        self.locals = locals;
    }

    fn set_upvars(&mut self, upvars: usize) {
        self.upvars = upvars;
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
pub struct BindingIdTag;

pub type BindingId = tlang_span::id::Id<BindingIdTag>;
pub type BindingIdAllocator = tlang_span::id::IdAllocator<BindingIdTag>;

#[derive(Debug, Default, Eq, PartialEq, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum BindingKind {
    Local,
    Upvar,
    Temp,
    Struct,
    Enum,
    Variant,
    Fn,
    /// A `::` alias that refers to a dot-method on a struct/enum prototype.
    StructMethod,
    Param,
    Field,
    Closure,
    /// A builtin/primitive type (e.g. `i64`, `bool`, `String`) — has no HIR
    /// declaration node.
    PrimTy,
    #[default]
    Unknown,
}

impl From<DefKind> for BindingKind {
    fn from(kind: DefKind) -> Self {
        match kind {
            DefKind::Variable => BindingKind::Local,
            DefKind::Const => BindingKind::Local,
            DefKind::Struct | DefKind::StructField => BindingKind::Struct,
            DefKind::Enum => BindingKind::Enum,
            DefKind::EnumVariant(_) => BindingKind::Variant,
            DefKind::Function(_) | DefKind::FunctionSelfRef(_) => BindingKind::Fn,
            DefKind::StructMethod(_) => BindingKind::StructMethod,
            DefKind::Parameter => BindingKind::Param,
            DefKind::Module => todo!(),
            DefKind::Protocol | DefKind::ProtocolMethod(_) => BindingKind::Enum,
        }
    }
}

pub type SlotIndex = usize;
pub type ScopeIndex = u16;

/// Describes a single captured free variable for a closure.
///
/// When a closure references a variable from a parent scope, the
/// `FreeVariableAnalysis` HIR pass records that reference here so the
/// runtime can capture only the needed bindings instead of the entire
/// scope stack.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct CaptureInfo {
    /// Slot index of the variable in the source scope.
    pub slot_index: SlotIndex,
    /// Relative scope distance from the closure to the source scope
    /// (same semantics as the `ScopeIndex` in `Slot::Upvar`).
    pub scope_index: ScopeIndex,
}

#[derive(Debug, Default, Eq, PartialEq, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Slot {
    /// Variable in the current (innermost) scope.
    Local(SlotIndex),
    /// Variable in a parent scope within the **same** function.
    ///
    /// At runtime this behaves identically to `Upvar` (the scope-stack
    /// distance is used to locate the value), but it is semantically
    /// distinct: no closure capture is involved.
    BlockVar(SlotIndex, ScopeIndex),
    /// Variable from an **enclosing function** — a true closure capture.
    ///
    /// `scope_index` is the scope-stack distance from the reference point
    /// to the defining scope, crossing at least one function boundary.
    Upvar(SlotIndex, ScopeIndex),
    Global(SlotIndex),
    #[default]
    None,
}

impl Slot {
    pub fn new_local(slot_index: usize) -> Self {
        Slot::Local(slot_index)
    }

    pub fn new_blockvar(slot_index: usize, scope_index: usize) -> Self {
        debug_assert!(scope_index <= ScopeIndex::MAX as usize);

        Slot::BlockVar(slot_index, scope_index as ScopeIndex)
    }

    pub fn new_upvar(slot_index: usize, scope_index: usize) -> Self {
        debug_assert!(scope_index <= ScopeIndex::MAX as usize);

        Slot::Upvar(slot_index, scope_index as ScopeIndex)
    }

    pub fn is_local(self) -> bool {
        matches!(self, Slot::Local(..))
    }

    pub fn is_blockvar(self) -> bool {
        matches!(self, Slot::BlockVar(..))
    }

    pub fn is_upvar(self) -> bool {
        matches!(self, Slot::Upvar(..))
    }

    pub fn is_global(self) -> bool {
        matches!(self, Slot::Global(..))
    }

    pub fn is_none(self) -> bool {
        matches!(self, Slot::None)
    }
}

/// Construct a `Slot` from a `(slot_index, scope_distance, crosses_function)` triple
/// returned by [`DefScope::get_slot()`].
///
/// The third element (`crosses_function: bool`) indicates whether the lookup
/// had to walk past at least one function-boundary scope
/// ([`DefScope::is_function_scope`]) to reach the defining scope.  This is the
/// key semantic distinction:
///
/// - `(idx, 0, _)` → `Local(idx)` — variable in the current scope.
/// - `(idx, n, false)` → `BlockVar(idx, n)` — variable in a parent block
///   within the **same** function; not a closure capture.
/// - `(idx, n, true)` → `Upvar(idx, n)` — variable from an **enclosing
///   function**; a true closure capture.
impl From<(usize, usize, bool)> for Slot {
    fn from(slot_data: (usize, usize, bool)) -> Self {
        match slot_data {
            (slot_index, 0, _) => Slot::new_local(slot_index),
            (slot_index, scope_index, false) => Slot::new_blockvar(slot_index, scope_index),
            (slot_index, scope_index, true) => Slot::new_upvar(slot_index, scope_index),
        }
    }
}

#[derive(Debug, Default, Eq, PartialEq, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Res {
    hir_id: Option<HirId>,
    binding_kind: BindingKind,
    slot: Slot,
}

impl Res {
    fn new(hir_id: HirId, binding_kind: BindingKind) -> Self {
        Res {
            hir_id: Some(hir_id),
            binding_kind,
            slot: Slot::None,
        }
    }

    pub fn new_prim_ty() -> Self {
        Res {
            hir_id: None,
            binding_kind: BindingKind::PrimTy,
            slot: Slot::None,
        }
    }

    pub fn new_local(hir_id: HirId) -> Self {
        Res::new(hir_id, BindingKind::Local)
    }

    pub fn new_fn(hir_id: HirId) -> Self {
        Res::new(hir_id, BindingKind::Fn)
    }

    pub fn new_enum(hir_id: HirId) -> Self {
        Res::new(hir_id, BindingKind::Enum)
    }

    pub fn new_enum_variant(hir_id: HirId) -> Self {
        Res::new(hir_id, BindingKind::Variant)
    }

    /// Creates a `Res` for a protocol method reference synthesised during HIR
    /// lowering.  Protocol methods use `BindingKind::Enum` (no slot/hir_id)
    /// so they are not picked up as normal value bindings by the resolver.
    pub fn new_protocol_method() -> Self {
        Res {
            hir_id: None,
            binding_kind: BindingKind::Enum,
            slot: Slot::None,
        }
    }

    pub fn new_builtin_fn(slot_index: usize) -> Self {
        Res {
            hir_id: None,
            binding_kind: BindingKind::Fn,
            slot: Slot::Global(slot_index),
        }
    }

    pub fn new_builtin_variant(slot_index: usize) -> Self {
        Res {
            hir_id: None,
            binding_kind: BindingKind::Variant,
            slot: Slot::Global(slot_index),
        }
    }

    pub fn new_upvar(hir_id: HirId, slot_index: usize, scope_index: usize) -> Self {
        debug_assert!(scope_index <= ScopeIndex::MAX as usize);

        Res {
            hir_id: Some(hir_id),
            binding_kind: BindingKind::Upvar,
            slot: Slot::Upvar(slot_index, scope_index as ScopeIndex),
        }
    }

    pub fn is_value(self) -> bool {
        matches!(
            self.binding_kind,
            BindingKind::Local
                | BindingKind::Upvar
                | BindingKind::Temp
                | BindingKind::Fn
                | BindingKind::StructMethod
                | BindingKind::Param
                | BindingKind::Closure
                | BindingKind::Field
                | BindingKind::Variant // variants can be used as values
                | BindingKind::Unknown
        )
    }

    pub fn is_prim_ty(self) -> bool {
        matches!(self.binding_kind, BindingKind::PrimTy)
    }

    pub fn is_unknown(self) -> bool {
        matches!(self.binding_kind, BindingKind::Unknown)
    }

    /// Returns true if the resolution is incomplete — the binding kind is
    /// still `Unknown` meaning no resolver pass has identified this path yet.
    /// A non-Unknown binding kind means the symbol was found (builtins may
    /// have neither `hir_id` nor a slot in slot-less compilation contexts,
    /// but they are still considered resolved).
    pub fn is_unresolved(self) -> bool {
        if self.is_unknown() {
            return true;
        }
        // Local/Temp/Param bindings that were synthesized (e.g. by loop
        // lowering) start with the correct binding kind but without an hir_id;
        // that's the only remaining "truly unresolved" case we need to flag.
        let needs_hir_id = matches!(
            self.binding_kind,
            BindingKind::Local | BindingKind::Temp | BindingKind::Param
        );
        needs_hir_id && self.hir_id.is_none() && self.slot.is_none()
    }

    pub fn is_def(self) -> bool {
        matches!(
            self.binding_kind,
            BindingKind::Struct | BindingKind::Enum | BindingKind::Fn
        )
    }

    pub fn is_struct_def(self) -> bool {
        matches!(self.binding_kind, BindingKind::Struct)
    }

    pub fn is_enum_def(self) -> bool {
        matches!(self.binding_kind, BindingKind::Enum)
    }

    pub fn is_enum_variant_def(self) -> bool {
        matches!(self.binding_kind, BindingKind::Variant)
    }

    pub fn hir_id(self) -> Option<HirId> {
        self.hir_id
    }

    pub fn set_hir_id(&mut self, hir_id: HirId) {
        self.hir_id = Some(hir_id);
    }

    pub fn binding_kind(self) -> BindingKind {
        self.binding_kind
    }

    pub fn set_binding_kind(&mut self, binding_kind: BindingKind) {
        self.binding_kind = binding_kind;
    }

    pub fn slot(self) -> Slot {
        self.slot
    }

    pub fn set_slot(&mut self, slot: Slot) {
        self.slot = slot;
    }
}

#[derive(Debug, Clone, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Path {
    pub segments: Vec<PathSegment>,
    pub res: Res,
    pub span: Span,
}

impl PartialEq for Path {
    fn eq(&self, other: &Self) -> bool {
        self.segments == other.segments
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.join("::"))
    }
}

impl Path {
    pub fn new(segments: Vec<PathSegment>, span: Span) -> Self {
        Self {
            segments,
            res: Res::default(),
            span,
        }
    }

    pub fn set_res(&mut self, res: Res) {
        self.res = res;
    }

    pub fn with_res(mut self, res: Res) -> Self {
        self.set_res(res);
        self
    }

    pub fn join(&self, separator: &str) -> String {
        self.segments
            .iter()
            .map(|segment| segment.ident.as_str())
            .collect::<Vec<_>>()
            .join(separator)
    }

    pub fn join_with(&self, segment_str: &str) -> String {
        self.to_string() + "::" + segment_str
    }

    pub fn first_ident(&self) -> &Ident {
        &self.segments[0].ident
    }

    pub fn last_ident(&self) -> &Ident {
        &self.segments[self.segments.len() - 1].ident
    }

    pub fn as_init(&self) -> Self {
        Path {
            segments: if self.segments.len() > 1 {
                self.segments[0..self.segments.len() - 1].to_vec()
            } else {
                vec![self.segments[0].clone()]
            },
            res: Default::default(),
            span: self.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct PathSegment {
    pub ident: Ident,
}

impl PathSegment {
    pub fn new(ident: Ident) -> Self {
        PathSegment { ident }
    }

    pub fn from_str(name: &str, span: Span) -> Self {
        PathSegment {
            ident: Ident::new(name, span),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Module {
    pub hir_id: HirId,
    pub block: Block,
    pub span: Span,
}

impl Default for Module {
    fn default() -> Self {
        Self {
            hir_id: HirId::new(1),
            block: Block {
                hir_id: HirId::new(2),
                stmts: vec![],
                expr: None,
                scope: Default::default(),
                span: Span::default(),
            },
            span: Span::default(),
        }
    }
}

impl HirScope for Module {
    fn locals(&self) -> usize {
        self.block.scope.locals()
    }

    fn upvars(&self) -> usize {
        self.block.scope.upvars()
    }

    fn set_locals(&mut self, locals: usize) {
        self.block.scope.set_locals(locals);
    }

    fn set_upvars(&mut self, upvars: usize) {
        self.block.scope.set_upvars(upvars);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Block {
    pub hir_id: HirId,
    pub stmts: Vec<Stmt>,
    pub expr: Option<Expr>,
    pub scope: HirScopeData,
    pub span: Span,
}

impl Block {
    pub fn new(hir_id: HirId, stmts: Vec<Stmt>, expr: Option<Expr>, span: Span) -> Self {
        Block {
            hir_id,
            stmts,
            expr,
            scope: Default::default(),
            span,
        }
    }

    pub fn has_completion(&self) -> bool {
        self.expr.is_some()
    }
}

impl HirScope for Block {
    fn locals(&self) -> usize {
        self.scope.locals()
    }

    fn upvars(&self) -> usize {
        self.scope.upvars()
    }

    fn set_locals(&mut self, locals: usize) {
        self.scope.set_locals(locals);
    }

    fn set_upvars(&mut self, upvars: usize) {
        self.scope.set_upvars(upvars);
    }
}

impl HirScope for &mut Block {
    fn locals(&self) -> usize {
        self.scope.locals()
    }

    fn upvars(&self) -> usize {
        self.scope.upvars()
    }

    fn set_locals(&mut self, locals: usize) {
        self.scope.set_locals(locals);
    }

    fn set_upvars(&mut self, upvars: usize) {
        self.scope.set_upvars(upvars);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Stmt {
    pub hir_id: HirId,
    pub kind: StmtKind,
    pub span: Span,
    // TODO: We might want to handle this somehow different, as we pass them on from the AST to
    //       HIR, which feels somewhat unnecessary.
    pub leading_comments: Vec<CommentToken>,
    pub trailing_comments: Vec<CommentToken>,
}

impl Stmt {
    pub fn new(hir_id: HirId, kind: StmtKind, span: Span) -> Self {
        Stmt {
            hir_id,
            kind,
            span,
            leading_comments: vec![],
            trailing_comments: vec![],
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum StmtKind {
    Expr(Box<Expr>),
    Let(Box<Pat>, Box<Expr>, Box<Ty>),
    Const(Visibility, Box<Pat>, Box<Expr>, Box<Ty>),
    FunctionDeclaration(Box<FunctionDeclaration>),
    DynFunctionDeclaration(Box<DynFunctionDeclaration>),
    Return(Option<Box<Expr>>),
    EnumDeclaration(Box<EnumDeclaration>),
    StructDeclaration(Box<StructDeclaration>),
    ProtocolDeclaration(Box<ProtocolDeclaration>),
    ImplBlock(Box<ImplBlock>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Pat {
    pub kind: PatKind,
    pub ty: Ty,
    pub span: Span,
}

impl Pat {
    pub fn is_wildcard(&self) -> bool {
        matches!(self.kind, PatKind::Wildcard)
    }

    pub fn is_rest(&self) -> bool {
        matches!(self.kind, PatKind::Rest(_))
    }

    pub fn is_ident(&self) -> bool {
        matches!(self.kind, PatKind::Identifier(_, _))
    }

    pub fn is_empty_list(&self) -> bool {
        match &self.kind {
            PatKind::List(pats) => pats.is_empty(),
            _ => false,
        }
    }

    pub fn ident(&self) -> Option<&Ident> {
        match &self.kind {
            PatKind::Identifier(_, ident) => Some(ident),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum PatKind {
    Wildcard,
    Identifier(HirId, Box<Ident>),
    Literal(Box<Literal>),
    List(Vec<Pat>),
    Rest(Box<Pat>),
    Enum(Box<Path>, Vec<(Ident, Pat)>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct MatchArm {
    /// HirId for the arm's own scope, which holds pattern-bound variables.
    /// Invariant: equals `block.hir_id` for inline-expression arms (single scope);
    /// differs from `block.hir_id` for block-body arms (two scopes: arm scope + block scope).
    pub hir_id: HirId,
    pub pat: Pat,
    pub guard: Option<Expr>,
    pub block: Block,
    /// Number of locals in the arm's pattern scope (populated by `ScopeDataUpdater`).
    /// Non-zero only when the arm has pattern-bound variables. Used by the runtime to
    /// pre-allocate the outer scope when `hir_id != block.hir_id`.
    pub pat_locals: usize,
    // TODO: We might want to handle this somehow different, as we pass them on from the AST to
    //       HIR, which feels somewhat unnecessary.
    pub leading_comments: Vec<CommentToken>,
    pub trailing_comments: Vec<CommentToken>,
}

impl MatchArm {
    pub fn has_let_guard(&self) -> bool {
        self.guard.as_ref().is_some_and(Expr::is_let)
    }
}

impl HirScope for MatchArm {
    fn locals(&self) -> usize {
        self.block.locals()
    }

    fn upvars(&self) -> usize {
        self.block.upvars()
    }

    fn set_locals(&mut self, locals: usize) {
        self.block.set_locals(locals);
    }

    fn set_upvars(&mut self, upvars: usize) {
        self.block.set_upvars(upvars);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct RangeExpression {
    pub start: Expr,
    pub end: Expr,
    pub inclusive: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct CallExpression {
    pub hir_id: HirId,
    pub callee: Expr,
    pub arguments: Vec<Expr>,
}

impl CallExpression {
    pub fn has_wildcard(&self) -> bool {
        self.arguments.iter().any(Expr::is_wildcard)
    }

    pub fn wildcard_count(&self) -> usize {
        self.arguments
            .iter()
            .filter(|arg| arg.is_wildcard())
            .count()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Expr {
    pub hir_id: HirId,
    pub kind: ExprKind,
    pub ty: Ty,
    pub span: Span,
}

impl Expr {
    pub fn is_wildcard(&self) -> bool {
        matches!(self.kind, ExprKind::Wildcard)
    }

    pub fn is_path(&self) -> bool {
        matches!(self.kind, ExprKind::Path(_))
    }

    pub fn is_tail_call(&self) -> bool {
        matches!(self.kind, ExprKind::TailCall(_))
    }

    pub fn is_let(&self) -> bool {
        matches!(self.kind, ExprKind::Let(..))
    }

    pub fn path(&self) -> Option<&Path> {
        match &self.kind {
            ExprKind::Path(path) => Some(path),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum ExprKind {
    Block(Box<Block>),
    Loop(Box<Block>),
    Break(Option<Box<Expr>>),
    Continue,
    Call(Box<CallExpression>),
    TailCall(Box<CallExpression>),
    Cast(Box<Expr>, Box<Ty>),
    TryCast(Box<Expr>, Box<Ty>),
    Binary(BinaryOpKind, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    // Let expression, only valid within if conditions and guards
    Let(Box<Pat>, Box<Expr>),
    IfElse(Box<Expr>, Box<Block>, Vec<ElseClause>),
    Path(Box<Path>),
    List(Vec<Expr>),
    Dict(Vec<(Expr, Expr)>),
    FunctionExpression(Box<FunctionDeclaration>),
    FieldAccess(Box<Expr>, Ident),
    IndexAccess(Box<Expr>, Box<Expr>),
    Literal(Box<Literal>),
    Match(Box<Expr>, Vec<MatchArm>),
    Implements(Box<Expr>, Path),
    Range(Box<RangeExpression>),
    TaggedString {
        tag: Box<Expr>,
        parts: Vec<Box<str>>,
        exprs: Vec<Expr>,
    },
    Wildcard, // TODO: This might be better to just be an identifier
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct ElseClause {
    pub condition: Option<Expr>,
    pub consequence: Block,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Ty {
    /// Points to the declaration's `HirId`, preserving resolution identity
    /// across lowering. Named types resolved via `node_id_to_hir_id` will have
    /// `Some`; primitive and unknown types remain `None`.
    pub res: Option<HirId>,
    pub kind: TyKind,
    pub span: Span,
}

impl Ty {
    pub fn unknown() -> Self {
        Self::default()
    }
}

/// Primitive type kinds shared across HIR and type-checking phases.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum PrimTy {
    Bool,
    I8,
    I16,
    I32,
    I64,
    Isize,
    U8,
    U16,
    U32,
    U64,
    Usize,
    F32,
    F64,
    Char,
    String,
    Nil,
}

impl PrimTy {
    /// Returns `true` for any numeric type (signed/unsigned integers and floats).
    pub fn is_numeric(self) -> bool {
        self.is_integer() || self.is_float()
    }

    /// Returns `true` for integer types (signed, unsigned, and pointer-sized).
    pub fn is_integer(self) -> bool {
        self.is_signed_integer() || self.is_unsigned_integer()
    }

    /// Returns `true` for signed integer types (`i8`..`i64`, `isize`).
    pub fn is_signed_integer(self) -> bool {
        matches!(
            self,
            PrimTy::I8 | PrimTy::I16 | PrimTy::I32 | PrimTy::I64 | PrimTy::Isize
        )
    }

    /// Returns `true` for unsigned integer types (`u8`..`u64`, `usize`).
    pub fn is_unsigned_integer(self) -> bool {
        matches!(
            self,
            PrimTy::U8 | PrimTy::U16 | PrimTy::U32 | PrimTy::U64 | PrimTy::Usize
        )
    }

    /// Returns `true` for floating-point types.
    pub fn is_float(self) -> bool {
        matches!(self, PrimTy::F32 | PrimTy::F64)
    }
}

impl Display for PrimTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrimTy::Bool => write!(f, "bool"),
            PrimTy::I8 => write!(f, "i8"),
            PrimTy::I16 => write!(f, "i16"),
            PrimTy::I32 => write!(f, "i32"),
            PrimTy::I64 => write!(f, "i64"),
            PrimTy::Isize => write!(f, "isize"),
            PrimTy::U8 => write!(f, "u8"),
            PrimTy::U16 => write!(f, "u16"),
            PrimTy::U32 => write!(f, "u32"),
            PrimTy::U64 => write!(f, "u64"),
            PrimTy::Usize => write!(f, "usize"),
            PrimTy::F32 => write!(f, "f32"),
            PrimTy::F64 => write!(f, "f64"),
            PrimTy::Char => write!(f, "char"),
            PrimTy::String => write!(f, "String"),
            PrimTy::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum TyKind {
    #[default]
    Unknown,
    /// Primitive types (Bool, I8..I64, U8..U64, F32, F64, Char, String, Nil).
    Primitive(PrimTy),
    /// Function type: parameter types → return type.
    Fn(Vec<Ty>, Box<Ty>),
    /// Homogeneous list/slice type.
    Slice(Box<Ty>),
    /// Dictionary type: key type → value type.
    Dict(Box<Ty>, Box<Ty>),
    /// Bottom type for diverging expressions.
    Never,
    /// Placeholder for future generic type variables (Phase 8: Generics).
    Var(TypeVarId),
    /// User-defined types (structs, enums).
    Path(Path),
    /// Union of multiple types.
    Union(Vec<Ty>),
}

impl Display for TyKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TyKind::Unknown => write!(f, "unknown"),
            TyKind::Primitive(p) => write!(f, "{p}"),
            TyKind::Fn(params, ret) => {
                write!(f, "fn(")?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", p.kind)?;
                }
                write!(f, ") -> {}", ret.kind)
            }
            TyKind::Slice(inner) => write!(f, "{}[]", inner.kind),
            TyKind::Dict(k, v) => write!(f, "dict[{}, {}]", k.kind, v.kind),
            TyKind::Never => write!(f, "never"),
            TyKind::Var(id) => write!(f, "?{id}"),
            TyKind::Path(path) => write!(f, "{path}"),
            TyKind::Union(tys) => {
                for (i, ty) in tys.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{}", ty.kind)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct FunctionParameter {
    pub hir_id: HirId,
    pub name: Ident,
    pub type_annotation: Ty,
    pub span: Span,
}

/// A type parameter in a generic declaration, e.g. `T` in `fn map<T>(...)`.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct TypeParam {
    pub hir_id: HirId,
    pub name: Ident,
    /// The `TypeVarId` used to represent this type parameter in `TyKind::Var`.
    pub type_var_id: TypeVarId,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct FunctionDeclaration {
    pub hir_id: HirId,
    pub visibility: Visibility,
    pub name: Expr,
    /// Type parameters for generic functions, e.g. `<T, U>` in `fn map<T, U>(...)`.
    pub type_params: Vec<TypeParam>,
    pub parameters: Vec<FunctionParameter>,
    /// Span of the `(…)` parameter list, from `(` through `)` inclusive.
    /// Used to place return-type inlay hints right after `)`.
    pub params_span: Span,
    pub return_type: Ty,
    pub body: Block,
    pub span: Span,
}

impl FunctionDeclaration {
    pub fn new(hir_id: HirId, name: Expr, params: Vec<FunctionParameter>, body: Block) -> Self {
        FunctionDeclaration {
            hir_id,
            visibility: Visibility::Private,
            name,
            type_params: Vec::new(),
            parameters: params,
            params_span: Span::default(),
            return_type: Ty::default(),
            body,
            span: Span::default(),
        }
    }

    /// # Panics
    pub fn name(&self) -> String {
        match &self.name.kind {
            ExprKind::Path(path) => path.to_string(),
            ExprKind::FieldAccess(expr, ident) => {
                format!("{}.{}", expr.path().unwrap(), ident)
            }
            _ => unreachable!(),
        }
    }
}

impl HirScope for FunctionDeclaration {
    fn locals(&self) -> usize {
        self.body.locals()
    }

    fn upvars(&self) -> usize {
        self.body.upvars()
    }

    fn set_locals(&mut self, locals: usize) {
        self.body.set_locals(locals);
    }

    fn set_upvars(&mut self, upvars: usize) {
        self.body.set_upvars(upvars);
    }
}

impl HirScope for std::rc::Rc<FunctionDeclaration> {
    fn locals(&self) -> usize {
        self.body.locals()
    }

    fn upvars(&self) -> usize {
        self.body.upvars()
    }

    fn set_locals(&mut self, _locals: usize) {
        unreachable!()
    }

    fn set_upvars(&mut self, _upvars: usize) {
        unreachable!()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DynFunctionDeclaration {
    pub hir_id: HirId,
    pub name: Expr,
    pub variants: Vec<(usize, HirId)>,
}

/// A constant item defined inside a struct, enum, or protocol body.
/// These are accessible via path resolution (e.g., `StructName::CONST_NAME`).
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct ConstItem {
    pub hir_id: HirId,
    pub visibility: Visibility,
    pub name: Ident,
    pub value: Expr,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct StructDeclaration {
    pub hir_id: HirId,
    pub visibility: Visibility,
    pub name: Ident,
    /// Type parameters for generic structs, e.g. `<T>` in `struct Pair<T>`.
    pub type_params: Vec<TypeParam>,
    pub fields: Vec<StructField>,
    pub consts: Vec<ConstItem>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct StructField {
    pub hir_id: HirId,
    pub name: Ident,
    pub ty: Ty,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct EnumDeclaration {
    pub hir_id: HirId,
    pub visibility: Visibility,
    pub name: Ident,
    /// Type parameters for generic enums, e.g. `<T, E>` in `enum Result<T, E>`.
    pub type_params: Vec<TypeParam>,
    pub variants: Vec<EnumVariant>,
    pub consts: Vec<ConstItem>,
}

impl EnumDeclaration {
    /// Returns `true` if all variants are simple (no parameters) and every
    /// variant has an explicit discriminant value.  Such enums are represented
    /// as plain const objects in the JS backend and their variants are stored
    /// as plain values (not enum objects) in the interpreter.
    pub fn is_discriminant_enum(&self) -> bool {
        !self.variants.is_empty()
            && self
                .variants
                .iter()
                .all(|v| v.parameters.is_empty() && v.discriminant.is_some())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct EnumVariant {
    pub hir_id: HirId,
    pub name: Ident,
    pub parameters: Vec<StructField>,
    pub discriminant: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct ProtocolDeclaration {
    pub hir_id: HirId,
    pub visibility: Visibility,
    pub name: Ident,
    /// Type parameters for generic protocols, e.g. `<T>` in `protocol Into<T>`.
    pub type_params: Vec<TypeParam>,
    pub constraints: Vec<Path>,
    pub methods: Vec<ProtocolMethodSignature>,
    pub consts: Vec<ConstItem>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct ProtocolMethodSignature {
    pub hir_id: HirId,
    pub name: Ident,
    pub parameters: Vec<FunctionParameter>,
    pub return_type: Ty,
    pub body: Option<Block>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct ImplBlock {
    pub hir_id: HirId,
    pub protocol_name: Path,
    pub target_type: Path,
    pub methods: Vec<FunctionDeclaration>,
    pub apply_methods: Vec<Ident>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum BinaryOpKind {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Exp,
    Eq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    And,
    Or,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,
}

impl Display for BinaryOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOpKind::Assign => write!(f, "="),
            BinaryOpKind::Add => write!(f, "+"),
            BinaryOpKind::Sub => write!(f, "-"),
            BinaryOpKind::Mul => write!(f, "*"),
            BinaryOpKind::Div => write!(f, "/"),
            BinaryOpKind::Mod => write!(f, "%"),
            BinaryOpKind::Exp => write!(f, "**"),
            BinaryOpKind::Eq => write!(f, "=="),
            BinaryOpKind::NotEq => write!(f, "!="),
            BinaryOpKind::Less => write!(f, "<"),
            BinaryOpKind::LessEq => write!(f, "<="),
            BinaryOpKind::Greater => write!(f, ">"),
            BinaryOpKind::GreaterEq => write!(f, ">="),
            BinaryOpKind::And => write!(f, "&&"),
            BinaryOpKind::Or => write!(f, "||"),
            BinaryOpKind::BitwiseAnd => write!(f, "&"),
            BinaryOpKind::BitwiseOr => write!(f, "|"),
            BinaryOpKind::BitwiseXor => write!(f, "^"),
            BinaryOpKind::LeftShift => write!(f, "<<"),
            BinaryOpKind::RightShift => write!(f, ">>"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LowerResultMeta {
    pub root_symbol_table: HirId,
    pub symbol_tables: HashMap<HirId, Arc<RwLock<DefScope>>>,
    pub hir_id_allocator: HirIdAllocator,
    pub symbol_id_allocator: DefIdAllocator,
    /// HirIds of expressions that produce compile-time-constant values
    /// (e.g. tagged string parts lists). Used by the interpreter to cache
    /// these values for singleton semantics.
    pub constant_pool_ids: HashSet<HirId>,
    /// HirIds of `CallExpression` nodes that were produced by the `|>`
    /// pipeline operator.  Stored here so that analysis passes (e.g. inlay
    /// hints) can identify pipeline-desugared calls without changing the HIR
    /// structure.
    pub pipeline_call_ids: HashSet<HirId>,
}

pub type LowerResult = (Module, LowerResultMeta);
