use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

#[cfg(feature = "serde")]
use serde::Serialize;
use tlang_ast::node::{Ident, UnaryOp};
use tlang_ast::token::{Literal, Token};
use tlang_span::{HirId, HirIdAllocator, Span};
use tlang_symbols::{SymbolIdAllocator, SymbolTable, SymbolType};

pub trait HirScope {
    // fn hir_id(&self) -> HirId;

    fn locals(&self) -> usize;
    fn upvars(&self) -> usize;

    fn set_locals(&mut self, locals: usize);
    fn set_upvars(&mut self, upvars: usize);
}

#[derive(Debug, Default, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct HirScopeData {
    // How many slots to allocate for local variables.
    locals: usize,
    // How many slots to allocate for variables from parent scopes.
    upvars: usize,
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
    Param,
    Field,
    Closure,
    #[default]
    Unknown,
}

impl From<SymbolType> for BindingKind {
    fn from(symbol_type: SymbolType) -> Self {
        match symbol_type {
            SymbolType::Variable => BindingKind::Local,
            SymbolType::Struct => BindingKind::Struct,
            SymbolType::Enum => BindingKind::Enum,
            SymbolType::EnumVariant(_) => BindingKind::Variant,
            SymbolType::Function(_) | SymbolType::FunctionSelfRef(_) => BindingKind::Fn,
            SymbolType::Parameter => BindingKind::Param,
            SymbolType::Module => BindingKind::Local, // Modules act like local bindings
        }
    }
}

pub type SlotIndex = usize;
pub type ScopeIndex = u16;

#[derive(Debug, Default, Eq, PartialEq, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Slot {
    Local(SlotIndex),
    Upvar(SlotIndex, ScopeIndex),
    Builtin,
    #[default]
    None,
}

impl Slot {
    pub fn new_local(slot_index: usize) -> Self {
        Slot::Local(slot_index)
    }

    pub fn new_upvar(slot_index: usize, scope_index: usize) -> Self {
        debug_assert!(scope_index <= ScopeIndex::MAX as usize);

        Slot::Upvar(slot_index, scope_index as ScopeIndex)
    }

    pub fn is_local(self) -> bool {
        matches!(self, Slot::Local(..))
    }

    pub fn is_upvar(self) -> bool {
        matches!(self, Slot::Upvar(..))
    }

    pub fn is_builtin(self) -> bool {
        matches!(self, Slot::Builtin)
    }

    pub fn is_none(self) -> bool {
        matches!(self, Slot::None)
    }
}

impl From<(usize, usize)> for Slot {
    fn from(slot_data: (usize, usize)) -> Self {
        match slot_data {
            (slot_index, 0) => Slot::new_local(slot_index),
            (slot_index, scope_index) => Slot::new_upvar(slot_index, scope_index),
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
                | BindingKind::Param
                | BindingKind::Closure
                | BindingKind::Field
                | BindingKind::Variant // variants can be used as values
                | BindingKind::Unknown
        )
    }

    pub fn is_unknown(self) -> bool {
        matches!(self.binding_kind, BindingKind::Unknown)
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Module {
    pub hir_id: HirId,
    pub block: Block,
    pub span: Span,
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

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Block {
    pub hir_id: HirId,
    pub stmts: Vec<Stmt>,
    pub expr: Option<Expr>,
    scope: HirScopeData,
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

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Stmt {
    pub hir_id: HirId,
    pub kind: StmtKind,
    pub span: Span,
    // TODO: We might want to handle this somehow different, as we pass them on from the AST to
    //       HIR, which feels somewhat unnecessary.
    pub leading_comments: Vec<Token>,
    pub trailing_comments: Vec<Token>,
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

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum StmtKind {
    Expr(Box<Expr>),
    Let(Box<Pat>, Box<Expr>, Box<Ty>),
    FunctionDeclaration(Box<FunctionDeclaration>),
    DynFunctionDeclaration(Box<DynFunctionDeclaration>),
    Return(Option<Box<Expr>>),
    EnumDeclaration(Box<EnumDeclaration>),
    StructDeclaration(Box<StructDeclaration>),
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Pat {
    pub kind: PatKind,
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

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum PatKind {
    Wildcard,
    Identifier(HirId, Box<Ident>),
    Literal(Box<Literal>),
    List(Vec<Pat>),
    Rest(Box<Pat>),
    Enum(Box<Path>, Vec<(Ident, Pat)>),
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct MatchArm {
    pub pat: Pat,
    pub guard: Option<Expr>,
    pub block: Block,
    // TODO: We might want to handle this somehow different, as we pass them on from the AST to
    //       HIR, which feels somewhat unnecessary.
    pub leading_comments: Vec<Token>,
    pub trailing_comments: Vec<Token>,
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

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct RangeExpression {
    pub start: Expr,
    pub end: Expr,
    pub inclusive: bool,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Expr {
    pub hir_id: HirId,
    pub kind: ExprKind,
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

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum ExprKind {
    Block(Box<Block>),
    Loop(Box<Block>),
    Break(Option<Box<Expr>>),
    Continue,
    Call(Box<CallExpression>),
    TailCall(Box<CallExpression>),
    Cast(Box<Expr>, Box<Ty>),
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
    Range(Box<RangeExpression>),
    Wildcard, // TODO: This might be better to just be an identifier
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct ElseClause {
    pub condition: Option<Expr>,
    pub consequence: Block,
}

#[derive(Debug, Default, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Ty {
    pub kind: TyKind,
    pub span: Span,
}

#[derive(Debug, Default, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum TyKind {
    #[default]
    Unknown,
    Path(Path),
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct FunctionParameter {
    pub hir_id: HirId,
    pub name: Ident,
    pub type_annotation: Ty,
    pub span: Span,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct FunctionDeclaration {
    pub hir_id: HirId,
    pub name: Expr,
    pub parameters: Vec<FunctionParameter>,
    pub return_type: Ty,
    pub body: Block,
    pub span: Span,
}

impl FunctionDeclaration {
    pub fn new(hir_id: HirId, name: Expr, params: Vec<FunctionParameter>, body: Block) -> Self {
        FunctionDeclaration {
            hir_id,
            name,
            parameters: params,
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

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DynFunctionDeclaration {
    pub hir_id: HirId,
    pub name: Expr,
    pub variants: Vec<(usize, HirId)>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct StructDeclaration {
    pub hir_id: HirId,
    pub name: Ident,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct StructField {
    pub hir_id: HirId,
    pub name: Ident,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct EnumDeclaration {
    pub hir_id: HirId,
    pub name: Ident,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct EnumVariant {
    pub hir_id: HirId,
    pub name: Ident,
    pub parameters: Vec<StructField>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone, Copy)]
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
}

#[derive(Debug)]
pub struct LowerResultMeta {
    pub root_symbol_table: HirId,
    pub symbol_tables: HashMap<HirId, Rc<RefCell<SymbolTable>>>,
    pub hir_id_allocator: HirIdAllocator,
    pub symbol_id_allocator: SymbolIdAllocator,
}

pub type LowerResult = (Module, LowerResultMeta);
