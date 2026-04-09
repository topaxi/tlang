use tlang_ast::node::Ident;
use tlang_hir::{self as hir, Res, TyKind};
use tlang_span::Span;

/// Builtin collection type names recognised by the type checker.
pub const LIST: &str = "List";
pub const DICT: &str = "Dict";

/// Returns `true` if `name` is a builtin collection type name.
pub fn is_builtin_type(name: &str) -> bool {
    matches!(name, LIST | DICT)
}

/// Construct the canonical `TyKind` for a builtin collection type name.
///
/// The returned path carries `Res::PrimTy` (no HirId), consistent with how
/// the HIR optimizer resolves these names via `PRIM_TY_NAMES`.
///
/// Returns `None` if `name` is not a known builtin collection type.
pub fn lookup(name: &str) -> Option<TyKind> {
    if !is_builtin_type(name) {
        return None;
    }
    let ident = Ident::new(name, Span::default());
    let segment = hir::PathSegment::new(ident);
    let mut path = hir::Path::new(vec![segment], Span::default());
    path.set_res(Res::new_prim_ty());
    Some(TyKind::Path(path))
}
