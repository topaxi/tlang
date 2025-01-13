use std::rc::Rc;

use tlang_hir::hir;

use crate::value::TlangValue;

pub trait Resolver {
    fn resolve_path(&self, path: &hir::Path) -> Option<TlangValue>;
    fn resolve_fn_decl(&self, id: hir::HirId) -> Option<Rc<hir::FunctionDeclaration>>;
}
