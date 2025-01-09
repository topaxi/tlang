use std::rc::Rc;

use tlang_hir::hir;

use crate::value::TlangValue;

pub trait Resolver {
    fn resolve_path(&self, path: &hir::Path) -> Option<TlangValue>;
    fn resolve_fn(&self, path: &hir::Path) -> Option<Rc<hir::FunctionDeclaration>>;
    fn resolve_struct(&self, path: &hir::Path) -> Option<Rc<hir::StructDeclaration>>;
}
