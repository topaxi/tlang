use tlang_hir as hir;

use crate::value::TlangValue;

pub trait Resolver {
    fn resolve_value(&self, path: &hir::Path) -> Option<TlangValue>;
}
