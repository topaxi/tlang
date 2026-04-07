use std::collections::HashMap;

use tlang_hir::Ty;
use tlang_span::HirId;

/// Supplementary type metadata for a given HIR node.
///
/// The `TypeTable` is a side-table that maps `HirId`s to their resolved
/// type information. It is populated by the type-checking pass and
/// consulted during code generation and diagnostics.
#[derive(Debug, Default)]
pub struct TypeInfo {
    /// The resolved type of this HIR node.
    pub ty: Ty,
}

/// Maps `HirId → TypeInfo` for supplementary type data (function signatures,
/// struct field types, etc.).
#[derive(Debug, Default)]
pub struct TypeTable {
    entries: HashMap<HirId, TypeInfo>,
}

impl TypeTable {
    pub fn new() -> Self {
        Self::default()
    }

    /// Insert type information for a given HIR node.
    pub fn insert(&mut self, hir_id: HirId, info: TypeInfo) {
        self.entries.insert(hir_id, info);
    }

    /// Look up type information for a given HIR node.
    pub fn get(&self, hir_id: &HirId) -> Option<&TypeInfo> {
        self.entries.get(hir_id)
    }

    /// Look up the resolved type of a HIR node, returning `Unknown` if absent.
    pub fn type_of(&self, hir_id: &HirId) -> &Ty {
        static UNKNOWN: std::sync::LazyLock<Ty> = std::sync::LazyLock::new(Ty::unknown);

        self.entries
            .get(hir_id)
            .map(|info| &info.ty)
            .unwrap_or(&UNKNOWN)
    }
}
