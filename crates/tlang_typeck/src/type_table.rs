use std::collections::HashMap;

use tlang_ast::node::Ident;
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

/// Metadata for a struct declaration, mapping field names to types.
#[derive(Debug, Clone)]
pub struct StructInfo {
    pub name: Ident,
    pub fields: Vec<(Ident, Ty)>,
}

/// Metadata for a single enum variant.
#[derive(Debug, Clone)]
pub struct VariantInfo {
    pub name: Ident,
    pub parameters: Vec<(Ident, Ty)>,
}

/// Metadata for an enum declaration, mapping variant names to their info.
#[derive(Debug, Clone)]
pub struct EnumInfo {
    pub name: Ident,
    pub variants: Vec<VariantInfo>,
}

/// A protocol method signature stored in `ProtocolInfo`.
#[derive(Debug, Clone)]
pub struct ProtocolMethodInfo {
    pub name: Ident,
    pub param_tys: Vec<Ty>,
    pub return_ty: Ty,
    pub has_default_body: bool,
}

/// Metadata for a protocol declaration, including required methods and
/// constraint protocols.
#[derive(Debug, Clone)]
pub struct ProtocolInfo {
    pub name: Ident,
    pub methods: Vec<ProtocolMethodInfo>,
    pub constraints: Vec<String>,
}

/// Records that `impl Protocol for Type` has been declared.
#[derive(Debug, Clone)]
pub struct ImplInfo {
    pub protocol_name: String,
    pub target_type_name: String,
}

/// Maps `HirId → TypeInfo` for supplementary type data (function signatures,
/// struct field types, etc.).
#[derive(Debug, Default)]
pub struct TypeTable {
    entries: HashMap<HirId, TypeInfo>,
    /// Struct declarations keyed by the struct's `HirId`.
    struct_info: HashMap<HirId, StructInfo>,
    /// Enum declarations keyed by the enum's `HirId`.
    enum_info: HashMap<HirId, EnumInfo>,
    /// Protocol declarations keyed by the protocol name.
    protocol_info: HashMap<String, ProtocolInfo>,
    /// Registered impl blocks.
    impl_info: Vec<ImplInfo>,
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

    /// Register struct declaration metadata.
    pub fn insert_struct_info(&mut self, hir_id: HirId, info: StructInfo) {
        self.struct_info.insert(hir_id, info);
    }

    /// Look up struct info by the struct declaration's `HirId`.
    pub fn get_struct_info(&self, hir_id: &HirId) -> Option<&StructInfo> {
        self.struct_info.get(hir_id)
    }

    /// Register enum declaration metadata.
    pub fn insert_enum_info(&mut self, hir_id: HirId, info: EnumInfo) {
        self.enum_info.insert(hir_id, info);
    }

    /// Look up enum info by the enum declaration's `HirId`.
    pub fn get_enum_info(&self, hir_id: &HirId) -> Option<&EnumInfo> {
        self.enum_info.get(hir_id)
    }

    /// Register protocol declaration metadata.
    pub fn insert_protocol_info(&mut self, info: ProtocolInfo) {
        self.protocol_info.insert(info.name.to_string(), info);
    }

    /// Look up protocol info by name.
    pub fn get_protocol_info(&self, name: &str) -> Option<&ProtocolInfo> {
        self.protocol_info.get(name)
    }

    /// Register an impl block.
    pub fn insert_impl_info(&mut self, info: ImplInfo) {
        self.impl_info.push(info);
    }

    /// Check whether `impl Protocol for Type` has been registered.
    pub fn has_impl(&self, protocol_name: &str, target_type_name: &str) -> bool {
        self.impl_info
            .iter()
            .any(|i| i.protocol_name == protocol_name && i.target_type_name == target_type_name)
    }
}
