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

/// Metadata for an associated type declared in a protocol.
#[derive(Debug, Clone)]
pub struct AssociatedTypeInfo {
    pub name: Ident,
    pub type_param_count: usize,
}

/// Metadata for a protocol declaration, including required methods and
/// constraint protocols.
#[derive(Debug, Clone)]
pub struct ProtocolInfo {
    pub name: Ident,
    pub methods: Vec<ProtocolMethodInfo>,
    pub constraints: Vec<String>,
    /// Associated types declared in this protocol.
    pub associated_types: Vec<AssociatedTypeInfo>,
}

/// Records that `impl Protocol for Type` has been declared.
#[derive(Debug, Clone)]
pub struct ImplInfo {
    pub protocol_name: String,
    pub target_type_name: String,
    /// Protocol type-argument keys (e.g. `["i64"]` for `impl Into<i64> for String`).
    pub protocol_type_args: Vec<String>,
    /// Whether this is a blanket impl (has impl-level type parameters).
    pub is_blanket: bool,
    /// Where clause predicates (type name → list of bound protocol names).
    pub where_predicates: Vec<(String, Vec<String>)>,
    /// Associated type bindings (name → concrete type as string).
    pub associated_type_bindings: Vec<(String, String)>,
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

    /// Return all registered impl blocks.
    pub fn impls(&self) -> &[ImplInfo] {
        &self.impl_info
    }

    /// Check whether `impl Protocol for Type` has been registered.
    pub fn has_impl(&self, protocol_name: &str, target_type_name: &str) -> bool {
        self.impl_info
            .iter()
            .any(|i| i.protocol_name == protocol_name && i.target_type_name == target_type_name)
    }

    /// Check whether `impl Protocol<TypeArg> for Type` has been registered.
    ///
    /// This is stricter than `has_impl` — it also requires that the protocol
    /// type-argument list contains the given key (e.g. `"i64"` for `Into<i64>`).
    pub fn has_impl_with_type_arg(
        &self,
        protocol_name: &str,
        target_type_name: &str,
        type_arg: &str,
    ) -> bool {
        self.impl_info.iter().any(|i| {
            i.protocol_name == protocol_name
                && i.target_type_name == target_type_name
                && i.protocol_type_args.iter().any(|a| a == type_arg)
        })
    }

    /// Resolve an associated type binding for a given protocol and target type.
    ///
    /// Returns the concrete type string if found in a concrete or blanket impl.
    pub fn resolve_associated_type(
        &self,
        protocol_name: &str,
        target_type_name: &str,
        assoc_type_name: &str,
    ) -> Option<String> {
        // Prefer concrete impls over blanket impls.
        let concrete = self.impl_info.iter().find(|i| {
            i.protocol_name == protocol_name
                && i.target_type_name == target_type_name
                && !i.is_blanket
        });
        if let Some(info) = concrete {
            return info
                .associated_type_bindings
                .iter()
                .find(|(name, _)| name == assoc_type_name)
                .map(|(_, ty)| ty.clone());
        }

        // Fall back to blanket impls that apply to the requested target type.
        self.find_blanket_impl_for(protocol_name, target_type_name)
            .and_then(|info| {
                info.associated_type_bindings
                    .iter()
                    .find(|(name, _)| name == assoc_type_name)
                    .map(|(_, ty)| ty.clone())
            })
    }

    /// Find a blanket impl for a protocol that could apply to a given target
    /// type, checking where clause predicates against registered impls.
    pub fn find_blanket_impl_for(
        &self,
        protocol_name: &str,
        target_type_name: &str,
    ) -> Option<&ImplInfo> {
        self.impl_info.iter().find(|i| {
            i.protocol_name == protocol_name
                && i.is_blanket
                && self.blanket_impl_matches(i, target_type_name)
        })
    }

    /// Check whether a blanket impl's where clause predicates are satisfied
    /// for the given target type.
    fn blanket_impl_matches(&self, impl_info: &ImplInfo, target_type_name: &str) -> bool {
        for (_type_param, bounds) in &impl_info.where_predicates {
            for bound in bounds {
                if !self.has_impl(bound, target_type_name) {
                    return false;
                }
            }
        }
        true
    }
}
