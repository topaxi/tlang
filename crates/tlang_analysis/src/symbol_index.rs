//! Lightweight symbol index extracted from the semantic analyzer.
//!
//! Instead of caching the full [`SemanticAnalyzer`], this module extracts only
//! the symbol entries and parent links needed for hover/goto lookups.  Both the
//! LSP server and the WASM playground bindings share this implementation to
//! ensure consistent symbol resolution behaviour.

use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use tlang_defs::{DefKind, DefScope};
use tlang_semantics::SemanticAnalyzer;
use tlang_span::{HirId, NodeId, Span};

use crate::typed_hir::TypedHir;

/// A single symbol entry extracted from the semantic analyzer.
#[derive(Debug, Clone)]
pub struct SymbolEntry {
    pub name: Box<str>,
    pub kind: DefKind,
    pub defined_at: Span,
    pub scope_start: u32,
    pub node_id: Option<NodeId>,
    pub hir_id: Option<HirId>,
    pub type_info: Option<String>,
    pub builtin: bool,
    /// Whether this is a compiler-generated temporary.
    pub temp: bool,
}

/// Lightweight symbol index extracted from the semantic analyzer.
///
/// Instead of caching the full [`SemanticAnalyzer`], we extract only the
/// symbol entries and parent links we need for hover/goto lookups.
#[derive(Debug, Default)]
pub struct SymbolIndex {
    /// Maps each scope [`NodeId`] to the symbols declared directly in that
    /// scope (not inherited from parent scopes).
    scopes: HashMap<NodeId, Vec<SymbolEntry>>,
    /// Maps a child scope [`NodeId`] to its parent scope [`NodeId`], enabling
    /// upward scope-chain walks during symbol lookup.
    parents: HashMap<NodeId, NodeId>,
}

impl SymbolIndex {
    fn choose_closest(entries: &[&SymbolEntry], span: Span) -> Option<SymbolEntry> {
        if entries.is_empty() {
            return None;
        }

        if let Some(definition) = entries.iter().find(|e| e.defined_at == span) {
            return Some((**definition).clone());
        }

        let closest = entries
            .iter()
            .rev()
            .find(|e| e.scope_start < span.start_lc.line)
            .cloned();
        if let Some(entry) = closest {
            return Some(entry.clone());
        }

        entries
            .iter()
            .rev()
            .find(|e| {
                e.defined_at.start_lc < span.start_lc || e.kind.arity().is_some() || e.builtin
            })
            .cloned()
            .cloned()
    }

    /// Build a `SymbolIndex` from the semantic analyzer's symbol tables.
    ///
    /// # Panics
    ///
    /// Panics if any scope `RwLock` is poisoned.
    pub fn from_analyzer(analyzer: &SemanticAnalyzer) -> Self {
        let mut index = SymbolIndex::default();
        let tables = analyzer.symbol_tables();

        // Pre-compute a reverse map from Arc pointer identity to NodeId so that
        // parent lookups are O(1) instead of a linear scan per scope.
        let ptr_to_node: HashMap<*const std::sync::RwLock<DefScope>, NodeId> = tables
            .iter()
            .map(|(&node_id, arc)| (Arc::as_ptr(arc), node_id))
            .collect();

        for (&node_id, scope_rc) in tables {
            let scope = scope_rc.read().unwrap();
            let entries: Vec<SymbolEntry> = scope
                .get_all_local_symbols()
                .iter()
                .filter(|d| d.declared)
                .map(|d| SymbolEntry {
                    name: d.name.clone(),
                    kind: d.kind,
                    defined_at: d.defined_at,
                    scope_start: d.scope_start,
                    node_id: d.node_id,
                    hir_id: d.hir_id,
                    type_info: None,
                    builtin: d.builtin,
                    temp: d.temp,
                })
                .collect();
            index.scopes.insert(node_id, entries);

            // Record parent link via the precomputed pointer→NodeId map.
            if let Some(parent_arc) = scope.parent() {
                let parent_ptr = Arc::as_ptr(&parent_arc);
                if let Some(&parent_id) = ptr_to_node.get(&parent_ptr) {
                    index.parents.insert(node_id, parent_id);
                }
            }
        }

        index
    }

    /// Populate inferred type information for entries from typed HIR.
    pub fn populate_type_info(&mut self, typed_hir: &TypedHir) {
        for entries in self.scopes.values_mut() {
            for entry in entries {
                if entry.builtin {
                    continue;
                }

                let def_line = entry.defined_at.start_lc.line;
                let def_col = if def_line > 0 {
                    entry.defined_at.start_lc.column.saturating_sub(1)
                } else {
                    entry.defined_at.start_lc.column
                };
                entry.type_info =
                    crate::inlay_hints::type_at_definition(typed_hir, def_line, def_col);
            }
        }
    }

    /// Look up the closest matching symbol by name, mimicking
    /// `DefScope::get_closest_by_name` by walking up the parent chain.
    pub fn get_closest_by_name(
        &self,
        scope_id: NodeId,
        name: &str,
        span: Span,
    ) -> Option<SymbolEntry> {
        let mut current = Some(scope_id);

        while let Some(sid) = current {
            if let Some(entries) = self.scopes.get(&sid) {
                let by_name: Vec<&SymbolEntry> =
                    entries.iter().filter(|e| *e.name == *name).collect();
                if let Some(entry) = Self::choose_closest(&by_name, span) {
                    return Some(entry);
                }
            }

            // Walk up to the parent scope.
            current = self.parents.get(&sid).copied();
        }

        None
    }

    /// Look up the closest callable symbol by name and arity.
    pub fn get_closest_by_name_and_arity(
        &self,
        scope_id: NodeId,
        name: &str,
        span: Span,
        arity: u16,
    ) -> Option<SymbolEntry> {
        let mut current = Some(scope_id);

        while let Some(sid) = current {
            if let Some(entries) = self.scopes.get(&sid) {
                let by_name: Vec<&SymbolEntry> = entries
                    .iter()
                    .filter(|e| *e.name == *name && e.kind.arity() == Some(arity))
                    .collect();
                if let Some(entry) = Self::choose_closest(&by_name, span) {
                    return Some(entry);
                }
            }

            current = self.parents.get(&sid).copied();
        }

        None
    }

    /// Collect all user-defined completion items from every scope.
    ///
    /// Items are deduplicated by `(label, detail)` and sorted
    /// alphabetically.  Temporary and builtin symbols are excluded.
    ///
    /// This is the canonical implementation shared by both the LSP server
    /// and the WASM playground bindings.
    pub fn collect_completion_items(&self) -> Vec<CompletionItem> {
        let mut seen = HashSet::new();
        let mut items = Vec::new();

        for entries in self.scopes.values() {
            for entry in entries.iter().filter(|e| !e.builtin && !e.temp) {
                let item = CompletionItem::from_symbol_entry(entry);
                let key = (item.label.clone(), item.detail.clone());

                if seen.insert(key) {
                    items.push(item);
                }
            }
        }

        items.sort_by(|a, b| {
            a.label
                .cmp(&b.label)
                .then_with(|| a.detail.cmp(&b.detail))
                .then_with(|| a.kind.cmp(&b.kind))
        });

        items
    }

    /// Collect completion items for methods of a given type.
    ///
    /// Searches all scopes for symbols whose qualified name starts with
    /// `type_name::` and have a callable kind (StructMethod, FunctionSelfRef,
    /// etc.).  Returns items with just the method name as the label (not
    /// the qualified `Type::method` form).
    pub fn collect_method_completions(&self, type_name: &str) -> Vec<CompletionItem> {
        let prefix = format!("{type_name}::");
        let mut seen = HashSet::new();
        let mut items = Vec::new();

        for entries in self.scopes.values() {
            for entry in entries {
                if !entry.name.starts_with(prefix.as_str()) {
                    continue;
                }
                // Include callable symbols (methods/functions) and struct fields.
                if !entry.kind.is_type_member() {
                    continue;
                }
                let member_name = &entry.name[prefix.len()..];
                if member_name.is_empty() {
                    continue;
                }
                let item = CompletionItem {
                    label: member_name.to_string(),
                    kind: entry.kind,
                    detail: completion_detail_for_entry(entry),
                };
                let key = (item.label.clone(), item.detail.clone());
                if seen.insert(key) {
                    items.push(item);
                }
            }
        }

        items.sort_by(|a, b| a.label.cmp(&b.label));
        items
    }

    /// Collect completion items for methods of a given protocol.
    ///
    /// Searches all scopes for symbols whose qualified name starts with
    /// `protocol_name::` and have a `ProtocolMethod` kind.  Returns items with
    /// just the method name as the label (not the qualified `Protocol::method`
    /// form).
    pub fn collect_protocol_completions(&self, protocol_name: &str) -> Vec<CompletionItem> {
        let prefix = format!("{protocol_name}::");
        let mut seen = HashSet::new();
        let mut items = Vec::new();

        for entries in self.scopes.values() {
            for entry in entries {
                if !entry.name.starts_with(prefix.as_str()) {
                    continue;
                }
                if !matches!(entry.kind, DefKind::ProtocolMethod(_)) {
                    continue;
                }
                let method_name = &entry.name[prefix.len()..];
                if method_name.is_empty() {
                    continue;
                }
                let item = CompletionItem {
                    label: method_name.to_string(),
                    kind: entry.kind,
                    detail: completion_detail_for_entry(entry),
                };
                let key = (item.label.clone(), item.detail.clone());
                if seen.insert(key) {
                    items.push(item);
                }
            }
        }

        items.sort_by(|a, b| a.label.cmp(&b.label));
        items
    }

    /// Find a method/function or struct field symbol whose qualified name ends
    /// with `::member_name`.
    ///
    /// This is used as a fallback when the cursor is on a field expression
    /// like `v1.add` or `v1.x` and the bare name doesn't resolve — we search
    /// for any `X::add` or `X::x` in scope that is a method, function, or
    /// struct field.
    pub fn find_member_by_suffix(
        &self,
        scope_id: NodeId,
        member_name: &str,
        span: Span,
    ) -> Option<SymbolEntry> {
        let suffix = format!("::{member_name}");
        let mut current = Some(scope_id);

        while let Some(sid) = current {
            if let Some(entries) = self.scopes.get(&sid) {
                let matches: Vec<&SymbolEntry> = entries
                    .iter()
                    .filter(|e| e.name.ends_with(suffix.as_str()) && e.kind.is_type_member())
                    .collect();

                if let Some(entry) = matches
                    .iter()
                    .rev()
                    .find(|e| {
                        e.defined_at.start_lc < span.start_lc
                            || e.kind.is_type_member()
                            || e.builtin
                    })
                    .cloned()
                {
                    return Some(entry.clone());
                }
            }

            current = self.parents.get(&sid).copied();
        }

        None
    }
}

/// A protocol-agnostic completion item produced by the analysis layer.
///
/// Both the LSP server and the WASM playground bindings map this into their
/// own wire format (`lsp_types::CompletionItem` or `CodemirrorCompletion`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompletionItem {
    /// The completion label shown to the user.
    pub label: String,
    /// The kind of symbol this completion represents.
    pub kind: DefKind,
    /// Optional detail string (e.g. `"fn(2)"` for a binary function).
    pub detail: Option<String>,
}

impl CompletionItem {
    /// Build a completion item from a [`SymbolEntry`].
    pub fn from_symbol_entry(entry: &SymbolEntry) -> Self {
        Self {
            label: entry.name.to_string(),
            kind: entry.kind,
            detail: completion_detail_for_entry(entry),
        }
    }
}

fn completion_detail_for_entry(entry: &SymbolEntry) -> Option<String> {
    entry
        .type_info
        .as_ref()
        .filter(|ty| ty.as_str() != entry.name.as_ref() && !ty.contains("unknown"))
        .cloned()
        .or_else(|| completion_detail(entry.kind))
}

/// Build an optional detail string for a completion item based on its
/// [`DefKind`].
///
/// This is the canonical source of truth — both the LSP server and the WASM
/// playground bindings should use this (or delegate to
/// [`CompletionItem::from_symbol_entry`]).
pub fn completion_detail(kind: DefKind) -> Option<String> {
    match kind {
        DefKind::Function(arity) | DefKind::FunctionSelfRef(arity) => Some(format!("fn({arity})")),
        DefKind::EnumVariant(arity) if arity > 0 => Some(format!("variant({arity})")),
        DefKind::ProtocolMethod(arity) | DefKind::StructMethod(arity) => {
            Some(format!("method({arity})"))
        }
        DefKind::StructField => Some("field".into()),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper: analyse `source` and return a `SymbolIndex`.
    fn index_for(source: &str) -> SymbolIndex {
        let result = crate::analyze(source, |_| {});
        SymbolIndex::from_analyzer(&result.analyzer)
    }

    #[test]
    fn completion_items_include_user_defined_functions() {
        let index = index_for("fn add(a, b) { a + b }");
        let items = index.collect_completion_items();
        assert!(
            items.iter().any(|i| i.label == "add"),
            "should include user-defined function `add`, got: {items:?}"
        );
    }

    #[test]
    fn completion_items_include_variables() {
        let index = index_for("let x = 42;");
        let items = index.collect_completion_items();
        assert!(
            items.iter().any(|i| i.label == "x"),
            "should include variable `x`, got: {items:?}"
        );
    }

    #[test]
    fn completion_items_include_enum_names() {
        let index = index_for("enum Color { Red, Green, Blue }");
        let items = index.collect_completion_items();
        assert!(
            items.iter().any(|i| i.label == "Color"),
            "should include enum `Color`, got: {items:?}"
        );
    }

    #[test]
    fn completion_items_are_sorted() {
        let index = index_for("fn zebra() { 0 }\nfn alpha() { 0 }");
        let items = index.collect_completion_items();
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
        let mut sorted = labels.clone();
        sorted.sort();
        assert_eq!(labels, sorted, "items should be sorted alphabetically");
    }

    #[test]
    fn completion_items_are_deduplicated() {
        // Multiple clauses of the same function should produce one completion.
        let index = index_for("fn f(0) { 0 }\nfn f(n) { n }");
        let items = index.collect_completion_items();
        let count = items.iter().filter(|i| i.label == "f").count();
        assert_eq!(count, 1, "duplicate function `f` should be deduplicated");
    }

    #[test]
    fn completion_detail_for_function() {
        assert_eq!(
            completion_detail(DefKind::Function(2)),
            Some("fn(2)".into())
        );
    }

    #[test]
    fn completion_detail_for_simple_enum_variant() {
        assert_eq!(completion_detail(DefKind::EnumVariant(0)), None);
    }

    #[test]
    fn completion_detail_for_tagged_enum_variant() {
        assert_eq!(
            completion_detail(DefKind::EnumVariant(1)),
            Some("variant(1)".into())
        );
    }

    #[test]
    fn completion_detail_for_variable() {
        assert_eq!(completion_detail(DefKind::Variable), None);
    }

    #[test]
    fn completion_items_prefer_inferred_type_info() {
        let result = crate::analyze(
            "fn add(a: i64, b: i64) -> i64 { a + b }\nlet answer = add(1, 2);",
            |_| {},
        );
        let mut index = SymbolIndex::from_analyzer(&result.analyzer);
        let typed_hir = crate::typed_hir::lower_and_typecheck(&result).unwrap();
        index.populate_type_info(&typed_hir);

        let items = index.collect_completion_items();
        let add = items.iter().find(|item| item.label == "add").unwrap();
        let answer = items.iter().find(|item| item.label == "answer").unwrap();

        assert_eq!(add.detail.as_deref(), Some("fn(i64, i64) -> i64"));
        assert_eq!(answer.detail.as_deref(), Some("i64"));
    }

    #[test]
    fn method_completions_for_struct() {
        let source = "struct Vector {\n    x: f64,\n    y: f64,\n}\nfn Vector::new(x: f64, y: f64) -> Vector { Vector { x, y } }\nfn Vector.add(self, other: Vector) -> Vector {\n    Vector::new(self.x + other.x, self.y + other.y)\n}";
        let result = crate::analyze(source, |_| {});
        assert!(
            result.parse_issues.is_empty(),
            "parse issues: {:?}",
            result.parse_issues
        );
        let index = SymbolIndex::from_analyzer(&result.analyzer);
        let methods = index.collect_method_completions("Vector");
        let labels: Vec<&str> = methods.iter().map(|m| m.label.as_str()).collect();
        assert!(
            labels.contains(&"new"),
            "should include `new`, got: {labels:?}"
        );
        assert!(
            labels.contains(&"add"),
            "should include `add`, got: {labels:?}"
        );
    }

    #[test]
    fn method_completions_empty_for_unknown_type() {
        let index = index_for("fn add(a, b) { a + b }");
        let methods = index.collect_method_completions("Vector");
        assert!(
            methods.is_empty(),
            "should be empty for unknown type, got: {methods:?}"
        );
    }

    #[test]
    fn method_completions_include_struct_fields() {
        let source = "struct Vector {\n    x: f64,\n    y: f64,\n}\nfn Vector.add(self, other: Vector) -> Vector { self }";
        let result = crate::analyze(source, |_| {});
        assert!(result.parse_issues.is_empty());
        let index = SymbolIndex::from_analyzer(&result.analyzer);
        let methods = index.collect_method_completions("Vector");
        let labels: Vec<&str> = methods.iter().map(|m| m.label.as_str()).collect();
        assert!(
            labels.contains(&"x"),
            "should include field `x`, got: {labels:?}"
        );
        assert!(
            labels.contains(&"y"),
            "should include field `y`, got: {labels:?}"
        );
        assert!(
            labels.contains(&"add"),
            "should include method `add`, got: {labels:?}"
        );
    }

    #[test]
    fn method_completions_for_enum() {
        let source = "enum Option<T> {\n  Some(T),\n  None,\n}\nfn Option.map(Option::Some(x), f) { Option::Some(f(x)) }\nfn Option.map(Option::None, _) { Option::None }\nfn Option.is_some(Option::Some(_)) { true }\nfn Option.is_some(Option::None) { false }";
        let result = crate::analyze(source, |_| {});
        assert!(result.parse_issues.is_empty());
        let index = SymbolIndex::from_analyzer(&result.analyzer);
        let methods = index.collect_method_completions("Option");
        let labels: Vec<&str> = methods.iter().map(|m| m.label.as_str()).collect();
        assert!(
            labels.contains(&"map"),
            "should include `map`, got: {labels:?}"
        );
        assert!(
            labels.contains(&"is_some"),
            "should include `is_some`, got: {labels:?}"
        );
    }

    #[test]
    fn protocol_completions_include_default_methods() {
        let source = "protocol Display {\n    fn to_string(self) { \"\" }\n    fn print(self) { log(to_string) }\n}";
        let result = crate::analyze(source, |_| {});
        assert!(result.parse_issues.is_empty());
        let index = SymbolIndex::from_analyzer(&result.analyzer);
        let methods = index.collect_protocol_completions("Display");
        let labels: Vec<&str> = methods.iter().map(|m| m.label.as_str()).collect();
        assert!(
            labels.contains(&"to_string"),
            "should include `to_string`, got: {labels:?}"
        );
        assert!(
            labels.contains(&"print"),
            "should include `print`, got: {labels:?}"
        );
    }

    #[test]
    fn protocol_completions_empty_for_unknown_protocol() {
        let index = index_for("fn add(a, b) { a + b }");
        let methods = index.collect_protocol_completions("Display");
        assert!(
            methods.is_empty(),
            "should be empty for unknown protocol, got: {methods:?}"
        );
    }
}
