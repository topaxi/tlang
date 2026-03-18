use std::collections::HashMap;

use tlang_ast::{
    node::{FunctionDeclaration, ImplBlock, Module, PatKind, Res, StmtKind, Ty, TyKind},
    token::Literal,
    visit_mut::{VisitorMut, walk_fn_decl},
};
use tlang_span::NodeId;

use crate::{
    analyzer::{SemanticAnalysisContext, SemanticAnalysisPass},
    builtin_types,
};

/// Semantic pass that infers parameter types from patterns across overloaded
/// function declarations.
///
/// - **Enum patterns** (`LinkedList::Node(...)`): all overloads agree on the
///   same enum prefix → infer that enum type.
/// - **Literal patterns** (`0`, `"hello"`, `true`, …): infer the canonical
///   builtin type (`i64`, `String`, `bool`, …).
/// - **List patterns** (`[]`, `[x, ...xs]`): infer `Slice`, since the
///   interpreter matches list patterns against both list slices and strings.
/// - When overloads **disagree** on types but each is a known type, a
///   `TyKind::Union` annotation is emitted (e.g. `String | Slice`).
/// - Identifier, wildcard, and `self` patterns are **unconstrained** and do
///   not affect inference.
#[derive(Default)]
pub struct FnParamTypeInference {
    /// Maps enum names to the `NodeId` of their declaration statement,
    /// populated by a pre-scan of the module before the main visitor run.
    enum_decl_ids: HashMap<String, NodeId>,
}

impl VisitorMut for FnParamTypeInference {
    fn visit_fn_decls(&mut self, decls: &mut [FunctionDeclaration]) {
        let num_params = decls.iter().map(|d| d.parameters.len()).max().unwrap_or(0);

        for i in 0..num_params {
            if let Some(inferred_ty) = infer_param_type(decls, i, &self.enum_decl_ids) {
                for decl in decls.iter_mut() {
                    if let Some(param) = decl.parameters.get_mut(i)
                        && param.type_annotation.is_none()
                    {
                        param.type_annotation = Some(inferred_ty.clone());
                    }
                }
            }
        }

        // Recurse into function bodies to handle nested declarations.
        for decl in decls.iter_mut() {
            walk_fn_decl(self, decl);
        }
    }

    fn visit_impl_block(&mut self, impl_block: &mut ImplBlock) {
        // Group methods by name so multi-clause methods get type inference
        // applied across all their overloads together, matching the behaviour
        // for top-level FunctionDeclarations.
        let mut groups: Vec<(String, Vec<usize>)> = Vec::new();
        for (i, decl) in impl_block.methods.iter().enumerate() {
            let name = decl.name();
            if let Some(group) = groups.iter_mut().find(|(n, _)| *n == name) {
                group.1.push(i);
            } else {
                groups.push((name, vec![i]));
            }
        }
        for (_name, indices) in groups {
            if indices.len() == 1 {
                walk_fn_decl(self, &mut impl_block.methods[indices[0]]);
            } else {
                // Clone the group, run inference, then write annotations back.
                let mut group: Vec<FunctionDeclaration> = indices
                    .iter()
                    .map(|&i| impl_block.methods[i].clone())
                    .collect();
                self.visit_fn_decls(&mut group);
                for (pos, &i) in indices.iter().enumerate() {
                    impl_block.methods[i] = group[pos].clone();
                }
            }
        }
    }
}

/// The type constraint contributed by a single pattern.
#[derive(Debug, Clone, PartialEq)]
enum PatternType {
    /// A named type (enum name or builtin type name).
    Named(String),
}

/// Returns the `PatternType` contributed by a single pattern, or `None` if the
/// pattern is unconstrained (identifier, wildcard, `self`) or unrecognised.
fn pattern_type(pat: &PatKind) -> Option<Result<PatternType, ()>> {
    match pat {
        PatKind::Enum(enum_pattern) => {
            let segs = &enum_pattern.path.segments;
            if segs.len() < 2 {
                return Some(Err(()));
            }
            let name = segs[segs.len() - 2].to_string();
            Some(Ok(PatternType::Named(name)))
        }
        PatKind::Literal(lit) => {
            builtin_type_for_literal(lit).map(|name| Ok(PatternType::Named(name.to_string())))
        }
        PatKind::List(_) => Some(Ok(PatternType::Named(builtin_types::SLICE.to_string()))),
        // Unconstrained — don't block or contribute to inference.
        PatKind::Identifier(_) | PatKind::Wildcard | PatKind::_Self => None,
        // Rest patterns at the top level, None patterns — block inference.
        _ => Some(Err(())),
    }
}

/// Returns the canonical builtin type name for a literal, or `None` if the
/// literal kind carries no type information (e.g. `Literal::None`).
fn builtin_type_for_literal(lit: &Literal) -> Option<&'static str> {
    match lit {
        Literal::Boolean(_) => Some(builtin_types::BOOL),
        // Both signed and unsigned integer literals default to i64, since
        // non-negative integers are stored as UnsignedInteger in the AST.
        Literal::Integer(_) | Literal::UnsignedInteger(_) => Some(builtin_types::I64),
        Literal::Float(_) => Some(builtin_types::F64),
        Literal::String(_) => Some(builtin_types::STRING),
        Literal::Char(_) => Some(builtin_types::CHAR),
        Literal::None => None,
    }
}

/// Infers a `Ty` for parameter `param_idx` by inspecting all declarations.
///
/// - If all constrained patterns agree on one type → `TyKind::Path`.
/// - If constrained patterns are all known types but disagree → `TyKind::Union`.
/// - If any pattern is unrecognised or blocks inference → `None`.
fn infer_param_type(
    decls: &[FunctionDeclaration],
    param_idx: usize,
    enum_decl_ids: &HashMap<String, NodeId>,
) -> Option<Ty> {
    let mut type_names: Vec<String> = Vec::new();

    for decl in decls {
        if let Some(param) = decl.parameters.get(param_idx) {
            match pattern_type(&param.pattern.kind) {
                None => {}                    // unconstrained, skip
                Some(Err(())) => return None, // blocks inference
                Some(Ok(PatternType::Named(name))) if !type_names.contains(&name) => {
                    type_names.push(name);
                }
                Some(Ok(PatternType::Named(_))) => {}
            }
        }
    }

    if type_names.is_empty() {
        return None;
    }

    use tlang_ast::node::{Ident, Path};

    let make_path = |name: &str| {
        let res = if builtin_types::ALL.contains(&name) {
            Res::PrimTy
        } else if let Some(&node_id) = enum_decl_ids.get(name) {
            Res::Def(node_id)
        } else {
            Res::Unresolved
        };
        let mut path = Path::new(vec![Ident::new(name, Default::default())]);
        path.res = res;
        path
    };

    // NodeId::new(1) is a placeholder for synthetic nodes; lowering only reads
    // the kind, not the NodeId.
    if type_names.len() == 1 {
        Some(Ty {
            id: NodeId::new(1),
            kind: TyKind::Path(make_path(&type_names[0])),
            parameters: vec![],
            span: Default::default(),
        })
    } else {
        Some(Ty {
            id: NodeId::new(1),
            kind: TyKind::Union(type_names.iter().map(|n| make_path(n)).collect()),
            parameters: vec![],
            span: Default::default(),
        })
    }
}

/// Scans the top-level statements of a module and returns a map from enum name
/// to the `NodeId` of the declaration statement, matching what
/// `DeclarationAnalyzer` records in the symbol table.
fn collect_enum_decl_ids(module: &Module) -> HashMap<String, NodeId> {
    let mut map = HashMap::new();
    for stmt in &module.statements {
        if let StmtKind::EnumDeclaration(decl) = &stmt.kind {
            map.insert(decl.name.to_string(), stmt.id);
        }
    }
    map
}

impl SemanticAnalysisPass for FnParamTypeInference {
    fn mutate(&mut self, module: &mut tlang_ast::node::Module) {
        self.enum_decl_ids = collect_enum_decl_ids(module);
        self.visit_module(module);
    }

    fn analyze(
        &mut self,
        _module: &tlang_ast::node::Module,
        _ctx: &mut SemanticAnalysisContext,
        _is_root: bool,
    ) {
        // Type annotation is handled in mutate(); no symbol-table work needed.
    }
}
