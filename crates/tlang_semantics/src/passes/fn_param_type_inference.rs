use std::collections::HashMap;

use tlang_ast::{
    node::{
        ExprKind, FunctionDeclaration, Ident, ImplBlock, Module, PatKind, Path, Res, StmtKind, Ty,
        TyKind,
    },
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
    fn visit_fn_decl(&mut self, decl: &mut FunctionDeclaration) {
        // Infer `self` parameter type from the method's qualified name.
        // Instance methods like `fn Vector.add(self, ...)` have a
        // FieldExpression name — extract the struct type from the base path.
        annotate_self_param(decl);

        // Also infer types from patterns for single-clause functions.
        // Multi-clause functions are handled by visit_fn_decls which has
        // access to all overloads at once.
        annotate_params_from_patterns(std::slice::from_mut(decl), &self.enum_decl_ids);

        walk_fn_decl(self, decl);
    }

    fn visit_fn_decls(&mut self, decls: &mut [FunctionDeclaration]) {
        // Annotate `self` parameters before cross-overload inference.
        for decl in decls.iter_mut() {
            annotate_self_param(decl);
        }

        annotate_params_from_patterns(decls, &self.enum_decl_ids);

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
            let Some(name) = decl.name() else {
                continue;
            };
            if let Some(group) = groups.iter_mut().find(|(n, _)| *n == name) {
                group.1.push(i);
            } else {
                groups.push((name, vec![i]));
            }
        }
        for (_name, indices) in groups {
            // Always run via visit_fn_decls — this handles both single and
            // multi-clause methods uniformly, including pattern-based type
            // inference for single-clause methods with destructuring params.
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

/// Annotates the `self` parameter of an instance method with the struct type
/// inferred from the function's qualified name.
///
/// For `fn Vector.add(self, ...)`, the name is a `FieldExpression` with
/// base path `Vector` — the `self` parameter gets type annotation `Vector`.
fn annotate_self_param(decl: &mut FunctionDeclaration) {
    let struct_path = match &decl.name.kind {
        ExprKind::FieldExpression(field_expr) => field_expr.base.path().cloned(),
        _ => None,
    };

    let struct_path = match struct_path {
        Some(p) => p,
        None => return,
    };

    for param in &mut decl.parameters {
        if matches!(param.pattern.kind, PatKind::_Self) && param.type_annotation.is_none() {
            param.type_annotation = Some(Ty {
                id: NodeId::new(1),
                kind: TyKind::Path(Path::new(
                    struct_path
                        .segments
                        .iter()
                        .map(|s| Ident::new(s.as_str(), Default::default()))
                        .collect(),
                )),
                parameters: vec![],
                span: Default::default(),
            });
        }
    }
}

/// The type constraint contributed by a single pattern.
#[derive(Debug, Clone, PartialEq)]
enum PatternType {
    /// A named type (enum name or builtin type name).
    Named { name: String, open_world_safe: bool },
}

/// Returns the `PatternType` contributed by a single pattern, or `None` if the
/// pattern is unconstrained (identifier, wildcard, `self`) or unrecognised.
fn pattern_type(pat: &PatKind) -> Option<Result<PatternType, ()>> {
    match pat {
        PatKind::Enum(enum_pattern) => {
            let segs = &enum_pattern.path.segments;
            match segs.len() {
                0 => Some(Err(())),
                1 => {
                    // Struct pattern: `Page { title }` — the type is the struct
                    // name itself (the single path segment).
                    Some(Ok(PatternType::Named {
                        name: segs[0].to_string(),
                        open_world_safe: false,
                    }))
                }
                _ => {
                    // Enum variant pattern: `Option::Some(x)` — the type is the
                    // second-to-last segment (e.g. "Option" from "Option::Some").
                    let name = segs[segs.len() - 2].to_string();
                    Some(Ok(PatternType::Named {
                        name,
                        open_world_safe: false,
                    }))
                }
            }
        }
        PatKind::Literal(lit) => builtin_type_for_literal(lit).map(|name| {
            Ok(PatternType::Named {
                name: name.to_string(),
                open_world_safe: true,
            })
        }),
        PatKind::List(_) => Some(Ok(PatternType::Named {
            name: builtin_types::LIST.to_string(),
            open_world_safe: true,
        })),
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

/// Infers and sets parameter type annotations for all parameters across
/// `decls` using cross-overload pattern analysis.  For a single-clause
/// function this reduces to pattern-based inference for that one clause.
fn annotate_params_from_patterns(
    decls: &mut [FunctionDeclaration],
    enum_decl_ids: &HashMap<String, NodeId>,
) {
    let num_params = decls.iter().map(|d| d.parameters.len()).max().unwrap_or(0);

    for i in 0..num_params {
        if let Some(inferred_ty) = infer_param_type(decls, i, enum_decl_ids) {
            for decl in decls.iter_mut() {
                if let Some(param) = decl.parameters.get_mut(i)
                    && param.type_annotation.is_none()
                {
                    param.type_annotation = Some(inferred_ty.clone());
                }
            }
        }
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
    let mut saw_unconstrained = false;
    let mut has_open_world_unsafe_constraint = false;

    for decl in decls {
        if let Some(param) = decl.parameters.get(param_idx) {
            match pattern_type(&param.pattern.kind) {
                None => saw_unconstrained = true,
                Some(Err(())) => return None, // blocks inference
                Some(Ok(PatternType::Named {
                    name,
                    open_world_safe,
                })) if !type_names.contains(&name) => {
                    has_open_world_unsafe_constraint |= !open_world_safe;
                    type_names.push(name);
                }
                Some(Ok(PatternType::Named {
                    open_world_safe, ..
                })) => {
                    has_open_world_unsafe_constraint |= !open_world_safe;
                }
            }
        }
    }

    if type_names.is_empty() {
        return None;
    }

    if saw_unconstrained && has_open_world_unsafe_constraint {
        return None;
    }

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
