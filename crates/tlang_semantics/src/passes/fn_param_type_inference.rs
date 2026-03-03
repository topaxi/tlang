use tlang_ast::{
    node::{FunctionDeclaration, PatKind, Ty},
    visit_mut::{VisitorMut, walk_fn_decl},
};
use tlang_span::NodeId;

use crate::analyzer::{SemanticAnalysisContext, SemanticAnalysisPass};

/// Semantic pass that infers parameter types from enum variant patterns.
///
/// When multiple function declarations share the same name and a parameter
/// position consistently uses variants of the same enum (e.g. `LinkedList::Empty`
/// and `LinkedList::Node(...)`), the parameter type is annotated as that enum
/// (`LinkedList`) so the lowered HIR can emit a proper type instead of `unknown`.
#[derive(Default)]
pub struct FnParamTypeInference;

impl VisitorMut for FnParamTypeInference {
    fn visit_fn_decls(&mut self, decls: &mut [FunctionDeclaration]) {
        let num_params = decls.iter().map(|d| d.parameters.len()).max().unwrap_or(0);

        for i in 0..num_params {
            if let Some(inferred_ty) = infer_enum_type(decls, i) {
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
}

/// Returns an inferred `Ty` for parameter `param_idx` if all declarations that
/// have an enum pattern at that position agree on the same enum type name.
/// Identifier, wildcard, and `self` patterns are treated as unconstrained and
/// do not prevent inference.
fn infer_enum_type(decls: &[FunctionDeclaration], param_idx: usize) -> Option<Ty> {
    let mut enum_type_name: Option<String> = None;

    for decl in decls {
        if let Some(param) = decl.parameters.get(param_idx) {
            match &param.pattern.kind {
                PatKind::Enum(enum_pattern) => {
                    let segs = &enum_pattern.path.segments;
                    if segs.len() < 2 {
                        return None;
                    }
                    // The enum type is the path prefix before the variant name.
                    // e.g. `LinkedList::Node` → `LinkedList`
                    let name = segs[segs.len() - 2].to_string();
                    match &enum_type_name {
                        None => enum_type_name = Some(name),
                        Some(existing) if *existing != name => return None,
                        _ => {}
                    }
                }
                PatKind::Identifier(_) | PatKind::Wildcard | PatKind::_Self => {}
                _ => return None,
            }
        }
    }

    enum_type_name.map(|name| {
        use tlang_ast::node::{Ident, Path};
        // NodeId::new(1) is used as a placeholder for this synthetic node;
        // the lowering only reads the path name, not the NodeId.
        Ty::new(
            NodeId::new(1),
            Path::new(vec![Ident::new(&name, Default::default())]),
        )
    })
}

impl SemanticAnalysisPass for FnParamTypeInference {
    fn mutate(&mut self, module: &mut tlang_ast::node::Module) {
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
