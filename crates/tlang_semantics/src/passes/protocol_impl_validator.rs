use std::collections::HashMap;

use tlang_ast::node::{
    FunctionDeclaration, FunctionParameter, Module, PatKind, ProtocolDeclaration, StmtKind, Ty,
    TyKind, TypeParam,
};
use tlang_span::Span;

use crate::analyzer::{SemanticAnalysisContext, SemanticAnalysisPass};
use crate::diagnostic;

/// Validates that protocol impl method signatures match the protocol method definitions.
///
/// This pass runs after `DeclarationAnalyzer` and checks:
/// - The `self` parameter is present in impl iff it is present in the protocol definition.
/// - The parameter count matches between impl and protocol.
/// - Where both sides have explicit type annotations on non-self parameters, the types match.
/// - Where both sides have explicit return type annotations, the return types match.
pub struct ProtocolImplValidator;

fn param_is_self(param: &FunctionParameter) -> bool {
    matches!(param.pattern.kind, PatKind::_Self)
}

/// Returns `true` when the parameter uses a plain identifier as its pattern
/// (i.e. not `self`, not a destructuring/enum/list/wildcard pattern).
/// Plain identifiers in the receiver position are disallowed when the protocol
/// method declares an explicit `self` receiver.
fn param_is_plain_identifier(param: &FunctionParameter) -> bool {
    matches!(param.pattern.kind, PatKind::Identifier(_))
}

/// Returns a display string for a type annotation, used for mismatch error messages.
fn format_ty(ty: &Ty) -> String {
    match &ty.kind {
        TyKind::Unknown => "_".to_string(),
        TyKind::Path(path) => {
            let base = path.to_string();
            if ty.parameters.is_empty() {
                base
            } else {
                let params = ty
                    .parameters
                    .iter()
                    .map(format_ty)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{base}<{params}>")
            }
        }
        TyKind::Union(paths) => paths
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<_>>()
            .join(" | "),
        TyKind::Fn(params, ret) => {
            let param_strs = params
                .iter()
                .map(|p| format_ty(&p.ty))
                .collect::<Vec<_>>()
                .join(", ");
            format!("fn({param_strs}) -> {}", format_ty(ret))
        }
    }
}

/// Returns `true` if `ty` is a single-segment path that names one of the given type parameters,
/// or is the special `Self` type. These should not be compared as concrete types.
fn is_type_var(ty: &Ty, type_param_names: &[String]) -> bool {
    if let TyKind::Path(path) = &ty.kind
        && path.segments.len() == 1
    {
        let name = path.segments[0].as_str();
        return name == "Self" || type_param_names.iter().any(|tp| tp == name);
    }
    false
}

fn type_param_names(type_params: &[TypeParam]) -> Vec<String> {
    type_params.iter().map(|tp| tp.name.to_string()).collect()
}

/// Checks whether two concrete type annotations are structurally equal (by their
/// string representations, ignoring `NodeId` and `Span`).
fn types_equal(a: &Ty, b: &Ty) -> bool {
    format_ty(a) == format_ty(b)
}

#[allow(clippy::too_many_lines)]
fn validate_impl_method(
    protocol: &ProtocolDeclaration,
    proto_method_name: &str,
    proto_params: &[FunctionParameter],
    proto_return_type: Option<&Ty>,
    proto_span: Span,
    impl_method: &FunctionDeclaration,
    diagnostics: &mut Vec<tlang_diagnostics::Diagnostic>,
) {
    let proto_type_param_names = type_param_names(&protocol.type_params);

    // Validate the receiver / self parameter.
    //
    // In tlang, impl methods may use destructuring patterns (e.g. `Option::Some(x)`)
    // as the receiver instead of the bare `self` keyword — this is valid.
    // What is NOT valid:
    //   - Protocol has `self` but impl uses a plain identifier in that position,
    //     which is ambiguous and misleading (use `self` or a pattern instead).
    //   - Protocol has no `self` but impl uses `self` (wrong method shape).
    let proto_has_self = proto_params.first().is_some_and(param_is_self);
    let impl_has_self = impl_method.parameters.first().is_some_and(param_is_self);

    if proto_has_self && !impl_has_self {
        // Enum/list/wildcard patterns in the receiver position are fine.
        // Only flag plain identifiers — they suggest the author forgot `self`.
        if impl_method
            .parameters
            .first()
            .is_some_and(param_is_plain_identifier)
        {
            let first_param_span = impl_method.parameters[0].span;
            diagnostics.push(
                diagnostic::error_at!(
                    first_param_span,
                    "impl of `{}::{}` is missing required `self` parameter (use `self` or a destructuring pattern)",
                    protocol.name,
                    proto_method_name
                )
                .with_label(
                    format!("`{}::{}` defined here", protocol.name, proto_method_name),
                    proto_span,
                ),
            );
        }
    } else if !proto_has_self && impl_has_self {
        let self_span = impl_method.parameters[0].span;
        diagnostics.push(
            diagnostic::error_at!(
                self_span,
                "impl of `{}::{}` has unexpected `self` parameter (not in protocol definition)",
                protocol.name,
                proto_method_name
            )
            .with_label(
                format!("`{}::{}` defined here", protocol.name, proto_method_name),
                proto_span,
            ),
        );
    }

    // Validate parameter count.
    if proto_params.len() != impl_method.parameters.len() {
        diagnostics.push(
            diagnostic::error_at!(
                impl_method.span,
                "impl of `{}::{}` has {} parameter(s) but protocol definition has {}",
                protocol.name,
                proto_method_name,
                impl_method.parameters.len(),
                proto_params.len()
            )
            .with_label(
                format!("`{}::{}` defined here", protocol.name, proto_method_name),
                proto_span,
            ),
        );
        // Don't attempt per-parameter type checks when counts differ.
        return;
    }

    // Validate type annotations on each non-self parameter.
    for (i, (proto_param, impl_param)) in
        proto_params.iter().zip(&impl_method.parameters).enumerate()
    {
        // Skip the self parameter for type comparison (self type is the impl target type).
        if param_is_self(proto_param) || param_is_self(impl_param) {
            continue;
        }

        if let (Some(proto_ty), Some(impl_ty)) =
            (&proto_param.type_annotation, &impl_param.type_annotation)
        {
            // Don't compare if the protocol side uses a type variable.
            if is_type_var(proto_ty, &proto_type_param_names) {
                continue;
            }

            if !types_equal(proto_ty, impl_ty) {
                diagnostics.push(
                    diagnostic::error_at!(
                        impl_param.span,
                        "type mismatch for parameter {} of `{}::{}`: expected `{}`, found `{}`",
                        i + 1,
                        protocol.name,
                        proto_method_name,
                        format_ty(proto_ty),
                        format_ty(impl_ty)
                    )
                    .with_label(
                        format!("`{}::{}` defined here", protocol.name, proto_method_name),
                        proto_span,
                    ),
                );
            }
        }
    }

    // Validate return type annotation.
    if let (Some(proto_ret), Some(impl_ret)) =
        (proto_return_type, &impl_method.return_type_annotation)
        && !is_type_var(proto_ret, &proto_type_param_names)
        && !types_equal(proto_ret, impl_ret)
    {
        diagnostics.push(
            diagnostic::error_at!(
                impl_method.span,
                "return type mismatch for `{}::{}`: expected `{}`, found `{}`",
                protocol.name,
                proto_method_name,
                format_ty(proto_ret),
                format_ty(impl_ret)
            )
            .with_label(
                format!("`{}::{}` defined here", protocol.name, proto_method_name),
                proto_span,
            ),
        );
    }
}

impl SemanticAnalysisPass for ProtocolImplValidator {
    fn name(&self) -> &'static str {
        "ProtocolImplValidator"
    }

    fn analyze(&mut self, module: &Module, ctx: &mut SemanticAnalysisContext, _is_root: bool) {
        // Collect all protocol declarations in this module.
        let mut protocol_map: HashMap<String, &ProtocolDeclaration> = HashMap::new();
        for stmt in &module.statements {
            if let StmtKind::ProtocolDeclaration(decl) = &stmt.kind {
                protocol_map.insert(decl.name.to_string(), decl);
            }
        }

        let mut diagnostics = Vec::new();

        for stmt in &module.statements {
            let StmtKind::ImplBlock(impl_block) = &stmt.kind else {
                continue;
            };

            let protocol_name = impl_block.protocol_name.to_string();
            let Some(protocol) = protocol_map.get(&protocol_name) else {
                continue;
            };

            // Build a lookup from protocol method name to (parameters, return_type, span).
            let proto_method_map: HashMap<&str, (&[FunctionParameter], Option<&Ty>, Span)> =
                protocol
                    .methods
                    .iter()
                    .map(|m| {
                        (
                            m.name.as_str(),
                            m.parameters.as_slice(),
                            m.return_type_annotation.as_ref(),
                            m.span,
                        )
                    })
                    .map(|(name, params, ret, span)| (name, (params, ret, span)))
                    .collect();

            for impl_method in &impl_block.methods {
                let Some(method_name) = impl_method.name() else {
                    continue;
                };

                let Some((proto_params, proto_ret, proto_span)) =
                    proto_method_map.get(method_name.as_str())
                else {
                    continue;
                };

                validate_impl_method(
                    protocol,
                    &method_name,
                    proto_params,
                    *proto_ret,
                    *proto_span,
                    impl_method,
                    &mut diagnostics,
                );
            }
        }

        for diag in diagnostics {
            ctx.add_diagnostic(diag);
        }
    }
}
