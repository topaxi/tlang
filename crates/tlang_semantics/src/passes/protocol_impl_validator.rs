use std::collections::HashMap;

use tlang_ast::node::{
    FunctionDeclaration, FunctionParameter, Module, PatKind, ProtocolDeclaration, StmtKind,
};
use tlang_span::Span;

use crate::analyzer::{SemanticAnalysisContext, SemanticAnalysisPass};
use crate::diagnostic;

/// Validates structural conformance between protocol impl methods and protocol definitions.
///
/// This pass runs after `DeclarationAnalyzer` and checks:
/// - The `self` receiver is present/absent in the impl consistent with the protocol.
/// - The parameter count matches.
///
/// Type annotation compatibility (parameter types, return types) is deferred to
/// the type checker (`tlang_typeck`), where types are fully resolved.
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

fn validate_impl_method(
    protocol: &ProtocolDeclaration,
    proto_method_name: &str,
    proto_params: &[FunctionParameter],
    proto_span: Span,
    impl_method: &FunctionDeclaration,
    diagnostics: &mut Vec<tlang_diagnostics::Diagnostic>,
) {
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

            // Build a lookup from protocol method name to (parameters, span).
            let proto_method_map: HashMap<&str, (&[FunctionParameter], Span)> = protocol
                .methods
                .iter()
                .map(|m| (m.name.as_str(), (m.parameters.as_slice(), m.span)))
                .collect();

            for impl_method in &impl_block.methods {
                let Some(method_name) = impl_method.name() else {
                    continue;
                };

                let Some((proto_params, proto_span)) = proto_method_map.get(method_name.as_str())
                else {
                    continue;
                };

                validate_impl_method(
                    protocol,
                    &method_name,
                    proto_params,
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
