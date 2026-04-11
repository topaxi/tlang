use std::collections::HashSet;

use tlang_ast::node::Module;

use crate::analyzer::{SemanticAnalysisContext, SemanticAnalysisPass};
use crate::diagnostic;

/// Validates protocol constraint satisfaction for impl blocks.
///
/// This pass runs after `DeclarationAnalyzer` which populates
/// `ctx.protocol_constraints` and `ctx.protocol_impls`. It checks that
/// when a type implements a protocol with constraints (e.g. `protocol Ord : Eq`),
/// all transitively required protocols are also implemented for that type.
pub struct ProtocolConstraintValidator;

impl ProtocolConstraintValidator {
    /// Collects all transitively required protocol names for a given protocol.
    fn collect_transitive_constraints(
        protocol: &str,
        constraints_map: &std::collections::HashMap<String, Vec<String>>,
        visited: &mut HashSet<String>,
    ) {
        if !visited.insert(protocol.to_string()) {
            return; // Already visited — prevents cycles
        }
        if let Some(direct_constraints) = constraints_map.get(protocol) {
            for constraint in direct_constraints {
                Self::collect_transitive_constraints(constraint, constraints_map, visited);
            }
        }
    }
}

impl SemanticAnalysisPass for ProtocolConstraintValidator {
    fn name(&self) -> &'static str {
        "ProtocolConstraintValidator"
    }

    fn analyze(&mut self, _module: &Module, ctx: &mut SemanticAnalysisContext, _is_root: bool) {
        // Build a set of (protocol, type) pairs for quick lookup
        let impl_set: HashSet<(String, String)> = ctx
            .protocol_impls
            .iter()
            .map(|(p, t, _span, _is_blanket)| (p.clone(), t.clone()))
            .collect();

        // Collect diagnostics separately to avoid borrowing ctx mutably while reading it
        let mut diagnostics = Vec::new();

        // For each impl block, check that all transitive constraints are satisfied.
        // Skip blanket impls since constraints are deferred to instantiation sites.
        for (protocol_name, target_type, impl_span, is_blanket) in &ctx.protocol_impls {
            if *is_blanket {
                continue;
            }

            let mut required = HashSet::new();
            Self::collect_transitive_constraints(
                protocol_name,
                &ctx.protocol_constraints,
                &mut required,
            );
            // Remove the protocol itself — we only care about its constraints
            required.remove(protocol_name);

            // Collect and sort missing protocols for deterministic error output
            let mut missing: Vec<_> = required
                .into_iter()
                .filter(|req| !impl_set.contains(&(req.clone(), target_type.clone())))
                .collect();
            missing.sort();

            for required_protocol in &missing {
                diagnostics.push(diagnostic::error_at!(
                    *impl_span,
                    "type `{}` implements `{}` but is missing `impl {} for {}`",
                    target_type,
                    protocol_name,
                    required_protocol,
                    target_type,
                ));
            }
        }

        // Validate associated type bindings: check that no unexpected
        // associated types are provided (i.e. binding names not declared by the protocol).
        for (protocol_name, target_type, binding_names, impl_span) in
            &ctx.impl_associated_type_bindings
        {
            if let Some(proto_assoc_types) = ctx.protocol_associated_types.get(protocol_name) {
                for bound_name in binding_names {
                    if !proto_assoc_types.iter().any(|n| n == bound_name) {
                        diagnostics.push(diagnostic::error_at!(
                            *impl_span,
                            "unexpected associated type `{}` in impl `{}` for `{}`",
                            bound_name,
                            protocol_name,
                            target_type,
                        ));
                    }
                }
            }
        }

        for diag in diagnostics {
            ctx.add_diagnostic(diag);
        }
    }
}
