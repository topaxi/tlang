use tlang_ast::node::{Module, Visibility};
use tlang_ast::visit::Visitor;

use crate::{analyzer::SemanticAnalysisContext, analyzer::SemanticAnalysisPass, diagnostic};

/// Validates that multi-clause function declarations with the same name
/// and arity have consistent visibility (`pub` or private).
///
/// **Valid:**
/// ```text
/// pub fn foo() { foo(1) }    // arity 0 is public
/// fn foo(x) { x }            // arity 1 is private — different arity, OK
/// ```
///
/// **Invalid:**
/// ```text
/// pub fn bar(Some(x)) { x }  // arity 1 is public
/// fn bar(None) { 0 }          // arity 1 is private — same arity, inconsistent
/// ```
#[derive(Default)]
pub struct VisibilityValidator;

impl SemanticAnalysisPass for VisibilityValidator {
    fn analyze(&mut self, module: &Module, ctx: &mut SemanticAnalysisContext, _is_root: bool) {
        self.visit_module(module, ctx);
    }
}

impl<'ast> Visitor<'ast> for VisibilityValidator {
    type Context = SemanticAnalysisContext;

    fn visit_fn_decls(
        &mut self,
        decls: &'ast [tlang_ast::node::FunctionDeclaration],
        ctx: &mut Self::Context,
    ) {
        if decls.len() < 2 {
            return;
        }

        // Group by arity and check visibility consistency within each group.
        let mut arity_groups: std::collections::HashMap<usize, Option<Visibility>> =
            std::collections::HashMap::new();

        for decl in decls {
            let arity = decl.parameters.len();
            match arity_groups.get(&arity) {
                Some(Some(expected_vis)) if *expected_vis != decl.visibility => {
                    let fn_name = decl.name_or_invalid();
                    let vis_label = match decl.visibility {
                        Visibility::Public => "public",
                        Visibility::Private => "private",
                    };
                    let expected_label = match expected_vis {
                        Visibility::Public => "public",
                        Visibility::Private => "private",
                    };
                    let message = format!(
                        "Inconsistent visibility for function `{fn_name}` with arity {arity}: \
                         this clause is {vis_label}, but a previous clause is {expected_label}"
                    );
                    ctx.add_diagnostic(diagnostic::Diagnostic::error(&message, decl.span));
                }
                Some(Some(_)) => {
                    // Same visibility, OK
                }
                _ => {
                    arity_groups.insert(arity, Some(decl.visibility));
                }
            }
        }
    }
}
