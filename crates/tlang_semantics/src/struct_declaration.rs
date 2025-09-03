use tlang_ast::{node::Module, visit::Visitor};

use crate::{analyzer::SemanticAnalysisContext, analyzer::SemanticAnalysisPass};

/**
 * Struct declaration analysis pass that collects struct declarations
 * and adds them to the semantic analysis context.
 */
#[derive(Default)]
pub struct StructDeclarationAnalyzer;

impl StructDeclarationAnalyzer {
    pub fn new() -> Self {
        Self
    }
}

impl SemanticAnalysisPass for StructDeclarationAnalyzer {
    fn analyze(&mut self, module: &Module, ctx: &mut SemanticAnalysisContext, _is_root: bool) {
        self.visit_module(module, ctx);
    }
}

impl<'ast> Visitor<'ast> for StructDeclarationAnalyzer {
    type Context = SemanticAnalysisContext;

    fn visit_struct_decl(
        &mut self,
        decl: &'ast tlang_ast::node::StructDeclaration,
        ctx: &mut Self::Context,
    ) {
        ctx.struct_declarations
            .insert(decl.name.to_string(), decl.clone());
    }
}
