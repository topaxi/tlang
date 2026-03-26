use oxc_ast::NONE;
use oxc_ast::ast::*;
use oxc_span::SPAN;
use tlang_hir as hir;

use crate::generator::InnerCodegen;
use crate::js;

impl<'a> InnerCodegen<'a> {
    /// Generate the struct declaration as a constructor function.
    ///
    /// For simplicities sake, we generate a constructor function for the struct
    /// as we would have to track the type of variables/paths to know whether we should
    /// generate a create struct call (using `new`) or normal function call.
    pub fn generate_struct_declaration(&mut self, decl: &hir::StructDeclaration) -> Statement<'a> {
        let raw_name = decl.name.as_str();
        // Use the pre-registered local scope name if available; otherwise declare
        // a fresh local binding.  Using resolve_local_variable (not the full
        // resolve_variable) prevents accidentally picking up builtin mappings from
        // parent scopes (e.g. a struct named `math` would otherwise map to `Math`).
        let name = self
            .current_scope()
            .resolve_local_variable(raw_name)
            .unwrap_or_else(|| self.current_scope().declare_local_variable(raw_name));

        let mut body_stmts = Vec::new();

        // if (new.target == null) return Object.assign(new Name, arguments[0]);
        let new_target = self.ast.expression_meta_property(
            SPAN,
            self.ast.identifier_name(SPAN, "new"),
            self.ast.identifier_name(SPAN, "target"),
        );
        let test = self.ast.expression_binary(
            SPAN,
            new_target,
            BinaryOperator::Equality,
            self.ast.expression_null_literal(SPAN),
        );

        let new_expr = self
            .ast
            .expression_new(SPAN, self.ident_expr(&name), NONE, self.ast.vec());
        let args0 = self.computed_member_expr(self.ident_expr("arguments"), self.num_expr(0.0));
        let assign_call = self.call_expr(
            self.static_member_expr(self.ident_expr("Object"), "assign"),
            vec![Argument::from(new_expr), Argument::from(args0)],
        );
        let return_stmt = self.ast.statement_return(SPAN, Some(assign_call));
        body_stmts.push(self.ast.statement_if(SPAN, test, return_stmt, None));

        // this.field = undefined; for each field
        for field in &decl.fields {
            let field_name = js::safe_js_variable_name(field.name.as_str());
            let target = self.assignment_target_member(self.this_expr(), &field_name);
            let assign = self.assign_expr(target, self.undefined_expr());
            body_stmts.push(self.expr_stmt(assign));
        }

        let formal_params = self.formal_params(self.ast.vec());
        let body = self.fn_body(self.ast.vec_from_iter(body_stmts));

        Statement::FunctionDeclaration(self.ast.alloc_function(
            SPAN,
            FunctionType::FunctionDeclaration,
            Some(self.binding_ident(&name)),
            false,
            false,
            false,
            NONE,
            NONE,
            formal_params,
            NONE,
            Some(body),
        ))
    }
}
