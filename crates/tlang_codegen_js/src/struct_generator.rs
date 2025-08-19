use tlang_ast::node::Ident;
use tlang_hir::hir;

use crate::generator::CodegenJS;
use crate::js;

impl CodegenJS {
    /// Generate the struct declaration
    ///
    /// For simplicities sake, we will generate a constructor function for the struct
    /// as we would have to track the type of variables/paths to know whether we should
    /// generate a create struct call (using `new`) or normal function call.
    pub(crate) fn generate_struct_declaration(&mut self, decl: &hir::StructDeclaration) {
        self.push_indent();
        self.push_str("function ");
        self.push_str(&js::safe_js_variable_name(decl.name.as_str()));
        self.push_str("() {\n");

        self.inc_indent();

        self.push_indent();
        self.push_str("if (new.target == null) return Object.assign(new ");
        self.push_str(&js::safe_js_variable_name(decl.name.as_str()));
        self.push_str(", arguments[0]);\n");

        if !decl.fields.is_empty() {
            self.push_char('\n');
        }

        for field in &decl.fields {
            self.generate_struct_field(&field.name, &field.ty);
        }

        self.push_str("}\n");
        self.dec_indent();
    }

    fn generate_struct_field(&mut self, field_name: &Ident, _field_type: &hir::Ty) {
        self.push_indent();
        self.push_str("this.");
        self.push_str(&js::safe_js_variable_name(field_name.as_str()));
        self.push_str(" = undefined;\n");
    }
}
