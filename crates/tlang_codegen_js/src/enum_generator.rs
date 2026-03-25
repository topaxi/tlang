use std::collections::HashSet;

use oxc_ast::NONE;
use oxc_ast::ast::*;
use oxc_span::SPAN;
use tlang_ast::node::Ident;
use tlang_hir as hir;

use crate::generator::InnerCodegen;

impl<'a> InnerCodegen<'a> {
    pub fn generate_enum_declaration(&mut self, decl: &hir::EnumDeclaration) -> Statement<'a> {
        let raw_name = decl.name.as_str();
        // Use the pre-registered scope name if available; otherwise declare it
        // now (handles enums declared outside of generate_stmts flow).
        let name = self
            .current_scope()
            .resolve_variable(raw_name)
            .unwrap_or_else(|| self.current_scope().declare_local_variable(raw_name));

        let mut elements = Vec::new();

        // tag = this
        elements.push(ClassElement::PropertyDefinition(
            self.ast.alloc_property_definition(
                SPAN,
                PropertyDefinitionType::PropertyDefinition,
                self.ast.vec(),
                PropertyKey::StaticIdentifier(self.ast.alloc_identifier_name(SPAN, "tag")),
                NONE,
                Some(self.this_expr()),
                false,
                false,
                false,
                false,
                false,
                false,
                false,
                None,
            ),
        ));

        // Field declarations
        let mut seen = HashSet::new();
        let field_names: Vec<String> = decl
            .variants
            .iter()
            .flat_map(|v| v.parameters.iter())
            .map(|f| f.name.as_str().to_string())
            .filter(|name| seen.insert(name.clone()))
            .collect();

        for field_name in &field_names {
            let field_name_str = self.alloc_str(field_name);
            let (key, computed) = if field_name.chars().all(char::is_numeric) {
                let num = field_name.parse::<f64>().unwrap();
                (
                    PropertyKey::NumericLiteral(self.ast.alloc_numeric_literal(
                        SPAN,
                        num,
                        None,
                        NumberBase::Decimal,
                    )),
                    true,
                )
            } else {
                (
                    PropertyKey::StaticIdentifier(
                        self.ast.alloc_identifier_name(SPAN, field_name_str),
                    ),
                    false,
                )
            };

            elements.push(ClassElement::PropertyDefinition(
                self.ast.alloc_property_definition(
                    SPAN,
                    PropertyDefinitionType::PropertyDefinition,
                    self.ast.vec(),
                    key,
                    NONE,
                    None,
                    computed,
                    false,
                    false,
                    false,
                    false,
                    false,
                    false,
                    None,
                ),
            ));
        }

        // Variants
        for variant in &decl.variants {
            elements.push(self.generate_enum_variant_element(&variant.name, &variant.parameters));
        }

        let class_body = self.ast.class_body(SPAN, self.ast.vec_from_iter(elements));

        Statement::ClassDeclaration(self.ast.alloc_class(
            SPAN,
            ClassType::ClassDeclaration,
            self.ast.vec(),
            Some(self.binding_ident(&name)),
            NONE,
            None,
            NONE,
            self.ast.vec(),
            class_body,
            false,
            false,
        ))
    }

    fn generate_enum_variant_element(
        &mut self,
        name: &Ident,
        parameters: &[hir::StructField],
    ) -> ClassElement<'a> {
        self.push_scope();

        let name_str = self.alloc_str(name.as_str());

        let value = if parameters.is_empty() {
            // static Variant = new this;
            self.ast
                .expression_new(SPAN, self.this_expr(), NONE, self.ast.vec())
        } else {
            self.generate_enum_variant_constructor(name_str, parameters)
        };

        self.pop_scope();

        ClassElement::PropertyDefinition(self.ast.alloc_property_definition(
            SPAN,
            PropertyDefinitionType::PropertyDefinition,
            self.ast.vec(),
            PropertyKey::StaticIdentifier(self.ast.alloc_identifier_name(SPAN, name_str)),
            NONE,
            Some(value),
            false,
            true, // static
            false,
            false,
            false,
            false,
            false,
            None,
        ))
    }

    fn generate_enum_variant_constructor(
        &mut self,
        variant_name: &str,
        parameters: &[hir::StructField],
    ) -> Expression<'a> {
        let named_fields = parameters
            .iter()
            .any(|param| !param.name.as_str().chars().all(char::is_numeric));

        let parameter_names: Vec<&'a str> = if named_fields {
            parameters
                .iter()
                .map(|p| self.alloc_str(p.name.as_str()))
                .collect()
        } else {
            parameters
                .iter()
                .map(|p| {
                    let name = self.infer_parameter_name_from_type(&p.ty);
                    self.alloc_str(&name)
                })
                .collect()
        };

        let obj = self.build_enum_variant_object(variant_name, parameters, &parameter_names);
        let new_this = self
            .ast
            .expression_new(SPAN, self.this_expr(), NONE, self.ast.vec());
        let assign_call = self.call_expr(
            self.static_member_expr(self.ident_expr("Object"), "assign"),
            vec![Argument::from(new_this), Argument::from(obj)],
        );

        let formal_params = self.build_enum_variant_params(named_fields, &parameter_names);
        let body = self.fn_body(self.ast.vec1(self.expr_stmt(assign_call)));
        self.ast
            .expression_arrow_function(SPAN, true, false, NONE, formal_params, NONE, body)
    }

    fn build_enum_variant_object(
        &mut self,
        variant_name: &str,
        parameters: &[hir::StructField],
        parameter_names: &[&'a str],
    ) -> Expression<'a> {
        let mut obj_props = Vec::new();

        // tag: this.Variant
        obj_props.push(ObjectPropertyKind::ObjectProperty(
            self.ast.alloc_object_property(
                SPAN,
                PropertyKind::Init,
                PropertyKey::StaticIdentifier(self.ast.alloc_identifier_name(SPAN, "tag")),
                self.static_member_expr(self.this_expr(), variant_name),
                false,
                false,
                false,
            ),
        ));

        for (i, param) in parameters.iter().enumerate() {
            let param_field_name = self.alloc_str(param.name.as_str());
            let parameter_name = parameter_names[i];

            let (key, computed) = if param_field_name.chars().all(char::is_numeric) {
                let num = param_field_name.parse::<f64>().unwrap();
                (
                    PropertyKey::NumericLiteral(self.ast.alloc_numeric_literal(
                        SPAN,
                        num,
                        None,
                        NumberBase::Decimal,
                    )),
                    true,
                )
            } else {
                (
                    PropertyKey::StaticIdentifier(
                        self.ast.alloc_identifier_name(SPAN, param_field_name),
                    ),
                    false,
                )
            };

            let shorthand = parameter_name == param_field_name;
            obj_props.push(ObjectPropertyKind::ObjectProperty(
                self.ast.alloc_object_property(
                    SPAN,
                    PropertyKind::Init,
                    key,
                    self.ident_expr(parameter_name),
                    computed,
                    shorthand,
                    false,
                ),
            ));
        }

        self.ast
            .expression_object(SPAN, self.ast.vec_from_iter(obj_props))
    }

    fn build_enum_variant_params(
        &mut self,
        named_fields: bool,
        parameter_names: &[&'a str],
    ) -> FormalParameters<'a> {
        if named_fields {
            let props: Vec<_> = parameter_names
                .iter()
                .map(|n| {
                    self.ast.binding_property(
                        SPAN,
                        PropertyKey::StaticIdentifier(self.ast.alloc_identifier_name(SPAN, *n)),
                        self.binding_pattern_ident(n),
                        true,
                        false,
                    )
                })
                .collect();

            let obj_pattern =
                self.ast
                    .binding_pattern_object_pattern(SPAN, self.ast.vec_from_iter(props), NONE);

            let param = self.ast.formal_parameter(
                SPAN,
                self.ast.vec(),
                obj_pattern,
                NONE,
                NONE,
                false,
                None,
                false,
                false,
            );

            self.ast.formal_parameters(
                SPAN,
                FormalParameterKind::FormalParameter,
                self.ast.vec1(param),
                NONE,
            )
        } else {
            let params: Vec<_> = parameter_names
                .iter()
                .map(|n| self.formal_param(n))
                .collect();
            self.formal_params(self.ast.vec_from_iter(params))
        }
    }

    fn infer_parameter_name_from_type(&mut self, ty: &hir::Ty) -> String {
        match &ty.kind {
            hir::TyKind::Path(path) => self.current_scope().declare_variable(&path.join("_")),
            _ => self.current_scope().declare_variable("arg"),
        }
    }
}
