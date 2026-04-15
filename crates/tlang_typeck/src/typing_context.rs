use tlang_hir as hir;

fn is_unspecialized_generic_builtin_path(path: &hir::Path) -> bool {
    matches!(
        path.first_ident().as_str(),
        "List" | "Dict" | "Option" | "Result"
    )
}

fn has_concrete_type(ty: &hir::Ty) -> bool {
    fn contains_var(kind: &hir::TyKind) -> bool {
        match kind {
            hir::TyKind::Var(_) => true,
            hir::TyKind::Fn(params, ret) => {
                params.iter().any(|param| contains_var(&param.kind)) || contains_var(&ret.kind)
            }
            hir::TyKind::Slice(inner) => contains_var(&inner.kind),
            hir::TyKind::Dict(key, value) => contains_var(&key.kind) || contains_var(&value.kind),
            hir::TyKind::Union(types) => types.iter().any(|ty| contains_var(&ty.kind)),
            hir::TyKind::Unknown | hir::TyKind::Primitive(_) | hir::TyKind::Never => false,
            hir::TyKind::Path(path) => is_unspecialized_generic_builtin_path(path),
        }
    }

    !matches!(ty.kind, hir::TyKind::Unknown)
        && !contains_var(&ty.kind)
        && !matches!(
            &ty.kind,
            hir::TyKind::Path(path) if is_unspecialized_generic_builtin_path(path)
        )
}

/// Whether `unknown` operands produce errors or propagate silently.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum TypingContext {
    /// Fully typed function: all params **and** return type annotated.
    /// `unknown` operands in operators are errors.
    Strict,
    /// Top-level code or function with missing annotations.
    /// `unknown` propagates silently through operators.
    #[default]
    Permissive,
}

impl TypingContext {
    pub fn is_strict(self) -> bool {
        self == TypingContext::Strict
    }

    /// Determine the typing context for a function declaration.
    ///
    /// A function is strict only when every parameter and the return type are
    /// fully concrete. Unresolved generic vars still belong to permissive mode.
    pub fn for_function(decl: &hir::FunctionDeclaration) -> Self {
        let all_params_typed = decl
            .parameters
            .iter()
            .all(|p| has_concrete_type(&p.type_annotation));
        let return_typed = has_concrete_type(&decl.return_type);

        if all_params_typed && return_typed {
            TypingContext::Strict
        } else {
            TypingContext::Permissive
        }
    }

    /// Determine the context for a closure, inheriting from its enclosing
    /// context when the closure itself is not fully annotated.
    pub fn for_closure(decl: &hir::FunctionDeclaration, enclosing: TypingContext) -> Self {
        let own = Self::for_function(decl);
        if own.is_strict() {
            TypingContext::Strict
        } else {
            enclosing
        }
    }
}
