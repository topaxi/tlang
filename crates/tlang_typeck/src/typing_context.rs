use tlang_hir as hir;

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
    /// A function is strict if **all** parameters have non-`unknown` type
    /// annotations **and** the return type is non-`unknown`.
    pub fn for_function(decl: &hir::FunctionDeclaration) -> Self {
        let all_params_typed = decl
            .parameters
            .iter()
            .all(|p| !matches!(p.type_annotation.kind, hir::TyKind::Unknown));
        let return_typed = !matches!(decl.return_type.kind, hir::TyKind::Unknown);

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
