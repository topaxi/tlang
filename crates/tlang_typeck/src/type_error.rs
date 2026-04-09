use tlang_diagnostics::{Diagnostic, Severity};
use tlang_span::Span;

/// A type-checking error with source location and diagnostic information.
///
/// `TypeError` provides structured error information that can be converted
/// into the project's `Diagnostic` type for unified reporting.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    /// A type mismatch between expected and actual types.
    TypeMismatch {
        expected: String,
        actual: String,
        span: Span,
    },
    /// An attempted cast on an `unknown` type, which is not permitted.
    CastOnUnknown { span: Span },
    /// No `From` implementation exists for an infallible `as` cast.
    NoFromImpl {
        from_ty: String,
        to_ty: String,
        span: Span,
    },
    /// Cannot apply the given binary operator to the provided operand types.
    InvalidBinaryOp {
        op: String,
        lhs: String,
        rhs: String,
        span: Span,
    },
    /// Cannot apply the given unary operator to the provided operand type.
    InvalidUnaryOp {
        op: String,
        operand: String,
        span: Span,
    },
    /// `unknown` value used in a strict (fully typed) context.
    UnknownInStrictContext { op: String, span: Span },
    /// Type mismatch in a let/const binding with an explicit type annotation.
    BindingTypeMismatch {
        declared: String,
        actual: String,
        span: Span,
    },
    /// Wrong number of arguments in a function call.
    ArgumentCountMismatch {
        expected: usize,
        actual: usize,
        span: Span,
    },
    /// Argument type does not match the corresponding parameter type.
    ArgumentTypeMismatch {
        param_name: String,
        expected: String,
        actual: String,
        span: Span,
    },
    /// Return type does not match the function's declared/inferred return type.
    ReturnTypeMismatch {
        expected: String,
        actual: String,
        span: Span,
    },
    /// Field does not exist on the struct type.
    UnknownField {
        field: String,
        type_name: String,
        available: Vec<String>,
        span: Span,
    },
    /// A required protocol method is missing from an impl block.
    MissingProtocolMethod {
        method: String,
        protocol: String,
        target_type: String,
        span: Span,
    },
    /// A constraint protocol is not implemented for the target type.
    MissingConstraintImpl {
        constraint: String,
        protocol: String,
        target_type: String,
        span: Span,
    },
}

impl TypeError {
    /// The primary span associated with this error.
    pub fn span(&self) -> Span {
        match self {
            TypeError::TypeMismatch { span, .. }
            | TypeError::CastOnUnknown { span }
            | TypeError::NoFromImpl { span, .. }
            | TypeError::InvalidBinaryOp { span, .. }
            | TypeError::InvalidUnaryOp { span, .. }
            | TypeError::UnknownInStrictContext { span, .. }
            | TypeError::BindingTypeMismatch { span, .. }
            | TypeError::ArgumentCountMismatch { span, .. }
            | TypeError::ArgumentTypeMismatch { span, .. }
            | TypeError::ReturnTypeMismatch { span, .. }
            | TypeError::UnknownField { span, .. }
            | TypeError::MissingProtocolMethod { span, .. }
            | TypeError::MissingConstraintImpl { span, .. } => *span,
        }
    }

    /// A human-readable message describing the error.
    pub fn message(&self) -> String {
        match self {
            TypeError::TypeMismatch {
                expected, actual, ..
            } => format!("type mismatch: expected `{expected}`, found `{actual}`"),
            TypeError::CastOnUnknown { .. } => {
                "cannot use `as` cast on `unknown` type; use `as?` instead".to_string()
            }
            TypeError::NoFromImpl { from_ty, to_ty, .. } => {
                format!("no `From<{from_ty}>` implementation for `{to_ty}`")
            }
            TypeError::InvalidBinaryOp { op, lhs, rhs, .. } => {
                format!("cannot apply operator `{op}` to types `{lhs}` and `{rhs}`")
            }
            TypeError::InvalidUnaryOp { op, operand, .. } => {
                format!("cannot apply operator `{op}` to type `{operand}`")
            }
            TypeError::UnknownInStrictContext { op, .. } => {
                format!(
                    "cannot use `unknown` value in operator `{op}` in fully typed function \
                     — convert with `as?` first"
                )
            }
            TypeError::BindingTypeMismatch {
                declared, actual, ..
            } => {
                format!(
                    "type mismatch in binding: declared `{declared}`, but expression has type `{actual}`"
                )
            }
            TypeError::ArgumentCountMismatch {
                expected, actual, ..
            } => {
                format!("expected {expected} arguments, found {actual}")
            }
            TypeError::ArgumentTypeMismatch {
                param_name,
                expected,
                actual,
                ..
            } => {
                format!(
                    "argument type mismatch: parameter `{param_name}` expects `{expected}`, got `{actual}`"
                )
            }
            TypeError::ReturnTypeMismatch {
                expected, actual, ..
            } => {
                format!(
                    "return type mismatch: function declares `→ {expected}`, but returns `{actual}`"
                )
            }
            TypeError::UnknownField {
                field,
                type_name,
                available,
                ..
            } => {
                format!(
                    "unknown field `{field}` on struct `{type_name}`; available fields: {}",
                    available.join(", ")
                )
            }
            TypeError::MissingProtocolMethod {
                method,
                protocol,
                target_type,
                ..
            } => {
                format!("missing method `{method}` in impl `{protocol}` for `{target_type}`")
            }
            TypeError::MissingConstraintImpl {
                constraint,
                protocol,
                target_type,
                ..
            } => {
                format!(
                    "missing constraint: `{protocol}` requires `{constraint}` to be implemented for `{target_type}`"
                )
            }
        }
    }
}

impl From<&TypeError> for Diagnostic {
    fn from(err: &TypeError) -> Self {
        Diagnostic::new(Severity::Error, err.message(), err.span())
    }
}

impl From<TypeError> for Diagnostic {
    fn from(err: TypeError) -> Self {
        Diagnostic::from(&err)
    }
}
