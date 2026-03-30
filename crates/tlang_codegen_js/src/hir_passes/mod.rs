pub mod boolean_return_simplification;
pub mod js_anf_return_opt;
pub mod js_anf_transform;
pub mod tail_call_self_reference_validation;

pub use boolean_return_simplification::BooleanReturnSimplification;
pub use js_anf_return_opt::JsAnfReturnOpt;
pub use js_anf_transform::JsAnfTransform;
pub use tail_call_self_reference_validation::TailCallSelfReferenceValidation;
