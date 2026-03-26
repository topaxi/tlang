use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::parse::{Parse, ParseStream};
use syn::{Ident, Token, braced, parenthesized};

/// A possibly-dotted name, e.g. `Foo` or `Temporal.Duration`.
///
/// Parsed from `Ident ( '.' Ident )*` token sequences.  A single `Ident` is
/// represented as a one-segment `DottedName` so existing (non-dotted) call
/// sites remain source-compatible.
#[derive(Clone)]
pub(crate) struct DottedName {
    pub segments: Vec<Ident>,
}

impl DottedName {
    /// Dotted string used for inventory registration (e.g. `"Temporal.Duration"`).
    pub fn to_dotted_string(&self) -> String {
        self.segments
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
            .join(".")
    }

    /// Underscore-joined, lowercased string safe for Rust identifiers
    /// (e.g. `"temporal_duration"`).
    #[allow(dead_code)]
    pub fn to_ident_prefix(&self) -> String {
        self.segments
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
            .join("_")
            .to_lowercase()
    }

    pub fn span(&self) -> proc_macro2::Span {
        self.segments
            .first()
            .expect("DottedName has at least one segment")
            .span()
    }
}

impl Parse for DottedName {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let first: Ident = input.parse()?;
        let mut segments = vec![first];
        while input.peek(Token![.]) {
            input.parse::<Token![.]>()?;
            segments.push(input.parse()?);
        }
        Ok(DottedName { segments })
    }
}

impl PartialEq for DottedName {
    fn eq(&self, other: &Self) -> bool {
        self.to_dotted_string() == other.to_dotted_string()
    }
}

impl std::fmt::Display for DottedName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_dotted_string())
    }
}

impl From<Ident> for DottedName {
    fn from(ident: Ident) -> Self {
        DottedName {
            segments: vec![ident],
        }
    }
}

pub(crate) struct ImplMethod {
    pub name: Ident,
    pub params: Vec<Ident>, // first param is `this`
    pub body: TokenStream2,
    /// When `true` inside a protocol impl block, also registers the method
    /// as a direct `NativeMethodDef` on the type (mirrors tlang's `apply`).
    pub apply: bool,
}

pub(crate) struct ImplBlock {
    pub protocol: Option<Ident>,
    #[allow(dead_code)]
    pub type_name: DottedName,
    pub methods: Vec<ImplMethod>,
}

/// Parse zero or more `impl [Protocol for] TypeName { ... }` blocks that follow
/// the type definition body. `kind` is used in error messages ("enum" or "struct").
pub(crate) fn parse_impl_blocks(
    input: ParseStream,
    type_name: &DottedName,
    kind: &str,
) -> syn::Result<Vec<ImplBlock>> {
    let mut blocks = Vec::new();

    while input.peek(Token![impl]) {
        input.parse::<Token![impl]>()?;

        // The first thing after `impl` can be a (possibly-dotted) type name
        // or a protocol name followed by `for`.
        let first_name: DottedName = input.parse()?;

        let (protocol, impl_type_name) = if input.peek(Token![for]) {
            // `impl Protocol for Type.Name { ... }`
            if first_name.segments.len() != 1 {
                return Err(syn::Error::new(
                    first_name.span(),
                    "protocol name must be a single identifier",
                ));
            }
            input.parse::<Token![for]>()?;
            let impl_type: DottedName = input.parse()?;
            (Some(first_name.segments[0].clone()), impl_type)
        } else {
            (None, first_name)
        };

        if impl_type_name != *type_name {
            return Err(syn::Error::new(
                impl_type_name.span(),
                format!(
                    "impl block type `{}` does not match {kind} name `{}`",
                    impl_type_name, type_name
                ),
            ));
        }

        let methods_content;
        braced!(methods_content in input);

        let mut methods = Vec::new();
        while !methods_content.is_empty() {
            methods.push(parse_impl_method(&methods_content)?);
        }

        blocks.push(ImplBlock {
            protocol,
            type_name: impl_type_name,
            methods,
        });
    }

    Ok(blocks)
}

pub(crate) fn parse_impl_method(input: ParseStream) -> syn::Result<ImplMethod> {
    // Optional `apply` keyword: `apply fn method(...)` registers the method
    // both as a protocol impl and as a direct shape method.
    let apply = if input.peek(Ident) && input.fork().parse::<Ident>()? == "apply" {
        input.parse::<Ident>()?; // consume `apply`
        true
    } else {
        false
    };
    input.parse::<Token![fn]>()?;
    let name: Ident = input.parse()?;

    let params_content;
    parenthesized!(params_content in input);
    let params = params_content.parse_terminated(Ident::parse, Token![,])?;
    let params: Vec<Ident> = params.into_iter().collect();

    // Optional return type annotation (ignored — body controls return type).
    if input.peek(Token![->]) {
        input.parse::<Token![->]>()?;
        input.parse::<syn::Type>()?;
    }

    let body_content;
    braced!(body_content in input);
    let body: TokenStream2 = body_content.parse()?;

    Ok(ImplMethod {
        name,
        params,
        body,
        apply,
    })
}

/// Generate `pub fn` wrappers + `inventory::submit!` calls for all impl blocks.
/// `type_name_str` is the registered type name (e.g. `"Option"`, `"List"`).
pub(crate) fn generate_impl_methods(
    type_name_str: &str,
    impl_blocks: &[ImplBlock],
) -> Vec<TokenStream2> {
    let mut method_fns = Vec::new();

    for impl_block in impl_blocks {
        for method in &impl_block.methods {
            let method_name = &method.name;
            let method_name_str = method_name.to_string();
            let body = &method.body;

            // Extra params = everything after `this`.
            let extra_params: Vec<&Ident> = method.params.iter().skip(1).collect();

            // Replace dots with underscores so the wrapper is a valid Rust ident.
            let type_prefix = type_name_str.replace('.', "_").to_lowercase();

            let wrapper_name = if let Some(protocol) = &impl_block.protocol {
                let protocol_str = protocol.to_string().to_lowercase();
                format_ident!("{type_prefix}_{protocol_str}_{method_name}_wrapper")
            } else {
                format_ident!("{type_prefix}_{method_name}_wrapper")
            };

            if let Some(protocol) = &impl_block.protocol {
                let protocol_str = protocol.to_string();
                let arity = method.params.len(); // includes `this`

                let arg_extractions: Vec<_> = extra_params
                    .iter()
                    .enumerate()
                    .map(|(i, ident)| {
                        let idx = i + 1; // args[0] is `this`
                        quote! { let #ident = args[#idx]; }
                    })
                    .collect();

                // When `apply` is set, also register as a direct shape method
                // using the same wrapper (mirrors tlang's `apply` keyword).
                let apply_method_submit = if method.apply {
                    quote! {
                        inventory::submit! {
                            crate::NativeMethodDef::new(
                                #type_name_str,
                                #method_name_str,
                                #wrapper_name,
                                0,
                            )
                        }
                    }
                } else {
                    quote! {}
                };

                method_fns.push(quote! {
                    pub fn #wrapper_name(
                        state: &mut tlang_memory::VMState,
                        args: &[tlang_memory::TlangValue],
                    ) -> tlang_memory::NativeFnReturn {
                        if args.len() != #arity {
                            state.panic(format!("Expected {} arguments, got {}", #arity, args.len()));
                        }
                        let this = args[0];
                        #(#arg_extractions)*
                        let vm = state;
                        (|| -> tlang_memory::TlangValue { #body })().into()
                    }

                    inventory::submit! {
                        crate::NativeProtocolImplDef::new(
                            #protocol_str,
                            #type_name_str,
                            #method_name_str,
                            #wrapper_name,
                            0,
                        )
                    }

                    #apply_method_submit
                });
            } else {
                let arg_extractions: Vec<_> = extra_params
                    .iter()
                    .enumerate()
                    .map(|(i, ident)| {
                        let idx = i + 1; // args[0] is `this`
                        // Use .get() so trailing args can be omitted (treated as Nil).
                        quote! { let #ident = args.get(#idx).copied().unwrap_or(tlang_memory::TlangValue::Nil); }
                    })
                    .collect();

                method_fns.push(quote! {
                    pub fn #wrapper_name(
                        state: &mut tlang_memory::VMState,
                        args: &[tlang_memory::TlangValue],
                    ) -> tlang_memory::NativeFnReturn {
                        let this = args[0];
                        #(#arg_extractions)*
                        let vm = state;
                        (|| -> tlang_memory::TlangValue { #body })().into()
                    }

                    inventory::submit! {
                        crate::NativeMethodDef::new(
                            #type_name_str,
                            #method_name_str,
                            #wrapper_name,
                            0,
                        )
                    }
                });
            }
        }
    }

    method_fns
}
