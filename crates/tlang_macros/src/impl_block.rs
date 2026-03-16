use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::parse::{Parse, ParseStream};
use syn::{Ident, Token, braced, parenthesized};

pub(crate) struct ImplMethod {
    pub name: Ident,
    pub params: Vec<Ident>, // first param is `this`
    pub body: TokenStream2,
}

pub(crate) struct ImplBlock {
    pub protocol: Option<Ident>,
    #[allow(dead_code)]
    pub type_name: Ident,
    pub methods: Vec<ImplMethod>,
}

/// Parse zero or more `impl [Protocol for] TypeName { ... }` blocks that follow
/// the type definition body. `kind` is used in error messages ("enum" or "struct").
pub(crate) fn parse_impl_blocks(
    input: ParseStream,
    type_name: &Ident,
    kind: &str,
) -> syn::Result<Vec<ImplBlock>> {
    let mut blocks = Vec::new();

    while input.peek(Token![impl]) {
        input.parse::<Token![impl]>()?;

        let first_ident: Ident = input.parse()?;

        let (protocol, impl_type_name) = if input.peek(Token![for]) {
            input.parse::<Token![for]>()?;
            let impl_type: Ident = input.parse()?;
            (Some(first_ident), impl_type)
        } else {
            (None, first_ident)
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

    Ok(ImplMethod { name, params, body })
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

            let type_prefix = type_name_str.to_lowercase();

            let native_wrapper_name = if let Some(protocol) = &impl_block.protocol {
                let protocol_str = protocol.to_string().to_lowercase();
                format_ident!("{type_prefix}_{protocol_str}_{method_name}")
            } else {
                format_ident!("{type_prefix}_{method_name}")
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

                let wrapper_name = format_ident!("{native_wrapper_name}_protocol_impl");

                method_fns.push(quote! {
                    fn #native_wrapper_name(
                        vm: &mut tlang_memory::VMState,
                        this: tlang_memory::TlangValue,
                        #(#extra_params: tlang_memory::TlangValue),*
                    ) -> tlang_memory::TlangValue {
                        #body
                    }

                    pub fn #wrapper_name(
                        state: &mut tlang_memory::VMState,
                        args: &[tlang_memory::TlangValue],
                    ) -> tlang_memory::NativeFnReturn {
                        if args.len() != #arity {
                            state.panic(format!("Expected {} arguments, got {}", #arity, args.len()));
                        }
                        let this = args[0];
                        #(#arg_extractions)*
                        #native_wrapper_name(state, this, #(#extra_params),*).into()
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
                });
            } else {
                let arg_extractions: Vec<_> = extra_params
                    .iter()
                    .enumerate()
                    .map(|(i, ident)| {
                        let idx = i + 1; // args[0] is `this`
                        quote! { let #ident = args[#idx]; }
                    })
                    .collect();

                let wrapper_name = format_ident!("{native_wrapper_name}_native_method");

                method_fns.push(quote! {
                    fn #native_wrapper_name(
                        vm: &mut tlang_memory::VMState,
                        this: tlang_memory::TlangValue,
                        #(#extra_params: tlang_memory::TlangValue),*
                    ) -> tlang_memory::TlangValue {
                        #body
                    }

                    pub fn #wrapper_name(
                        state: &mut tlang_memory::VMState,
                        args: &[tlang_memory::TlangValue],
                    ) -> tlang_memory::NativeFnReturn {
                        let this = args[0];
                        #(#arg_extractions)*
                        #native_wrapper_name(state, this, #(#extra_params),*).into()
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
