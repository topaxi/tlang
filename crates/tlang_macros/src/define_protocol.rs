use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::parse::{Parse, ParseStream};
use syn::{Ident, Token, braced, parenthesized};

struct ProtocolMethod {
    name: Ident,
    params: Vec<Ident>,
    arity: u16,
    /// Method-level type parameter names, e.g. `["U"]` for `fn map<U>(this, f)`.
    type_params: Vec<Ident>,
    /// Optional default implementation body (raw token stream between braces).
    default_body: Option<proc_macro2::TokenStream>,
}

struct AssociatedTypeDecl {
    name: Ident,
}

struct Protocol {
    name: Ident,
    /// Protocol-level type parameter names, e.g. `T` in `Functor<T>`.
    type_params: Vec<Ident>,
    /// Associated type declarations, e.g. `type Wrapped`.
    associated_types: Vec<AssociatedTypeDecl>,
    methods: Vec<ProtocolMethod>,
}

struct DefineProtocolInput {
    protocols: Vec<Protocol>,
}

impl Parse for DefineProtocolInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut protocols = Vec::new();

        while !input.is_empty() {
            let name: Ident = input.parse()?;

            // Parse optional type parameters: `<T, U>`
            let type_params = if input.peek(Token![<]) {
                input.parse::<Token![<]>()?;
                let params =
                    syn::punctuated::Punctuated::<Ident, Token![,]>::parse_separated_nonempty(
                        input,
                    )?;
                input.parse::<Token![>]>()?;
                params.into_iter().collect()
            } else {
                Vec::new()
            };

            let methods_content;
            braced!(methods_content in input);

            let mut associated_types = Vec::new();
            let mut methods = Vec::new();

            while !methods_content.is_empty() {
                // Check for associated type declaration: `type Name;`
                if methods_content.peek(Token![type]) {
                    methods_content.parse::<Token![type]>()?;
                    let assoc_name: Ident = methods_content.parse()?;
                    // Allow optional trailing semicolon
                    if methods_content.peek(Token![;]) {
                        methods_content.parse::<Token![;]>()?;
                    }
                    associated_types.push(AssociatedTypeDecl { name: assoc_name });
                    continue;
                }

                methods_content.parse::<Token![fn]>()?;
                let method_name: Ident = methods_content.parse()?;

                // Parse optional method-level type parameters: `<U>`
                let method_type_params = if methods_content.peek(Token![<]) {
                    methods_content.parse::<Token![<]>()?;
                    let params = syn::punctuated::Punctuated::<Ident, Token![,]>::parse_separated_nonempty(&methods_content)?;
                    methods_content.parse::<Token![>]>()?;
                    params.into_iter().collect()
                } else {
                    Vec::new()
                };

                let params_content;
                parenthesized!(params_content in methods_content);
                let params = params_content.parse_terminated(Ident::parse, Token![,])?;
                let arity = params.len() as u16;
                let params: Vec<Ident> = params.into_iter().collect();

                // Parse optional default body { ... } or trailing semicolon
                let default_body = if methods_content.peek(syn::token::Brace) {
                    let body_content;
                    braced!(body_content in methods_content);
                    Some(body_content.parse::<proc_macro2::TokenStream>()?)
                } else {
                    // Allow trailing semicolon
                    if methods_content.peek(Token![;]) {
                        methods_content.parse::<Token![;]>()?;
                    }
                    None
                };

                methods.push(ProtocolMethod {
                    name: method_name,
                    params,
                    arity,
                    type_params: method_type_params,
                    default_body,
                });
            }

            protocols.push(Protocol {
                name,
                type_params,
                associated_types,
                methods,
            });
        }

        Ok(DefineProtocolInput { protocols })
    }
}

pub(crate) fn generate_define_protocol(input: TokenStream) -> TokenStream {
    let def = syn::parse_macro_input!(input as DefineProtocolInput);

    let mut submits = Vec::new();
    let mut default_impls = Vec::new();

    for protocol in &def.protocols {
        let protocol_name_str = protocol.name.to_string();

        let method_entries: Vec<_> = protocol
            .methods
            .iter()
            .map(|m| {
                let name_str = m.name.to_string();
                let arity = m.arity;
                quote! { (#name_str, #arity) }
            })
            .collect();

        let type_param_entries: Vec<_> = protocol
            .type_params
            .iter()
            .map(|tp| {
                let tp_str = tp.to_string();
                quote! { #tp_str }
            })
            .collect();

        let assoc_type_entries: Vec<_> = protocol
            .associated_types
            .iter()
            .map(|at| {
                let at_str = at.name.to_string();
                quote! { #at_str }
            })
            .collect();

        if type_param_entries.is_empty() && assoc_type_entries.is_empty() {
            submits.push(quote! {
                inventory::submit! {
                    crate::NativeProtocolDef::new(
                        #protocol_name_str,
                        &[#(#method_entries),*],
                    )
                }
            });
        } else {
            submits.push(quote! {
                inventory::submit! {
                    crate::NativeProtocolDef::with_metadata(
                        #protocol_name_str,
                        &[#(#method_entries),*],
                        &[#(#type_param_entries),*],
                        &[#(#assoc_type_entries),*],
                    )
                }
            });
        }

        // Generate default impl wrappers for methods with bodies
        for method in &protocol.methods {
            if let Some(body) = &method.default_body {
                let method_name_str = method.name.to_string();
                let arity = method.arity as usize;

                // Generate unique fn names
                let wrapper_fn_name = format_ident!(
                    "__protocol_default_{}_{}",
                    protocol_name_str.to_lowercase(),
                    method_name_str
                );
                let native_fn_name = format_ident!(
                    "__protocol_default_{}_{}_native",
                    protocol_name_str.to_lowercase(),
                    method_name_str
                );

                // The first param is conventionally `this` or `self`; the rest
                // are regular params.  Inside the body the user writes bare
                // idents (e.g. `vm.stringify(this)`).  We bind `vm` to the
                // VMState ref and each param ident to the corresponding arg
                // slot.
                let param_bindings: Vec<_> = method
                    .params
                    .iter()
                    .enumerate()
                    .map(|(i, p)| {
                        quote! { let #p = args[#i]; }
                    })
                    .collect();

                default_impls.push(quote! {
                    fn #wrapper_fn_name(
                        vm: &mut tlang_memory::VMState,
                        args: &[tlang_memory::TlangValue],
                    ) -> tlang_memory::TlangValue {
                        #(#param_bindings)*
                        #body
                    }

                    fn #native_fn_name(
                        state: &mut tlang_memory::VMState,
                        args: &[tlang_memory::TlangValue],
                    ) -> tlang_memory::NativeFnReturn {
                        if args.len() != #arity {
                            state.panic(format!(
                                "Expected {} arguments, got {}",
                                #arity,
                                args.len()
                            ));
                        }
                        #wrapper_fn_name(state, args).into()
                    }

                    inventory::submit! {
                        crate::NativeProtocolImplDef::new(
                            #protocol_name_str,
                            "*",
                            #method_name_str,
                            #native_fn_name,
                            0,
                        )
                    }
                });
            }
        }
    }

    let generated = quote! {
        #(#submits)*
        #(#default_impls)*
    };

    TokenStream::from(generated)
}
