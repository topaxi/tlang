use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::parse::{Parse, ParseStream};
use syn::{Ident, Token, braced, parenthesized};

struct ImplMethod {
    name: Ident,
    params: Vec<Ident>,
    body: TokenStream2,
}

struct ImplBlock {
    protocol: Option<Ident>,
    #[allow(dead_code)]
    type_name: Ident,
    methods: Vec<ImplMethod>,
}

struct DefineStructInput {
    name: Ident,
    fields: Vec<Ident>,
    impl_blocks: Vec<ImplBlock>,
}

impl Parse for DefineStructInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Parse: struct Name { field1, field2, ... }
        input.parse::<Token![struct]>()?;
        let name: Ident = input.parse()?;

        let fields_content;
        braced!(fields_content in input);

        let fields = fields_content.parse_terminated(Ident::parse, Token![,])?;
        let fields: Vec<Ident> = fields.into_iter().collect();

        // Parse optional impl blocks
        let impl_blocks = parse_impl_blocks(input, &name)?;

        Ok(DefineStructInput {
            name,
            fields,
            impl_blocks,
        })
    }
}

fn parse_impl_blocks(input: ParseStream, type_name: &Ident) -> syn::Result<Vec<ImplBlock>> {
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
                    "impl block type `{}` does not match struct name `{}`",
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

fn parse_impl_method(input: ParseStream) -> syn::Result<ImplMethod> {
    input.parse::<Token![fn]>()?;
    let name: Ident = input.parse()?;

    let params_content;
    parenthesized!(params_content in input);
    let params = params_content.parse_terminated(Ident::parse, Token![,])?;
    let params: Vec<Ident> = params.into_iter().collect();

    // Optional return type
    if input.peek(Token![->]) {
        input.parse::<Token![->]>()?;
        input.parse::<syn::Type>()?;
    }

    let body_content;
    braced!(body_content in input);
    let body: TokenStream2 = body_content.parse()?;

    Ok(ImplMethod { name, params, body })
}

pub(crate) fn generate_define_struct(input: TokenStream) -> TokenStream {
    let def = syn::parse_macro_input!(input as DefineStructInput);

    let struct_name = &def.name;
    let struct_name_str = struct_name.to_string();

    let field_strs: Vec<String> = def.fields.iter().map(|f| f.to_string()).collect();

    // Generate impl block expansions (same pattern as define_enum)
    let mut method_fns = Vec::new();

    for impl_block in &def.impl_blocks {
        for method in &impl_block.methods {
            let method_name = &method.name;
            let method_name_str = method_name.to_string();
            let body = &method.body;

            let extra_params: Vec<&Ident> = method.params.iter().skip(1).collect();

            let native_wrapper_name = if let Some(protocol) = &impl_block.protocol {
                let protocol_str = protocol.to_string().to_lowercase();
                format_ident!(
                    "{}_{}_{}",
                    struct_name_str.to_lowercase(),
                    protocol_str,
                    method_name
                )
            } else {
                format_ident!("{}_{}", struct_name_str.to_lowercase(), method_name)
            };

            let type_name_str = &struct_name_str;

            if let Some(protocol) = &impl_block.protocol {
                let protocol_str = protocol.to_string();
                let arity = method.params.len();

                let arg_extractions: Vec<_> = extra_params
                    .iter()
                    .enumerate()
                    .map(|(i, ident)| {
                        let idx = i + 1;
                        quote! { let #ident = args[#idx]; }
                    })
                    .collect();

                let wrapper_name = format_ident!("{}_protocol_impl", native_wrapper_name);

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
                        let idx = i + 1;
                        quote! { let #ident = args[#idx]; }
                    })
                    .collect();

                let wrapper_name = format_ident!("{}_native_method", native_wrapper_name);

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

    let generated = quote! {
        inventory::submit! {
            crate::NativeStructDef::new(
                #struct_name_str,
                &[#(#field_strs),*],
            )
        }

        #(#method_fns)*
    };

    TokenStream::from(generated)
}
