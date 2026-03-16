use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::parse::{Parse, ParseStream};
use syn::{Ident, Token, braced, parenthesized};

/// Represents a variant field list like `(value)` or `(head, tail)`.
struct VariantFields(Vec<Ident>);

impl Parse for VariantFields {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(syn::token::Paren) {
            let content;
            parenthesized!(content in input);
            let fields = content.parse_terminated(Ident::parse, Token![,])?;
            Ok(VariantFields(fields.into_iter().collect()))
        } else {
            Ok(VariantFields(Vec::new()))
        }
    }
}

struct EnumVariant {
    name: Ident,
    fields: Vec<Ident>,
}

struct ImplMethod {
    name: Ident,
    params: Vec<Ident>, // includes `this`
    body: TokenStream2,
}

struct ImplBlock {
    protocol: Option<Ident>,
    #[allow(dead_code)]
    type_name: Ident,
    methods: Vec<ImplMethod>,
}

struct DefineEnumInput {
    name: Ident,
    variants: Vec<EnumVariant>,
    impl_blocks: Vec<ImplBlock>,
}

impl Parse for DefineEnumInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Parse: enum Name { Variant1(field1), Variant2, ... }
        input.parse::<Token![enum]>()?;
        let name: Ident = input.parse()?;

        let variants_content;
        braced!(variants_content in input);

        let mut variants = Vec::new();
        while !variants_content.is_empty() {
            let variant_name: Ident = variants_content.parse()?;
            let fields = variants_content.parse::<VariantFields>()?;
            variants.push(EnumVariant {
                name: variant_name,
                fields: fields.0,
            });
            // Allow trailing comma
            if variants_content.peek(Token![,]) {
                variants_content.parse::<Token![,]>()?;
            }
        }

        // Parse optional impl blocks
        let impl_blocks = parse_impl_blocks(input, &name)?;

        Ok(DefineEnumInput {
            name,
            variants,
            impl_blocks,
        })
    }
}

fn parse_impl_blocks(input: ParseStream, type_name: &Ident) -> syn::Result<Vec<ImplBlock>> {
    let mut blocks = Vec::new();

    while input.peek(Token![impl]) {
        input.parse::<Token![impl]>()?;

        let first_ident: Ident = input.parse()?;

        // Check if this is `impl Protocol for Type` or `impl Type`
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
                    "impl block type `{}` does not match enum name `{}`",
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

    // Optional return type (-> TlangValue or -> NativeFnReturn)
    if input.peek(Token![->]) {
        input.parse::<Token![->]>()?;
        // Consume the return type path
        input.parse::<syn::Type>()?;
    }

    // Parse body block
    let body_content;
    braced!(body_content in input);
    let body: TokenStream2 = body_content.parse()?;

    Ok(ImplMethod { name, params, body })
}

pub(crate) fn generate_define_enum(input: TokenStream) -> TokenStream {
    let def = syn::parse_macro_input!(input as DefineEnumInput);

    let enum_name = &def.name;
    let enum_name_str = enum_name.to_string();
    let enum_name_upper = enum_name_str.to_uppercase();

    // Generate variant constant definitions and variant descriptors
    let mut const_defs = Vec::new();
    let mut variant_descriptors = Vec::new();

    for (i, variant) in def.variants.iter().enumerate() {
        let variant_name = &variant.name;
        let variant_name_str = variant_name.to_string();
        let variant_name_upper = variant_name_str.to_uppercase();
        let const_name = format_ident!("{}_{}", enum_name_upper, variant_name_upper);

        const_defs.push(quote! {
            pub const #const_name: usize = #i;
        });

        let field_strs: Vec<String> = variant.fields.iter().map(|f| f.to_string()).collect();

        variant_descriptors.push(quote! {
            tlang_memory::NativeEnumVariantDef {
                name: #variant_name_str,
                fields: &[#(#field_strs),*],
            }
        });
    }

    // Generate impl block expansions
    let mut method_fns = Vec::new();

    for impl_block in &def.impl_blocks {
        for method in &impl_block.methods {
            let method_name = &method.name;
            let method_name_str = method_name.to_string();
            let body = &method.body;

            // Build parameter list: skip `this` (first param), rest are TlangValue args
            let extra_params: Vec<&Ident> = method
                .params
                .iter()
                .skip(1) // skip `this`
                .collect();

            let native_wrapper_name = if let Some(protocol) = &impl_block.protocol {
                let protocol_str = protocol.to_string().to_lowercase();
                format_ident!(
                    "{}_{}_{}",
                    enum_name_str.to_lowercase(),
                    protocol_str,
                    method_name
                )
            } else {
                format_ident!("{}_{}", enum_name_str.to_lowercase(), method_name)
            };

            let type_name_str = &enum_name_str;

            if let Some(protocol) = &impl_block.protocol {
                // Protocol impl: fn(vm, this, args...) -> NativeFnReturn
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
                // Regular method: fn(vm, this, args...) -> NativeFnReturn
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
        #(#const_defs)*

        inventory::submit! {
            crate::NativeEnumDef::new(
                #enum_name_str,
                &[#(#variant_descriptors),*],
            )
        }

        #(#method_fns)*
    };

    TokenStream::from(generated)
}
