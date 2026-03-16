use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::parse::{Parse, ParseStream};
use syn::{Ident, Token, braced, parenthesized};

use crate::impl_block::{ImplBlock, generate_impl_methods, parse_impl_blocks};

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

struct DefineEnumInput {
    name: Ident,
    variants: Vec<EnumVariant>,
    impl_blocks: Vec<ImplBlock>,
}

impl Parse for DefineEnumInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
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
            if variants_content.peek(Token![,]) {
                variants_content.parse::<Token![,]>()?;
            }
        }

        let impl_blocks = parse_impl_blocks(input, &name, "enum")?;

        Ok(DefineEnumInput {
            name,
            variants,
            impl_blocks,
        })
    }
}

pub(crate) fn generate_define_enum(input: TokenStream) -> TokenStream {
    let def = syn::parse_macro_input!(input as DefineEnumInput);

    let enum_name = &def.name;
    let enum_name_str = enum_name.to_string();
    let enum_name_upper = enum_name_str.to_uppercase();

    let mut const_defs = Vec::new();
    let mut variant_descriptors = Vec::new();

    for (i, variant) in def.variants.iter().enumerate() {
        let variant_name = &variant.name;
        let variant_name_str = variant_name.to_string();
        let const_name = format_ident!(
            "{}_{}",
            enum_name_upper,
            variant_name.to_string().to_uppercase()
        );

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

    let method_fns = generate_impl_methods(&enum_name_str, &def.impl_blocks);

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
