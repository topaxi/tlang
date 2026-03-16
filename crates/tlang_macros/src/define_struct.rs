use proc_macro::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::{Ident, Token, braced};

use crate::impl_block::{ImplBlock, generate_impl_methods, parse_impl_blocks};

struct DefineStructInput {
    name: Ident,
    fields: Vec<Ident>,
    impl_blocks: Vec<ImplBlock>,
}

impl Parse for DefineStructInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![struct]>()?;
        let name: Ident = input.parse()?;

        let fields_content;
        braced!(fields_content in input);

        let fields = fields_content.parse_terminated(Ident::parse, Token![,])?;
        let fields: Vec<Ident> = fields.into_iter().collect();

        let impl_blocks = parse_impl_blocks(input, &name, "struct")?;

        Ok(DefineStructInput {
            name,
            fields,
            impl_blocks,
        })
    }
}

pub(crate) fn generate_define_struct(input: TokenStream) -> TokenStream {
    let def = syn::parse_macro_input!(input as DefineStructInput);

    let struct_name = &def.name;
    let struct_name_str = struct_name.to_string();

    let field_strs: Vec<String> = def.fields.iter().map(|f| f.to_string()).collect();

    let method_fns = generate_impl_methods(&struct_name_str, &def.impl_blocks);

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
