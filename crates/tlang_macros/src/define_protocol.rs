use proc_macro::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::{Ident, Token, braced, parenthesized};

struct ProtocolMethod {
    name: Ident,
    arity: u16,
}

struct Protocol {
    name: Ident,
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

            let methods_content;
            braced!(methods_content in input);

            let mut methods = Vec::new();
            while !methods_content.is_empty() {
                methods_content.parse::<Token![fn]>()?;
                let method_name: Ident = methods_content.parse()?;

                let params_content;
                parenthesized!(params_content in methods_content);
                let params = params_content.parse_terminated(Ident::parse, Token![,])?;
                let arity = params.len() as u16;

                methods.push(ProtocolMethod {
                    name: method_name,
                    arity,
                });

                // Allow trailing semicolon
                if methods_content.peek(Token![;]) {
                    methods_content.parse::<Token![;]>()?;
                }
            }

            protocols.push(Protocol { name, methods });
        }

        Ok(DefineProtocolInput { protocols })
    }
}

pub(crate) fn generate_define_protocol(input: TokenStream) -> TokenStream {
    let def = syn::parse_macro_input!(input as DefineProtocolInput);

    let mut submits = Vec::new();

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

        submits.push(quote! {
            inventory::submit! {
                crate::NativeProtocolDef::new(
                    #protocol_name_str,
                    &[#(#method_entries),*],
                )
            }
        });
    }

    let generated = quote! {
        #(#submits)*
    };

    TokenStream::from(generated)
}
