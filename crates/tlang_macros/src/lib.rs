use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

#[proc_macro_attribute]
pub fn native_fn(attr: TokenStream, input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr with syn::punctuated::Punctuated::<syn::Meta, syn::Token![,]>::parse_terminated);
    let input_fn = parse_macro_input!(input as syn::ItemFn);

    let fn_name_ident = input_fn.sig.ident.clone();
    let fn_name_string = fn_name_ident.to_string();
    let mut binding_name = String::new();

    for meta in args {
        match meta {
            syn::Meta::NameValue(syn::MetaNameValue {
                path,
                value:
                    syn::Expr::Lit(syn::ExprLit {
                        lit: syn::Lit::Str(lit_str),
                        ..
                    }),
                ..
            }) if path.is_ident("name") => {
                binding_name = lit_str.value();
            }
            _ => {} // Ignore other attributes
        }
    }

    let generated = quote! {
        #input_fn

        inventory::submit! {
            crate::NativeFnDef {
                name: #fn_name_string,
                binding_name: #binding_name,
                function: #fn_name_ident,
                module_path: module_path!(),
            }
        }
    };

    TokenStream::from(generated)
}
