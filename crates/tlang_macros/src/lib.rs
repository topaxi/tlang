use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

#[proc_macro_attribute]
pub fn native_fn(attr: TokenStream, input: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr with syn::punctuated::Punctuated::<syn::Meta, syn::Token![,]>::parse_terminated);
    let input_fn = parse_macro_input!(input as syn::ItemFn);

    let fn_name = input_fn.sig.ident.clone();
    let mut binding_name = file!()
        .split('/')
        .last()
        .unwrap_or("unknown")
        .replace(".rs", "")
        + "::"
        + fn_name.to_string().as_str();
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

    let generated = if binding_name.is_empty() {
        quote! {
            #input_fn

            inventory::submit! {
                crate::NativeFn {
                    name: #binding_name,
                    function: #fn_name,
                }
            }
        }
    } else {
        todo!();

        quote! {
            #input_fn

            inventory::submit! {
                crate::NativeFn {
                    name: (module_path!()
                        .split("::")
                        .last()
                        .unwrap_or("unknown") + "::" + stringify!(fn_name)).as_str(),
                    function: #fn_name,
                }
            }
        }
    };

    TokenStream::from(generated)
}

fn extract_name_from_meta(meta: &syn::Meta) -> String {
    if let syn::Meta::NameValue(syn::MetaNameValue {
        path,
        value:
            syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Str(lit_str),
                ..
            }),
        ..
    }) = meta
    {
        println!("Custom name provided...");
        if path.is_ident("name") {
            return lit_str.value();
        }
    }
    println!("No custom name provided...");
    "".to_string() // Return an empty string if no 'name' attribute is provided
}

fn generate_prefixed_name(ident: &syn::Ident) -> String {
    "math::".to_string() + &ident.to_string() // Placeholder for module-based prefixing
}
