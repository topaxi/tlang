use proc_macro::TokenStream;
use quote::quote;

pub(crate) struct ProtocolImplMeta {
    pub protocol: String,
    pub type_name: String,
    pub method: Option<String>,
    pub priority: u8,
}

pub(crate) fn parse_protocol_impl_attr(attr: TokenStream) -> ProtocolImplMeta {
    let parser = syn::punctuated::Punctuated::<syn::Expr, syn::Token![,]>::parse_terminated;
    let exprs =
        syn::parse::Parser::parse(parser, attr).expect("Failed to parse protocol_impl attributes");

    let mut meta = ProtocolImplMeta {
        protocol: String::new(),
        type_name: String::new(),
        method: None,
        priority: 0,
    };

    for (i, expr) in exprs.iter().enumerate() {
        match expr {
            syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Str(s),
                ..
            }) => match i {
                0 => meta.protocol = s.value(),
                1 => meta.type_name = s.value(),
                _ => {}
            },
            syn::Expr::Assign(assign) => {
                if let syn::Expr::Path(path) = &*assign.left {
                    if path.path.is_ident("priority") {
                        if let syn::Expr::Lit(syn::ExprLit {
                            lit: syn::Lit::Int(lit_int),
                            ..
                        }) = &*assign.right
                        {
                            meta.priority = lit_int.base10_parse().expect("Invalid priority value");
                        }
                    } else if path.path.is_ident("method")
                        && let syn::Expr::Lit(syn::ExprLit {
                            lit: syn::Lit::Str(s),
                            ..
                        }) = &*assign.right
                    {
                        meta.method = Some(s.value());
                    }
                }
            }
            _ => {}
        }
    }

    if meta.protocol.is_empty() {
        panic!("protocol_impl requires a protocol name as first argument");
    }
    if meta.type_name.is_empty() {
        panic!("protocol_impl requires a type name as second argument");
    }

    meta
}

pub(crate) fn generate_protocol_impl(
    input_fn: &syn::ItemFn,
    meta: ProtocolImplMeta,
) -> TokenStream {
    use syn::{FnArg, PatType};

    let fn_name_ident = &input_fn.sig.ident;
    let fn_name_string = fn_name_ident.to_string();
    let protocol = &meta.protocol;
    let type_name = &meta.type_name;
    let method_name = meta.method.as_deref().unwrap_or(&fn_name_string);
    let priority = meta.priority;

    // Collect parameter names (skip `state`/`vm`)
    let mut param_idents = Vec::new();
    let mut iter = input_fn.sig.inputs.iter();

    // Skip first param (vm: &mut VMState)
    iter.next()
        .expect("protocol_impl function must have a VMState parameter");

    for arg in iter {
        if let FnArg::Typed(PatType { pat, .. }) = arg {
            param_idents.push(pat.clone());
        }
    }

    let arity = param_idents.len();

    let native_fn_name = syn::Ident::new(
        &format!("{fn_name_string}_protocol_impl"),
        fn_name_ident.span(),
    );

    let arg_extractions: Vec<_> = param_idents
        .iter()
        .enumerate()
        .map(|(i, pat)| {
            quote! { let #pat = args[#i]; }
        })
        .collect();

    let generated = quote! {
        #input_fn

        pub fn #native_fn_name(
            state: &mut tlang_memory::VMState,
            args: &[tlang_memory::TlangValue],
        ) -> tlang_memory::NativeFnReturn {
            if args.len() != #arity {
                state.panic(format!("Expected {} arguments, got {}", #arity, args.len()));
            }
            #(#arg_extractions)*
            #fn_name_ident(state, #(#param_idents),*).into()
        }

        inventory::submit! {
            crate::NativeProtocolImplDef::new(
                #protocol,
                #type_name,
                #method_name,
                #native_fn_name,
                #priority,
            )
        }
    };

    TokenStream::from(generated)
}
