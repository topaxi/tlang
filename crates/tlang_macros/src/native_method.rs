use proc_macro::TokenStream;
use quote::quote;

pub(crate) struct NativeMethodMeta {
    pub type_name: String,
    pub priority: u8,
}

pub(crate) fn parse_native_method_attr(attr: TokenStream) -> NativeMethodMeta {
    let parser = syn::punctuated::Punctuated::<syn::Expr, syn::Token![,]>::parse_terminated;
    let exprs =
        syn::parse::Parser::parse(parser, attr).expect("Failed to parse native_method attributes");

    let mut meta = NativeMethodMeta {
        type_name: String::new(),
        priority: 0,
    };

    for (i, expr) in exprs.iter().enumerate() {
        match expr {
            syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Str(s),
                ..
            }) if i == 0 => {
                meta.type_name = s.value();
            }
            syn::Expr::Assign(assign) => {
                if let syn::Expr::Path(path) = &*assign.left
                    && path.path.is_ident("priority")
                    && let syn::Expr::Lit(syn::ExprLit {
                        lit: syn::Lit::Int(lit_int),
                        ..
                    }) = &*assign.right
                {
                    meta.priority = lit_int.base10_parse().expect("Invalid priority value");
                }
            }
            _ => {}
        }
    }

    if meta.type_name.is_empty() {
        panic!(
            "native_method requires a type name as first argument, e.g. #[native_method(\"Option\")]"
        );
    }

    meta
}

pub(crate) fn generate_native_method(
    input_fn: &syn::ItemFn,
    meta: &NativeMethodMeta,
) -> TokenStream {
    use syn::{FnArg, PatType};

    let fn_name_ident = &input_fn.sig.ident;
    let fn_name_string = fn_name_ident.to_string();
    let type_name = &meta.type_name;
    let priority = meta.priority;

    // Collect parameter names (skip `state`/`vm` and `this`)
    let mut param_idents = Vec::new();
    let mut iter = input_fn.sig.inputs.iter();

    // Skip first param (vm: &mut VMState)
    iter.next()
        .expect("native_method function must have a VMState parameter");
    // Skip second param (this: TlangValue)
    iter.next()
        .expect("native_method function must have a `this` parameter");

    for arg in iter {
        if let FnArg::Typed(PatType { pat, .. }) = arg {
            param_idents.push(pat.clone());
        }
    }

    let native_fn_name = syn::Ident::new(
        &format!("{fn_name_string}_native_method"),
        fn_name_ident.span(),
    );

    // The wrapper receives args as [this, arg1, arg2, ...] and calls the original fn.
    // Use .get() with a Nil default so callers can omit trailing optional args.
    let arg_extractions: Vec<_> = param_idents
        .iter()
        .enumerate()
        .map(|(i, pat)| {
            let idx = i + 1; // args[0] is `this`
            quote! { let #pat = args.get(#idx).copied().unwrap_or(tlang_memory::TlangValue::Nil); }
        })
        .collect();

    let generated = quote! {
        #input_fn

        pub fn #native_fn_name(
            state: &mut tlang_memory::VMState,
            args: &[tlang_memory::TlangValue],
        ) -> tlang_memory::NativeFnReturn {
            let this = args[0];
            #(#arg_extractions)*
            #fn_name_ident(state, this, #(#param_idents),*).into()
        }

        inventory::submit! {
            crate::NativeMethodDef::new(
                #type_name,
                #fn_name_string,
                #native_fn_name,
                #priority,
            )
        }
    };

    TokenStream::from(generated)
}
