use proc_macro::TokenStream;
use quote::quote;

pub(crate) struct NativeFnMeta {
    name: Option<String>,
    arity: Option<usize>,
}

pub(crate) fn parse_native_fn_meta(
    args: syn::punctuated::Punctuated<syn::Meta, syn::token::Comma>,
) -> NativeFnMeta {
    use syn::*;

    let mut meta = NativeFnMeta {
        name: None,
        arity: None,
    };

    for meta_item in args {
        match meta_item {
            Meta::NameValue(MetaNameValue { path, value, .. }) => {
                if path.is_ident("name") {
                    if let Expr::Lit(ExprLit {
                        lit: Lit::Str(lit_str),
                        ..
                    }) = value
                    {
                        meta.name = Some(lit_str.value());
                    }
                } else if path.is_ident("arity") {
                    if let Expr::Lit(ExprLit {
                        lit: Lit::Int(lit_int),
                        ..
                    }) = value
                    {
                        meta.arity = Some(lit_int.base10_parse().expect("Invalid arity"));
                    }
                }
            }
            _ => {}
        }
    }
    meta
}

pub(crate) fn generate_native_fn(input_fn: syn::ItemFn, native_fn_meta: NativeFnMeta) -> TokenStream {
    use syn::{FnArg, Ident, PatType, Type};

    let fn_name_ident = input_fn.sig.ident.clone();
    let fn_name_string = fn_name_ident.to_string();

    let mut arity = 0usize;
    let mut arg_idents = Vec::new();

    let mut iter = input_fn.sig.inputs.iter();
    iter.next().expect("Native function must have state arg");

    for arg in iter {
        if let FnArg::Typed(PatType { pat, ty, .. }) = arg {
            if let Type::Path(type_path) = &**ty {
                if type_path.path.segments.last().unwrap().ident == "TlangValue" {
                    arity += 1;
                    arg_idents.push(pat.clone());
                } else if let Some(seg) = type_path.path.segments.last() {
                    if seg.ident == "TlangValue" {
                        arity += 1;
                        arg_idents.push(pat.clone());
                    }
                }
            }
        }
    }

    let is_n_ary = input_fn.sig.inputs.len() == 2
        && matches!(
            input_fn.sig.inputs.iter().nth(1),
            Some(syn::FnArg::Typed(pat_type))
                if matches!(
                    &*pat_type.ty,
                    syn::Type::Reference(type_ref)
                        if matches!(
                            &*type_ref.elem,
                            syn::Type::Slice(slice)
                                if matches!(
                                    &*slice.elem,
                                    syn::Type::Path(type_path)
                                        if type_path.path.is_ident("TlangValue")
                                )
                        )
                )
        );

    let fn_native_name_ident =
        Ident::new(&format!("{}_native", fn_name_string), fn_name_ident.span());
    let binding_name = native_fn_meta.name.unwrap_or_default();
    let mut arity = native_fn_meta.arity.unwrap_or(arity);

    let wrapper_fn = if is_n_ary {
        // n-ary functions are identified by taking a slice of TlangValue.
        // This should be an edge case.
        arity = usize::MAX;
        quote! {
            pub fn #fn_native_name_ident(state: &mut tlang_memory::InterpreterState, args: &[tlang_memory::TlangValue]) -> tlang_memory::value::object::NativeFnReturn {
                tlang_memory::value::object::NativeFnReturn::Return(#fn_name_ident(state, args))
            }
        }
    } else {
        quote! {
            pub fn #fn_native_name_ident(state: &mut tlang_memory::InterpreterState, args: &[tlang_memory::TlangValue]) -> tlang_memory::value::object::NativeFnReturn {
                if args.len() != #arity {
                    state.panic(format!("Expected {} arguments, got {}", #arity, args.len()));
                }
                let mut iter = args.iter().cloned();
                #(let #arg_idents = iter.next().unwrap();)*
                tlang_memory::value::object::NativeFnReturn::Return(#fn_name_ident(state, #(#arg_idents),*))
            }
        }
    };

    let generated = quote! {
        #input_fn
        #wrapper_fn

        inventory::submit! {
            crate::NativeFnDef::new(
                #fn_name_string,
                #binding_name,
                #arity,
                #fn_native_name_ident,
                module_path!(),
            )
        }
    };

    TokenStream::from(generated)
}
