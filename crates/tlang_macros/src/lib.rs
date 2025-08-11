use proc_macro::TokenStream;
use syn::parse_macro_input;

mod native_fn;

#[proc_macro_attribute]
pub fn native_fn(attr: TokenStream, input: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(input as syn::ItemFn);
    let native_fn_meta = native_fn::parse_native_fn_meta(
        parse_macro_input!(attr with syn::punctuated::Punctuated::<syn::Meta, syn::Token![,]>::parse_terminated),
    );

    native_fn::generate_native_fn(&input_fn, native_fn_meta)
}
