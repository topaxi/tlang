use proc_macro::TokenStream;
use syn::parse_macro_input;

mod define_enum;
mod define_protocol;
mod define_struct;
mod impl_block;
mod native_fn;
mod native_method;
mod protocol_impl;

#[proc_macro_attribute]
pub fn native_fn(attr: TokenStream, input: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(input as syn::ItemFn);
    let native_fn_meta = native_fn::parse_native_fn_meta(
        parse_macro_input!(attr with syn::punctuated::Punctuated::<syn::Meta, syn::Token![,]>::parse_terminated),
    );

    native_fn::generate_native_fn(&input_fn, native_fn_meta)
}

#[proc_macro_attribute]
pub fn native_method(attr: TokenStream, input: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(input as syn::ItemFn);
    let meta = native_method::parse_native_method_attr(attr);
    native_method::generate_native_method(&input_fn, meta)
}

#[proc_macro_attribute]
pub fn protocol_impl(attr: TokenStream, input: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(input as syn::ItemFn);
    let meta = protocol_impl::parse_protocol_impl_attr(attr);
    protocol_impl::generate_protocol_impl(&input_fn, meta)
}

#[proc_macro]
pub fn define_enum(input: TokenStream) -> TokenStream {
    define_enum::generate_define_enum(input)
}

#[proc_macro]
pub fn define_struct(input: TokenStream) -> TokenStream {
    define_struct::generate_define_struct(input)
}

#[proc_macro]
pub fn define_protocol(input: TokenStream) -> TokenStream {
    define_protocol::generate_define_protocol(input)
}
