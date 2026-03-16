use tlang_macros::native_fn;
use tlang_memory::{TlangValue, VMState};

tlang_macros::define_protocol! {
    Truthy {
        fn truthy(this);
    }
    Functor {
        fn map(this, f);
    }
    Match {
        fn matches(this, value);
    }
    Iterable {
        fn iter(this);
    }
    Iterator {
        fn next(this);
    }
}

#[native_fn(name = "map")]
pub fn map(state: &mut VMState, iterable: TlangValue, func: TlangValue) -> TlangValue {
    let type_name = state.type_name_of(iterable);
    if let Some(fn_value) = state.get_protocol_impl("Functor", type_name, "map") {
        return state.call(fn_value, &[iterable, func]);
    }

    state.panic(format!(
        "No implementation of `Functor::map` for type `{}`",
        type_name,
    ))
}
