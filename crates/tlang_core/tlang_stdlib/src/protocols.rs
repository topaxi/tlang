use tlang_macros::native_fn;
use tlang_memory::{TlangValue, VMState};

tlang_macros::define_protocol! {
    Truthy {
        fn truthy(this);
    }
    Functor {
        fn map(this, f);
    }
    Accepts {
        fn accepts(this, value);
    }
    Iterable {
        fn iter(this);
    }
    Iterator {
        fn next(this);
    }
    Display {
        fn to_string(this) {
            let s = vm.stringify_default(this);
            vm.new_string(s)
        }
    }
}

/// Global map protocol dispatch function.
#[native_fn(name = "map")]
pub fn map(state: &mut VMState, iterable: TlangValue, func: TlangValue) -> TlangValue {
    let type_name = state.type_name_of(iterable);
    let type_shape_key = state.type_shape_key_of(iterable);

    if let Some(fn_value) = state
        .protocol_id_by_name("Functor")
        .and_then(|functor_id| state.get_protocol_impl(functor_id, type_shape_key, "map"))
    {
        return state.call(fn_value, &[iterable, func]);
    }

    state.panic(format!(
        "No implementation of `Functor::map` for type `{}`",
        type_name,
    ))
}
