pub mod math {
    use crate::state::InterpreterState;
    use crate::value::TlangValue;

    pub fn floor(_: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
        match &args[0] {
            TlangValue::Float(f) => TlangValue::Float(f.floor()),
            TlangValue::Int(i) => TlangValue::Int(*i),
            value => panic!("Expected float or int, got {:?}", value),
        }
    }

    pub fn sqrt(_: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
        match &args[0] {
            TlangValue::Float(f) => TlangValue::Float(f.sqrt()),
            TlangValue::Int(i) => TlangValue::Float(((*i) as f64).sqrt()),
            value => panic!("Expected float or int, got {:?}", value),
        }
    }

    pub fn random(_: &mut InterpreterState, _: &[TlangValue]) -> TlangValue {
        TlangValue::Float(rand::random())
    }

    pub fn random_int(_: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
        match &args[0] {
            TlangValue::Int(i) => TlangValue::Int(rand::random::<i64>() % i),
            value => panic!("Expected int, got {:?}", value),
        }
    }
}

pub mod collections {
    use crate::state::InterpreterState;
    use crate::value::{TlangObjectKind, TlangValue};

    pub fn len(state: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
        match state.get_object(args[0]) {
            Some(TlangObjectKind::Struct(obj)) => TlangValue::Int(obj.field_values.len() as i64),
            Some(TlangObjectKind::String(string)) => TlangValue::Int(string.len() as i64),
            _ => panic!("Expected struct or string, got {:?}", args[0]),
        }
    }
}

pub mod utils {
    use crate::state::InterpreterState;
    use crate::value::TlangValue;

    pub fn log(state: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
        println!(
            "{}",
            args.iter()
                .map(|v| state.stringify(v))
                .collect::<Vec<_>>()
                .join(" ")
        );
        TlangValue::Nil
    }
}
