use tlang_bindings_js::tlang::Tlang;
use wasm_bindgen::prelude::*;
use wasm_bindgen_test::*;

#[wasm_bindgen_test]
fn smoketest() -> Result<(), JsError> {
    let mut tlang = Tlang::new("fn main() { log::log(\"Hello, world!\"); }".to_string());

    tlang.eval()?;

    Ok(())
}
