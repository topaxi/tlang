use tlang_hir::hir;
use tlang_parser::error::ParseError;
use tlang_parser::parser::Parser;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct TlangInterpreter(tlang_interpreter::Interpreter);

impl Default for TlangInterpreter {
    fn default() -> Self {
        Self::new()
    }
}

#[wasm_bindgen]
impl TlangInterpreter {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        console_error_panic_hook::set_once();

        TlangInterpreter(tlang_interpreter::Interpreter::default())
    }

    #[wasm_bindgen]
    pub fn eval(&mut self, source: &str) {
        let hir = self.parse(source).unwrap();
        self.0.eval(&hir);
    }

    fn parse(&self, source: &str) -> Result<hir::Module, ParseError> {
        let ast = Parser::from_source(source).parse()?;
        let hir = tlang_ast_lowering::lower_to_hir(&ast);

        Ok(hir)
    }
}
