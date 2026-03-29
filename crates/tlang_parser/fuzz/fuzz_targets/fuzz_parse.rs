#![no_main]

use libfuzzer_sys::fuzz_target;
use tlang_parser::Parser;

fuzz_target!(|data: &[u8]| {
    if let Ok(source) = std::str::from_utf8(data) {
        let mut parser = Parser::from_source(source);
        // The parser must never panic, regardless of input.
        // Parse errors (Err) are expected and fine.
        let _ = parser.parse();
    }
});
