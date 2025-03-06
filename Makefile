.PHONY: all clean test test-bindings-js tlangdi-release-debug

all:
	cargo +nightly make

clean:
	cargo +nightly make clean

test:
	cargo build --release --features=binary --bin tlangdi
	cargo build --release --bin tlang_cli_js
	cargo run --release --bin tlang_test_runner

test-bindings-js:
	wasm-pack test --node crates/tlang_bindings_js

tlangdi-release-debug:
	RUSTFLAGS="-C force-frame-pointers=yes -C opt-level=0" cargo build --release --features=binary --bin tlangdi
