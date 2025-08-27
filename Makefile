.PHONY: all clean test test-debug test-review test-accept test-bindings-js tlangdi-release-debug

all:
	cargo +nightly make

clean:
	cargo +nightly make clean

test:
	cargo build --release --features=binary --bin tlangdi
	cargo build --release --bin tlang_cli_js
	RUST_BACKTRACE=1 cargo insta test -p tlang_test_runner

test-debug:
	RUSTFLAGS="-C force-frame-pointers=yes -C opt-level=0" cargo build --release --features=binary --bin tlangdi
	cargo build --release --bin tlang_cli_js
	RUST_BACKTRACE=full cargo insta test -p tlang_test_runner

test-review:
	cargo build --release --features=binary --bin tlangdi
	cargo build --release --bin tlang_cli_js
	RUST_BACKTRACE=1 cargo insta test -p tlang_test_runner --review

test-accept:
	cargo build --release --features=binary --bin tlangdi
	cargo build --release --bin tlang_cli_js
	RUST_BACKTRACE=1 INSTA_UPDATE=always cargo test -p tlang_test_runner

test-bindings-js:
	wasm-pack test --node crates/tlang_bindings_js

tlangdi-release-debug:
	RUSTFLAGS="-C force-frame-pointers=yes -C opt-level=0" cargo build --release --features=binary --bin tlangdi
