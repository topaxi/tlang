.PHONY: all clean test

all:
	cargo +nightly make

clean:
	cargo +nightly make clean

test:
	cargo build --release --features=binary --bin tlangdi
	cargo build --release --bin tlang_cli_js
	cargo run --release --bin tlang_test_runner
