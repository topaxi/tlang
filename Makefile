.PHONY: all clean test tlangdi-release-debug

all:
	cargo +nightly make

clean:
	cargo +nightly make clean

test:
	cargo build --release --features=binary --bin tlangdi
	cargo build --release --bin tlang_cli_js
	cargo run --release --bin tlang_test_runner

tlangdi-release-debug:
	RUSTFLAGS="-C force-frame-pointers=yes" cargo build --release --features=binary --bin tlangdi
