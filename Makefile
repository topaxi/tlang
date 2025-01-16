.PHONY: all clean

all:
	cargo +nightly make

clean:
	cargo +nightly make clean

test:
	cargo build --features=binary --bin tlangdi
	cargo run --release --bin tlang_test_runner
