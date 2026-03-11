.PHONY: all clean test test-coverage test-debug test-review test-accept test-bindings-js tlangdi-release-debug

all:
	cargo +nightly make

clean:
	cargo +nightly make clean

test:
	cargo build --release --bin tlang
	RUST_BACKTRACE=1 cargo insta test -p tlang_test_runner --no-quiet -- --nocapture

test-coverage:
	cargo llvm-cov clean --profraw-only
	cargo llvm-cov nextest --profile=ci --workspace --exclude tlang_bindings_js --exclude tlang_test_runner --no-report
	# Quality gate: thresholds represent the current baseline coverage. Fail if coverage
	# regresses below these values (lines 80%, functions 81%, regions 78%).
	cargo llvm-cov report --fail-under-lines 80 --fail-under-functions 81 --fail-under-regions 78
	cargo llvm-cov report --html

test-debug:
	RUSTFLAGS="-C force-frame-pointers=yes -C opt-level=0" cargo build --release --bin tlang
	RUST_BACKTRACE=full cargo insta test -p tlang_test_runner --no-quiet -- --nocapture

test-review:
	cargo build --release --bin tlang
	RUST_BACKTRACE=1 cargo insta test -p tlang_test_runner --review --no-quiet -- --nocapture

test-accept:
	cargo build --release --bin tlang
	RUST_BACKTRACE=1 INSTA_UPDATE=always cargo test -p tlang_test_runner

test-bindings-js:
	wasm-pack test --node crates/tlang_bindings_js

tlangdi-release-debug:
	RUSTFLAGS="-C force-frame-pointers=yes -C opt-level=0" cargo build --release --features=tlang_vm/binary --bin tlangdi
