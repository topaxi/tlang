.PHONY: all clean verify test test-coverage test-debug test-review test-accept test-bindings-js tlangdi-release-debug lsp-build lsp-install

all:
	cargo +nightly make

clean:
	cargo +nightly make clean

verify:
	cargo fmt
	cargo clippy
	npm ci
	npm run lint
	$(MAKE) test-coverage
	$(MAKE) test
	$(MAKE) test-bindings-js
	npm run build
	npm run test:e2e

test:
	cargo build --release --bin tlang
	RUST_BACKTRACE=1 cargo insta test -p tlang_test_runner --no-quiet -- --nocapture

test-coverage:
	cargo llvm-cov clean --profraw-only
	find target -maxdepth 5 -name incremental -type d -exec rm -rf {} + 2>/dev/null || true
	cargo llvm-cov nextest --profile=ci --workspace --exclude tlang_bindings_js --exclude tlang_test_runner --exclude tlang_macros --exclude xtask --features tlang_vm/testing --no-report
	# Quality gate: thresholds represent the current baseline coverage.
	# Fail if coverage regresses below these values.
	# Excludes proc macros (tlang_macros) and dev tools (xtask) which cannot be instrumented by llvm-cov.
	# Also excludes binary entry points (main.rs) from the report.
	cargo llvm-cov report --ignore-filename-regex '(^|/)(main\.rs|tlang_macros/|xtask/)' --fail-under-lines 80 --fail-under-functions 80 --fail-under-regions 80
	cargo llvm-cov report --ignore-filename-regex '(^|/)(main\.rs|tlang_macros/|xtask/)' --html

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

lsp-build:
	cargo build --release --bin tlang-lsp-server

lsp-install: lsp-build
	mkdir -p ~/.local/bin
	ln -sf "$(CURDIR)/target/release/tlang-lsp-server" ~/.local/bin/tlang-lsp-server
