# tlang - toy language to learn parsing / simple codegen

tlang is a functional programming language implemented in Rust that compiles to JavaScript. It features pattern matching, tail call optimization, algebraic data types (enums), and WebAssembly bindings for browser usage.

Always reference these instructions first and fallback to search or bash commands only when you encounter unexpected information that does not match the info here.

## Working Effectively

### Bootstrap and Build (NEVER CANCEL - Build times are long)

**CRITICAL**: All build commands can take 30+ minutes. NEVER CANCEL builds or tests. Use timeouts of 60+ minutes for build commands and 30+ minutes for test commands.

Run `make copilot-bootstrap` to install all necessary dependencies and tools.
Prefer this command to install dependencies. In case this fails, take the steps below.

1. **Install Rust toolchain and dependencies:**

   ```bash
   # Rust nightly is required (set by rust-toolchain.toml)
   rustup target add wasm32-unknown-unknown

   # Install required Rust components for testing and formatting
   rustup component add clippy rustfmt
   cargo install cargo-nextest        # ~3m 10s installation time
   cargo install cargo-insta
   cargo install wasm-pack           # ~1m 30s installation time

   # Install exact wasm-bindgen-cli version (CRITICAL - version must match Cargo.toml)
   WASM_BINDGEN_VERSION=$(grep -oP 'wasm-bindgen = "=\K[^"]+' crates/tlang_bindings_js/Cargo.toml)
   cargo install wasm-bindgen-cli --version $WASM_BINDGEN_VERSION  # ~1m 30s installation time
   ```

2. **Install Node.js dependencies (CRITICAL - Node.js version must be 24.0.2):**

   ```bash
   # CRITICAL: Ensure Node.js version 24.0.2 is installed (required by package.json volta config)
   # Tests will fail with incorrect Node.js versions due to stack trace differences in expected output
   node --version  # Must show v24.0.2
   npm ci  # ~12s installation time
   ```

3. **Download binaryen (required for WebAssembly optimization):**
   ```bash
   wget https://github.com/WebAssembly/binaryen/releases/download/version_119/binaryen-version_119-x86_64-linux.tar.gz
   tar xvzf binaryen-version_119-x86_64-linux.tar.gz
   export PATH=$(pwd)/binaryen-version_119/bin:$PATH
   ```

### Build Commands (NEVER CANCEL - Set timeout to 60+ minutes)

**CRITICAL: Node.js version 24.0.2 is required for all tests to pass. Verify with `node --version` before running tests.**

- **Rust tests**: `cargo nextest run --profile=ci` -- takes ~55s. NEVER CANCEL. Set timeout to 30+ minutes.
- **Integration tests**: `make test` -- takes ~64s. Tests built compiler/interpreter with both backends. **REQUIRES Node.js 24.0.2 exactly** (see package.json volta config). Test failures with different Node.js versions are due to stack trace differences in expected output. NEVER CANCEL. Set timeout to 30+ minutes.
- **WebAssembly bindings test**: `make test-bindings-js` -- takes ~21s. NEVER CANCEL. Set timeout to 30+ minutes.
- **Build playground**: `npm run build` -- takes ~13s. NEVER CANCEL. Set timeout to 30+ minutes.
- **Build interpreter**: `cargo build --release --features=binary --bin tlangdi` -- takes ~30s. NEVER CANCEL. Set timeout to 60+ minutes.
- **Build CLI**: `cargo build --release --bin tlang_cli_js` -- takes ~21s. NEVER CANCEL. Set timeout to 60+ minutes.

### Run Applications

- **Development server**: `npm run dev` (starts Vite dev server on http://localhost:5173/)
- **CLI compiler**: `./target/release/tlang_cli_js input.tlang --output-type js`
- **Interpreter**: `./target/release/tlangdi input.tlang`

## Commit Message Guidelines

### Semantic Commit Messages

Use semantic commit messages following the [Conventional Commits](https://www.conventionalcommits.org/) specification:

```
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```

#### Commit Types

- **feat**: A new feature for the tlang language or tooling
- **fix**: A bug fix in the compiler, interpreter, or tooling
- **docs**: Documentation only changes (README, copilot instructions, code comments)
- **style**: Code style changes (formatting, missing semicolons, etc.) that don't affect functionality
- **refactor**: Code restructuring that doesn't fix bugs or add features
- **test**: Adding missing tests or correcting existing tests
- **chore**: Changes to build process, dependencies, or development tools
- **perf**: Performance improvements
- **ci**: Changes to CI configuration files and scripts

#### Examples for tlang Project

```bash
feat(parser): add support for optional function parameters
fix(codegen): resolve tail call optimization bug in nested functions
docs: update installation instructions for wasm-bindgen
test(interpreter): add tests for enum pattern matching
refactor(hir): simplify expression lowering logic
chore(deps): update wasm-bindgen to latest version
perf(optimizer): improve dead code elimination algorithm
ci: add semantic commit message validation
```

#### Scope Guidelines

Common scopes for tlang:

- **parser**: Changes to parsing logic
- **lexer**: Tokenization and lexical analysis
- **ast**: Abstract syntax tree definitions
- **semantics**: Semantic analysis and type checking
- **hir**: High-level intermediate representation
- **codegen**: Code generation (JavaScript output)
- **interpreter**: Direct execution interpreter
- **bindings**: WebAssembly bindings for browser
- **cli**: Command-line tools
- **playground**: Web-based playground
- **deps**: Dependency updates

## Validation

### Mandatory Testing After Changes

ALWAYS manually validate any code changes through complete end-to-end scenarios:

0. **Run linting and formatting before any commits:**

   ```bash
   # Ensure required Rust components are installed (should be done in bootstrap step 1)
   # rustup component add clippy rustfmt

   # Run Rust linting and formatting before any commits
   cargo clippy --fix --allow-dirty --lib --all-features    # Fix Rust linting issues automatically
   cargo clippy --fix --allow-dirty --tests --all-features  # Fix Rust linting issues in tests
   cargo fmt                                                # Format Rust code
   ```

   **Remember to use semantic commit messages** (see Commit Message Guidelines above) when committing your changes.

1. **Test Rust build and core functionality:**

   ```bash
   cargo nextest run --profile=ci  # NEVER CANCEL - takes ~55s
   make test                       # NEVER CANCEL - takes ~64s (integration tests with interpreter + JS backends, **REQUIRES Node.js 24.0.2 exactly**)
   ```

2. **Test WebAssembly bindings:**

   ```bash
   make test-bindings-js           # NEVER CANCEL - takes ~21s
   ```

3. **Test CLI functionality with working example:**

   ```bash
   # This should output generated JavaScript and print "120" when executed
   ./target/release/tlang_cli_js tests/functions/pipeline/pipeline.tlang --output-type js | node
   ```

4. **Test interpreter functionality:**

   ```bash
   # This should print "120"
   ./target/release/tlangdi tests/functions/pipeline/pipeline.tlang
   ```

5. **Test playground build and development server:**
   ```bash
   npm run build     # NEVER CANCEL - takes ~13s, requires binaryen in PATH
   npm run dev       # Should start server on localhost:5173
   ```

### Required Linting and Validation

ALWAYS run these before completing changes or CI will fail:

```bash
# Ensure required Rust components are installed (should be done in bootstrap step 1)
# rustup component add clippy rustfmt

# Run Rust linting and formatting before any commits
cargo clippy --fix --allow-dirty --lib --all-features    # Fix Rust linting issues automatically
cargo clippy --fix --allow-dirty --tests --all-features  # Fix Rust linting issues in tests
cargo fmt                                                # Format Rust code

# Run JavaScript/TypeScript linting and validation
npm run lint        # ~4s - ESLint and Stylelint
npm run typecheck   # ~0.3s - TypeScript check
```

**Use semantic commit messages** (see Commit Message Guidelines above) for all commits to maintain consistent project history.

## Project Structure

### Key Crates (Rust workspace)

- **tlang_lexer**: Tokenization and lexical analysis
- **tlang_parser**: Parse tree generation
- **tlang_ast**: Abstract syntax tree definitions
- **tlang_semantics**: Semantic analysis and type checking
- **tlang_hir**: High-level intermediate representation
- **tlang_hir_opt**: HIR optimizations
- **tlang_codegen_js**: JavaScript code generation
- **tlang_interpreter**: Direct execution interpreter
- **tlang_bindings_js**: WebAssembly bindings for browser
- **tlang_cli_js**: Command-line compiler tool
- **tlang_test_runner**: Integration test runner

### Frontend Components

- **tlang-playground/**: Web-based playground (Vite + TypeScript + Lit)
- **packages/**: Shared TypeScript packages including CodeMirror language support

### Test Structure

- **tests/**: Integration tests organized by feature (enums, functions, loops, operators, strings, structs)
- **crates/\*/tests/**: Unit tests for individual crates

## Common Tasks

### Repo Root Directory Listing

```
.
..
.github/          # GitHub workflows and configuration
.gitignore
.prettierrc.toml
.stylelintignore
Cargo.lock
Cargo.toml        # Rust workspace configuration
Makefile          # Build automation
README.md
crates/           # Rust crates
eslint.config.mjs # ESLint configuration
package-lock.json
package.json      # Node.js workspace configuration
packages/         # TypeScript packages
rust-toolchain.toml # Rust toolchain specification (nightly)
stylelint.config.js
tests/            # Integration tests
tlang-playground/ # Web playground
tsconfig.json
```

### Understanding Test Failures

- The `make test` command runs integration tests with both interpreter and JavaScript backends
- **CRITICAL**: Tests require Node.js 24.0.2 exactly (specified in package.json volta config). Test failures with different Node.js versions are due to stack trace differences in expected output files.
- Common failure causes:
  1. **Node.js version mismatch**: Ensure you're using Node.js 24.0.2 exactly. Use `node --version` to verify. Do NOT update expected output files if using wrong Node.js version.
  2. **Stacktrace/line number differences**: When only stacktraces or line numbers differ, verify you're using the correct Node.js version first before updating expected output.
- Interpreter tests should always pass
- Use CI results as authoritative - if tests pass on CI, local failures are likely environment issues
- Focus on ensuring your changes don't break existing interpreter functionality

### Working with WebAssembly

- WebAssembly build requires binaryen in PATH for optimization
- Development builds: `wasm-pack build --dev --target web crates/tlang_bindings_js`
- Production builds: `wasm-pack build --target web crates/tlang_bindings_js`
- The playground automatically rebuilds WASM when starting dev server

### Language Features to Test

When making changes, test these key language features:

- **Function definitions and calls**: Pattern matching in function parameters
- **Enums and pattern matching**: Option, Result, custom enums
- **List operations**: Map, filter, fold with proper tail call optimization
- **Pipeline operator**: Functional composition with `|>`
- **Tail call optimization**: Functions marked with `rec` keyword
- **Struct definitions and methods**: Object-like data structures

### Example Working tlang Code

```tlang
// map(a[], fn(a) -> b) -> b[]
fn map([], _) { [] }
fn map([x, ...xs], f) { [f(x), ...map(xs, f)] }

[0,1,2,3,4,5,6,7,8,9]
|> map(fn (x) { x ** 2 })
|> filter(fn (x) { x % 2 == 0 })
|> foldl(0, fn (acc, x) { acc + x })
|> log();
```

## Troubleshooting

### Common Build Issues

- **Missing wasm target**: Run `rustup target add wasm32-unknown-unknown`
- **Wrong wasm-bindgen version**: Reinstall with exact version from Cargo.toml
- **Binaryen not found**: Download and add to PATH as shown above
- **Node.js version**: Project requires Node.js 24.0.2 exactly (see package.json volta config). Use `node --version` to verify. Test failures with wrong Node.js versions are due to stack trace format differences in expected output files.

### Known Limitations

- Some JavaScript backend tests fail due to panic handling differences
- CLI requires file input (cannot use stdin directly)
- WebAssembly builds require internet access to download binaryen
- Build times are long due to the complexity of the compiler pipeline

### Time Expectations

- Initial setup (tools installation): 5-7 minutes
- Full rebuild from scratch: 2-3 minutes
- Incremental builds: 10-30 seconds
- Test suite: 1-2 minutes
- **NEVER CANCEL any build or test command before these time limits**
