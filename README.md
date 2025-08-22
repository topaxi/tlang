# tlang - A Functional Programming Language

tlang is a functional programming language implemented in Rust that compiles to JavaScript. It was created as a learning project to explore parsing techniques and simple code generation, but has evolved into a feature-rich language with modern functional programming capabilities.

## Getting Started

Try tlang in your browser at the [interactive playground](https://topaxi.github.io/tlang/) or follow the [development setup](#development-setup) to run it locally.

### Quick Example

```tlang
// Define recursive list functions with pattern matching
fn map([], _) { [] }
fn map([x, ...xs], f) { [f(x), ...map(xs, f)] }

fn filter([], _) { [] }
fn filter([x, ...xs], f) if f(x) { [x, ...filter(xs, f)] }
fn filter([_, ...xs], f) { rec filter(xs, f) }

fn foldl([], acc, _) { acc }
fn foldl([x, ...xs], acc, f) { rec foldl(xs, f(acc, x), f) }

// Calculate sum of squares of even numbers
[0,1,2,3,4,5,6,7,8,9]
|> map(fn (x) { x ** 2 })
|> filter(fn (x) { x % 2 == 0 })
|> foldl(0, fn (acc, x) { acc + x })
|> log();  // Output: 120
```

**Note**: When compiling to JavaScript, standard library functions like `map`, `filter`, and `foldl` are automatically available. When using the interpreter, you need to define them yourself (as shown above).

## Features

- **Pattern Matching**: Powerful pattern matching in function definitions and control flow
- **Tail Call Optimization**: Efficient recursion with the `rec` keyword
- **Algebraic Data Types**: Built-in `Option` and `Result` types, plus custom enums
- **Struct System**: Define data structures with associated methods
- **Pipeline Operator**: Functional composition with `|>` operator
- **Multiple Backends**: Both direct interpretation and JavaScript compilation
- **WebAssembly Bindings**: Run in browsers via WebAssembly
- **Interactive Playground**: Web-based development environment

## Quick Examples

### Basic Function Definition and Pattern Matching
```tlang
// Recursive list processing with pattern matching
fn map([], _) { [] }
fn map([x, ...xs], f) { [f(x), ...map(xs, f)] }

fn filter([], _) { [] }
fn filter([x, ...xs], f) if f(x) { [x, ...filter(xs, f)] }
fn filter([_, ...xs], f) { rec filter(xs, f) }
```

### Pipeline Operations
```tlang
[0,1,2,3,4,5,6,7,8,9]
|> map(fn (x) { x ** 2 })
|> filter(fn (x) { x % 2 == 0 })
|> foldl(0, fn (acc, x) { acc + x })
|> log();
```

### Tail-Recursive Fibonacci
```tlang
fn fib(n) { fib(n, 0, 1) }
fn fib(0, a, _) { a }
fn fib(1, _, b) { b }
fn fib(n, a, b) { rec fib(n - 1, b, a + b) }

fib(50) |> log();
```

### Struct Definitions and Methods
```tlang
struct Vector {
    x: int,
    y: int,
}

fn Vector::new(x: int, y: int) -> Vector {
    Vector { x, y }
}

fn Vector.add(self, other: Vector) -> Vector {
    Vector::new(self.x + other.x, self.y + other.y)
}

let v1 = Vector::new(3, 4);
let v2 = Vector::new(1, 2);
v1.add(v2) |> log();
```

### Option Type Usage
```tlang
let some_x = Option::Some(10);
let none_x = Option::None;

some_x.is_some() |> log();  // true
some_x.unwrap() |> log();   // 10
```

## Development Setup

### Prerequisites

1. **Rust Nightly**: tlang requires Rust nightly toolchain
   ```bash
   # The rust-toolchain.toml file will automatically use nightly
   rustup target add wasm32-unknown-unknown
   ```

2. **Node.js**: Version 24.0.2 (specified in package.json)
   ```bash
   # Using volta (recommended)
   volta install node@24.0.2
   volta install npm@11.4.2
   ```

3. **Required Rust Tools**:
   ```bash
   cargo install cargo-nextest        # ~3m 10s installation time
   cargo install wasm-pack           # ~1m 30s installation time
   
   # Install exact wasm-bindgen-cli version (critical - version must match Cargo.toml)
   WASM_BINDGEN_VERSION=$(grep -oP 'wasm-bindgen = "=\K[^"]+' crates/tlang_bindings_js/Cargo.toml)
   cargo install wasm-bindgen-cli --version $WASM_BINDGEN_VERSION  # ~1m 30s
   ```

4. **WebAssembly Optimization (binaryen)**:
   ```bash
   wget https://github.com/WebAssembly/binaryen/releases/download/version_119/binaryen-version_119-x86_64-linux.tar.gz
   tar xvzf binaryen-version_119-x86_64-linux.tar.gz
   export PATH=$(pwd)/binaryen-version_119/bin:$PATH
   ```

5. **Node.js Dependencies**:
   ```bash
   npm ci  # ~12s installation time
   ```

### Building the Project

⚠️ **Important**: Build commands can take 30+ minutes. Never cancel build operations.

```bash
# Build and test Rust components (55s)
cargo nextest run --profile=ci

# Run integration tests with both backends (64s)
make test

# Test WebAssembly bindings (21s)
make test-bindings-js

# Build CLI compiler (21s)
cargo build --release --bin tlang_cli_js

# Build interpreter (30s)
cargo build --release --features=binary --bin tlangdi

# Build playground (13s)
npm run build
```

**Note**: For development, you can use `cargo build` (without `--release`) for faster builds, but the binaries will be in `target/debug/` instead of `target/release/`.

### Code Quality

Before committing changes, always run:

```bash
# Install required Rust components
rustup component add clippy rustfmt

# Fix linting issues and format code
cargo clippy --fix --allow-dirty --lib --all-features
cargo clippy --fix --allow-dirty --tests --all-features
cargo fmt

# JavaScript/TypeScript linting
npm run lint        # ~4s
npm run typecheck   # ~0.3s
```

## Usage

### Command Line Interface

Compile tlang to JavaScript:
```bash
./target/release/tlang_cli_js input.tlang --output-type js
```

Output options:
- `--output-type js` - Generate JavaScript (default)
- `--output-type hir` - Show High-level IR
- `--output-type ast` - Show Abstract Syntax Tree

### Interpreter

Run tlang directly:
```bash
./target/release/tlangdi input.tlang
```

### Web Playground

Start the development server:
```bash
npm run dev  # Starts server on http://localhost:5173/
```

The playground provides:
- Live code editing with syntax highlighting
- Instant compilation and execution
- Output display for JavaScript, HIR, and AST
- Error diagnostics and highlighting

## Project Structure

- **`crates/`** - Rust workspace with compiler components:
  - `tlang_lexer` - Tokenization and lexical analysis
  - `tlang_parser` - Parse tree generation
  - `tlang_ast` - Abstract syntax tree definitions
  - `tlang_semantics` - Semantic analysis and type checking
  - `tlang_hir` - High-level intermediate representation
  - `tlang_codegen_js` - JavaScript code generation
  - `tlang_interpreter` - Direct execution interpreter
  - `tlang_bindings_js` - WebAssembly bindings
- **`tests/`** - Integration tests organized by feature
- **`tlang-playground/`** - Web-based playground (Vite + TypeScript + Lit)
- **`packages/`** - Shared TypeScript packages

## Contributing

This project follows semantic commit conventions. Use prefixes like:
- `feat:` - New language features
- `fix:` - Bug fixes
- `docs:` - Documentation changes
- `test:` - Test additions/changes
- `refactor:` - Code restructuring

## License

See the repository for license information.
