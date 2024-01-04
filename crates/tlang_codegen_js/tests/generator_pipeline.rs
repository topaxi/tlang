use indoc::indoc;
use pretty_assertions::assert_eq;
use tlang_ast::symbols::SymbolType;
use tlang_codegen_js::generator::CodegenJS;
use tlang_parser::parser::Parser;
use tlang_semantics::SemanticAnalyzer;

macro_rules! compile {
    ($source:expr) => {{
        let mut parser = Parser::from_source($source);
        let mut ast = parser.parse();
        let mut semantic_analyzer = SemanticAnalyzer::default();
        semantic_analyzer.add_builtin_symbols(vec![
            ("log", SymbolType::Function),
            ("max", SymbolType::Function),
            ("min", SymbolType::Function),
        ]);
        semantic_analyzer.analyze(&mut ast);
        let mut codegen = CodegenJS::default();
        codegen.generate_code(&ast);
        codegen.get_output().to_string()
    }};
}

#[test]
fn test_pipeline_operator() {
    let output = compile!("fn main() { 1 |> log; }");
    let expected_output = indoc! {"
        function main() {
            console.log(1);
        }
    "};
    assert_eq!(output, expected_output);

    let output = compile!("fn main() { 1 |> min |> max; }");
    let expected_output = indoc! {"
        function main() {
            Math.max(Math.min(1));
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_pipeline_operator_with_call_parenthesis() {
    let output = compile!("fn main() { 1 |> max(); }");
    let expected_output = indoc! {"
        function main() {
            Math.max(1);
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_pipeline_operator_with_call_parenthesis_and_arguments() {
    let output = compile!("fn main() { 1 |> foo(2); }");
    let expected_output = indoc! {"
        function main() {
            foo(1, 2);
        }
    "};
    assert_eq!(output, expected_output);

    let output = compile!("fn main() { 1 |> foo(2, 3); }");
    let expected_output = indoc! {"
        function main() {
            foo(1, 2, 3);
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_pipeline_operator_to_function_call_with_wildcards() {
    let output = compile!("fn main() { 1 |> foo(2, _); }");
    let expected_output = indoc! {"
        function main() {
            foo(2, 1);
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_pipeline_operator_long_chaining() {
    let output = compile!(indoc! {"
        [1,2,3]
        |> map(fn (x) { x ** 2 })
        |> filter(fn (x) { x % 2 == 0 })
        |> foldl(fn (acc, x) { acc + x }, 0)
        |> log();
    "});
    let expected_output = indoc! {"
        console.log(foldl(filter(map([1, 2, 3], function(x) {
            return x ** 2;
        }), function(x) {
            return x % 2 === 0;
        }), function(acc, x) {
            return acc + x;
        }, 0));
    "};
    assert_eq!(output, expected_output);
}
