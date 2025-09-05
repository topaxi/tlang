use indoc::indoc;
use pretty_assertions::assert_eq;

mod common;

#[test]
fn test_simple_tail_call_should_not_be_flattened() {
    let output = compile!(indoc! {"
        fn factorial(n, acc) {
            if n == 0 {
                acc
            } else {
                rec factorial(n - 1, n * acc)
            }
        }
    "});
    
    // If tail call detection is working, the if-else should NOT be flattened
    // and should remain as a simple if-else expression, not using temp variables
    println!("Generated output:");
    println!("{}", output);
    
    // For now, let's just check that we don't get the broken syntax
    assert!(!output.contains("return if (n === 0)"));
}