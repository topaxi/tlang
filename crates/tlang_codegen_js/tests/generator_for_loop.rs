use indoc::indoc;
use insta::assert_snapshot;

mod common;

// Note: These tests may initially fail as for loop codegen is not yet implemented
// They serve as specifications for the expected JavaScript output

#[test]
fn test_for_loop_simple_iteration() {
    let output = compile!(indoc! {"
        fn test_sum() {
            let sum = 0;
            for i in [1, 2, 3, 4, 5] {
                sum = sum + i;
            }
            sum
        }
    "});

    // Expected: Should generate JavaScript for loop or for...of with proper iteration
    assert_snapshot!(output, @"");
}

#[test]
fn test_for_loop_with_accumulator_simple() {
    let output = compile!(indoc! {"
        fn test_sum() {
            for i in [1, 2, 3, 4, 5] with sum = 0 {
                sum + i
            }
        }
    "});

    // Expected: Should generate JavaScript that accumulates values and returns final result
    assert_snapshot!(output, @"");
}

#[test]
fn test_for_loop_with_accumulator_pattern() {
    let output = compile!(indoc! {"
        fn separate_even_odd() {
            for n in [1, 2, 3, 4] with [even, odd] = [[], []] {
                if n % 2 == 0 {
                    [[...even, n], odd]
                } else {
                    [even, [...odd, n]]
                }
            }
        }
    "});

    // Expected: Should handle destructuring pattern in accumulator
    assert_snapshot!(output, @"");
}

#[test]
fn test_for_loop_nested() {
    let output = compile!(indoc! {"
        fn matrix_sum() {
            let total = 0;
            for row in [[1, 2], [3, 4]] {
                for val in row {
                    total = total + val;
                }
            }
            total
        }
    "});

    // Expected: Should handle nested for loops correctly
    assert_snapshot!(output, @"");
}

#[test]
fn test_for_loop_with_string_iteration() {
    let output = compile!(indoc! {r#"
        fn char_count() {
            for char in "hello" with count = 0 {
                count + 1
            }
        }
    "#});

    // Expected: Should handle string iteration (may need special handling)
    assert_snapshot!(output, @"");
}

#[test]
fn test_for_loop_with_range() {
    let output = compile!(indoc! {"
        fn range_sum() {
            for i in 1..5 with sum = 0 {
                sum + i
            }
        }
    "});

    // Expected: Should handle range iteration
    assert_snapshot!(output, @"");
}

#[test]
fn test_for_loop_with_variable_iterable() {
    let output = compile!(indoc! {"
        fn sum_list(list) {
            for item in list with sum = 0 {
                sum + item
            }
        }
    "});

    // Expected: Should handle variable as iterable
    assert_snapshot!(output, @"");
}

#[test]
fn test_for_loop_with_function_call_iterable() {
    let output = compile!(indoc! {"
        fn get_numbers() { [1, 2, 3] }
        
        fn sum_from_function() {
            for num in get_numbers() with sum = 0 {
                sum + num
            }
        }
    "});

    // Expected: Should handle function call as iterable
    assert_snapshot!(output, @"");
}

#[test]
fn test_for_loop_expression_in_let() {
    let output = compile!(indoc! {"
        fn test() {
            let result = for i in [1, 2, 3] with acc = 0 {
                acc + i
            };
            result
        }
    "});

    // Expected: Should handle for loop as expression in let statement
    assert_snapshot!(output, @"");
}

#[test]
fn test_for_loop_expression_in_return() {
    let output = compile!(indoc! {"
        fn test() {
            for i in [1, 2, 3] with acc = 0 {
                acc + i
            }
        }
    "});

    // Expected: Should handle for loop as expression in return
    assert_snapshot!(output, @"");
}

#[test]
fn test_for_loop_with_complex_pattern() {
    let output = compile!(indoc! {"
        fn process_pairs() {
            for [a, b] in [[1, 2], [3, 4], [5, 6]] with sum = 0 {
                sum + a + b
            }
        }
    "});

    // Expected: Should handle complex destructuring pattern in for loop
    assert_snapshot!(output, @"");
}

#[test]
fn test_for_loop_with_guards() {
    let output = compile!(indoc! {"
        fn sum_evens() {
            for i in [1, 2, 3, 4, 5, 6] with sum = 0 {
                if i % 2 == 0 {
                    sum + i
                } else {
                    sum
                }
            }
        }
    "});

    // Expected: Should handle conditional logic within for loop body
    assert_snapshot!(output, @"");
}
