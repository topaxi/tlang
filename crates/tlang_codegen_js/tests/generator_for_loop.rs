use indoc::indoc;
use insta::assert_snapshot;

mod common;

// For loops are lowered to normal loop expressions in HIR and should generate correct JavaScript

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

    assert_snapshot!(output, @r"
    function test_sum() {
        let sum = 0;
        {
            let iterator$$ = iterator.iter([1, 2, 3, 4, 5]);
            for (;;) {
                let $tmp$0 = iterator$$.next(),i;if ($tmp$0.tag === Option.Some && (i = $tmp$0[0], true)) {
                    sum = sum + i;
                } else if ($tmp$0.tag === Option.None) {
                    break;
                };
            }
        }
        return sum;
    }
    ");
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
    assert_snapshot!(output, @r"
    function test_sum() {
        let $hir$0 = undefined;
        {
            let iterator$$ = iterator.iter([1, 2, 3, 4, 5]);
            let accumulator$$ = 0;
            let $hir$1 = undefined;
            for (;;) {
                let sum = accumulator$$;
                let $hir$2 = undefined;
                let $tmp$0 = iterator$$.next(),i;if ($tmp$0.tag === Option.Some && (i = $tmp$0[0], true)) {
                    $hir$2 = sum + i;
                } else if ($tmp$0.tag === Option.None) {
                    $hir$2 = accumulator$$;
                    break;
                };
                accumulator$$ = $hir$2;
                $hir$1 = $hir$2;
            }
            $hir$0 = $hir$1;
        };
        return $hir$0;
    }
    ");
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
    assert_snapshot!(output, @r"
    function separate_even_odd() {
        let $hir$0 = undefined;
        {
            let iterator$$ = iterator.iter([1, 2, 3, 4]);
            let accumulator$$ = [[], []];
            let $hir$1 = undefined;
            for (;;) {
                let [even, odd] = accumulator$$;
                let $hir$2 = undefined;
                let $tmp$0 = iterator$$.next(),n;if ($tmp$0.tag === Option.Some && (n = $tmp$0[0], true)) {
                    $hir$2 = (n % 2 === 0 ? [[...even, n], odd] : [even, [...odd, n]]);
                } else if ($tmp$0.tag === Option.None) {
                    $hir$2 = accumulator$$;
                    break;
                };
                accumulator$$ = $hir$2;
                $hir$1 = $hir$2;
            }
            $hir$0 = $hir$1;
        };
        return $hir$0;
    }
    ");
}

#[test]
fn test_for_loop_nested() {
    let output = compile!(indoc! {"
        fn matrix_sum() {
            let total = 0;
            for row in [[1, 2], [3, 4]] {
                for val in row {
                    total = total + val
                }
            }
            total
        }
    "});

    // Expected: Should handle nested for loops correctly
    assert_snapshot!(output, @r"
    function matrix_sum() {
        let total = 0;
        {
            let iterator$$ = iterator.iter([[1, 2], [3, 4]]);
            for (;;) {
                let $tmp$0 = iterator$$.next(),row;if ($tmp$0.tag === Option.Some && (row = $tmp$0[0], true)) {
                    {
                        let iterator$$ = iterator.iter(row);
                        for (;;) {
                            let $hir$0 = undefined;
                            let $hir$1 = undefined;
                            let $tmp$1 = iterator$$.next(),val;if ($tmp$1.tag === Option.Some && (val = $tmp$1[0], true)) {
                                total = total + val;
                            } else if ($tmp$1.tag === Option.None) {
                                break;
                            };
                            $hir$1;
                            $hir$0;
                        }
                    }
                } else if ($tmp$0.tag === Option.None) {
                    break;
                };
            }
        }
        return total;
    }
    ");
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
    assert_snapshot!(output, @r#"
    function char_count() {
        let $hir$0 = undefined;
        {
            let iterator$$ = iterator.iter("hello");
            let accumulator$$ = 0;
            let $hir$1 = undefined;
            for (;;) {
                let count = accumulator$$;
                let $hir$2 = undefined;
                let $tmp$0 = iterator$$.next(),char;if ($tmp$0.tag === Option.Some && (char = $tmp$0[0], true)) {
                    $hir$2 = count + 1;
                } else if ($tmp$0.tag === Option.None) {
                    $hir$2 = accumulator$$;
                    break;
                };
                accumulator$$ = $hir$2;
                $hir$1 = $hir$2;
            }
            $hir$0 = $hir$1;
        };
        return $hir$0;
    }
    "#);
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
    assert_snapshot!(output, @r"
    function range_sum() {
        let $hir$0 = undefined;
        {
            let iterator$$ = iterator.iter(0);
            let accumulator$$ = 0;
            let $hir$1 = undefined;
            for (;;) {
                let sum = accumulator$$;
                let $hir$2 = undefined;
                let $tmp$0 = iterator$$.next(),i;if ($tmp$0.tag === Option.Some && (i = $tmp$0[0], true)) {
                    $hir$2 = sum + i;
                } else if ($tmp$0.tag === Option.None) {
                    $hir$2 = accumulator$$;
                    break;
                };
                accumulator$$ = $hir$2;
                $hir$1 = $hir$2;
            }
            $hir$0 = $hir$1;
        };
        return $hir$0;
    }
    ");
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
    assert_snapshot!(output, @r"
    function sum_list(list) {
        let $hir$0 = undefined;
        {
            let iterator$$ = iterator.iter(list);
            let accumulator$$ = 0;
            let $hir$1 = undefined;
            for (;;) {
                let sum = accumulator$$;
                let $hir$2 = undefined;
                let $tmp$0 = iterator$$.next(),item;if ($tmp$0.tag === Option.Some && (item = $tmp$0[0], true)) {
                    $hir$2 = sum + item;
                } else if ($tmp$0.tag === Option.None) {
                    $hir$2 = accumulator$$;
                    break;
                };
                accumulator$$ = $hir$2;
                $hir$1 = $hir$2;
            }
            $hir$0 = $hir$1;
        };
        return $hir$0;
    }
    ");
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
    assert_snapshot!(output, @r"
    function get_numbers() {
        return [1, 2, 3];
    }
    function sum_from_function() {
        let $hir$0 = undefined;
        {
            let iterator$$ = iterator.iter(get_numbers());
            let accumulator$$ = 0;
            let $hir$1 = undefined;
            for (;;) {
                let sum = accumulator$$;
                let $hir$2 = undefined;
                let $tmp$0 = iterator$$.next(),num;if ($tmp$0.tag === Option.Some && (num = $tmp$0[0], true)) {
                    $hir$2 = sum + num;
                } else if ($tmp$0.tag === Option.None) {
                    $hir$2 = accumulator$$;
                    break;
                };
                accumulator$$ = $hir$2;
                $hir$1 = $hir$2;
            }
            $hir$0 = $hir$1;
        };
        return $hir$0;
    }
    ");
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
    assert_snapshot!(output, @r"
    function test() {
        let $hir$0 = undefined;
        {
            let iterator$$ = iterator.iter([1, 2, 3]);
            let accumulator$$ = 0;
            let $hir$1 = undefined;
            for (;;) {
                let acc = accumulator$$;
                let $hir$2 = undefined;
                let $tmp$0 = iterator$$.next(),i;if ($tmp$0.tag === Option.Some && (i = $tmp$0[0], true)) {
                    $hir$2 = acc + i;
                } else if ($tmp$0.tag === Option.None) {
                    $hir$2 = accumulator$$;
                    break;
                };
                accumulator$$ = $hir$2;
                $hir$1 = $hir$2;
            }
            $hir$0 = $hir$1;
        };
        let result = $hir$0;
        return result;
    }
    ");
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
    assert_snapshot!(output, @r"
    function test() {
        let $hir$0 = undefined;
        {
            let iterator$$ = iterator.iter([1, 2, 3]);
            let accumulator$$ = 0;
            let $hir$1 = undefined;
            for (;;) {
                let acc = accumulator$$;
                let $hir$2 = undefined;
                let $tmp$0 = iterator$$.next(),i;if ($tmp$0.tag === Option.Some && (i = $tmp$0[0], true)) {
                    $hir$2 = acc + i;
                } else if ($tmp$0.tag === Option.None) {
                    $hir$2 = accumulator$$;
                    break;
                };
                accumulator$$ = $hir$2;
                $hir$1 = $hir$2;
            }
            $hir$0 = $hir$1;
        };
        return $hir$0;
    }
    ");
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
    assert_snapshot!(output, @r"
    function process_pairs() {
        let $hir$0 = undefined;
        {
            let iterator$$ = iterator.iter([[1, 2], [3, 4], [5, 6]]);
            let accumulator$$ = 0;
            let $hir$1 = undefined;
            for (;;) {
                let sum = accumulator$$;
                let $hir$2 = undefined;
                let $tmp$0 = iterator$$.next(),a,b;if ($tmp$0.tag === Option.Some && $tmp$0[0].length >= 2 && (a = $tmp$0[0][0], true) && (b = $tmp$0[0][1], true)) {
                    $hir$2 = sum + a + b;
                } else if ($tmp$0.tag === Option.None) {
                    $hir$2 = accumulator$$;
                    break;
                };
                accumulator$$ = $hir$2;
                $hir$1 = $hir$2;
            }
            $hir$0 = $hir$1;
        };
        return $hir$0;
    }
    ");
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
    assert_snapshot!(output, @r"
    function sum_evens() {
        let $hir$0 = undefined;
        {
            let iterator$$ = iterator.iter([1, 2, 3, 4, 5, 6]);
            let accumulator$$ = 0;
            let $hir$1 = undefined;
            for (;;) {
                let sum = accumulator$$;
                let $hir$2 = undefined;
                let $tmp$0 = iterator$$.next(),i;if ($tmp$0.tag === Option.Some && (i = $tmp$0[0], true)) {
                    $hir$2 = (i % 2 === 0 ? sum + i : sum);
                } else if ($tmp$0.tag === Option.None) {
                    $hir$2 = accumulator$$;
                    break;
                };
                accumulator$$ = $hir$2;
                $hir$1 = $hir$2;
            }
            $hir$0 = $hir$1;
        };
        return $hir$0;
    }
    ");
}
