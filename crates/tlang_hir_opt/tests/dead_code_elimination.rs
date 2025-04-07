use insta::assert_snapshot;
use tlang_hir_opt::dead_code_elimination::DeadCodeEliminator;

mod common;

#[test]
fn test_simple_dead_code_removal() {
    let _ = env_logger::builder().is_test(true).try_init();

    let source = r#"
        let x = 1;
        let y = 2;
        let z = 3;
        y;
    "#;

    let module = common::compile_with_passes(source, vec![Box::new(DeadCodeEliminator::default())]);

    assert_snapshot!(common::pretty_print(&module), @r###"
        let y = 2;
        y;
    "###);
}

#[test]
fn test_binary_search_no_dead_code() {
    let _ = env_logger::builder().is_test(true).try_init();

    let source = r#"
        let mid = math::floor((low + high) / 2);
        let midValue = list[mid];

        if midValue == target; {
            mid
        } else if midValue < target; {
            mid + 1
        } else {
            mid - 1
        }
    "#;

    let module = common::compile_with_passes(source, vec![Box::new(DeadCodeEliminator::default())]);

    assert_snapshot!(common::pretty_print(&module), @r###"
        let mid: unknown = math::floor(((low + high) / 2));
        let midValue: unknown = list[mid];
        if (midValue == target) {
            mid
        } else if (midValue < target) {
            (mid + 1)
        } else {
            (mid - 1)
        };
    "###);
}

#[test]
fn test_range_with_guard_expressions() {
    let source = r#"
        fn range(start, end) { range(start, end, []) }
        fn range(start, end, acc) if start >= end; { acc }
        fn range(start, end, acc) { rec range(start + 1, end, [...acc, start]) }

        range(0, 5);
    "#;

    let module = common::compile_with_passes(source, vec![Box::new(DeadCodeEliminator::default())]);

    assert_snapshot!(common::pretty_print(&module), @r###"
    fn range$$2(start: unknown, end: unknown) -> unknown {
        range$$3(start, end, [])
    };
    fn range$$3(start: unknown, end: unknown, acc: unknown) -> unknown {
        match [start, end, acc] {
            [start, end, acc] if (start >= end) => {
                acc
            },
            [start, end, acc] => {
                rec range$$3((start + 1), end, [...acc, start])
            },
        }
    };
    dyn fn range
        -> range$$2
        -> range$$3;
    range$$2(0, 5);
    "###);
}

#[test]
fn test_nested_function_captures() {
    let source = r#"
        let x = 1;
        let y = 2;
        let z = 3;

        fn outer() {
            let a = 4;
            fn middle() {
                let b = 5;
                fn inner() {
                    // x is captured from 3 levels up
                    // a is captured from 2 levels up
                    // b is captured from 1 level up
                    x + a + b
                }
                inner()
            }
            middle()
        }

        outer();
    "#;

    let module = common::compile_with_passes(source, vec![Box::new(DeadCodeEliminator::default())]);

    // The final code should preserve x, a, and b with correct depths
    assert_snapshot!(common::pretty_print(&module), @r###"
    let x: unknown = 1;
    let y: unknown = 2;
    let z: unknown = 3;
    fn outer() -> unknown {
        let a: unknown = 4;
        fn middle() -> unknown {
            let b: unknown = 5;
            fn inner() -> unknown {
                ((x + a) + b)
            };
            inner()
        };
        middle()
    };
    outer();
    "###);
}
