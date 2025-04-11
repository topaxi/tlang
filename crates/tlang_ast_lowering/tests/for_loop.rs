use insta::assert_snapshot;

mod common;

use self::common::{hir_from_str, pretty_print};

#[test]
fn test_lower_for_loop() {
    let hir = hir_from_str(
        r#"
            fn main() {
                for i in [1, 2, 3] {
                    log(i);
                }
            }
        "#,
    );

    assert_snapshot!(pretty_print(&hir), @r###"
    fn main() -> unknown {
        {
            let iterator$$: unknown = [1, 2, 3].iter();
            loop {
                match iterator$$.next() {
                    Some? { 0: i } => {
                        log?(i);
                    },
                    None? => {
                        break
                    },
                }
            };
        }
    };
    "###);
}

#[test]
fn test_lower_for_loop_with_accumulator() {
    let hir = hir_from_str(
        r#"
            fn main() {
                let sum = for i in [1, 2, 3] with acc = 0; {
                    acc + i
                };

                log(sum);
            }
        "#,
    );

    assert_snapshot!(pretty_print(&hir), @r###"
    fn main() -> unknown {
        let sum: unknown = {
            let iterator$$: unknown = [1, 2, 3].iter();
            let accumulator$$: unknown = 0; // generalized case compatible with patterns
                                            // could optimize away intermediate variable
                                            // when `acc` is just a single ident path.
                                            // Could be done in an optimization pass though.
            loop {
                let acc = accumulator$$;
                accumulator$$ = match iterator$$.next() {
                    Some? { 0: i } => {
                        (acc + i)
                    },
                    None? => {
                        break
                    },
                }
            };
            accumulator$$
        };
        log?(sum);
    };
    "###);
}
