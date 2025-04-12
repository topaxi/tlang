use insta::assert_snapshot;

mod common;

use self::common::{hir_from_str, pretty_print};

#[test]
fn test_lower_for_loop_on_list_simple() {
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
                    Option::Some? { 0: i } => {
                        log?(i);
                    },
                    Option::None? => {
                        break
                    },
                }
            }
        }
    };
    "###);
}

#[test]
fn test_lower_for_loop_on_list_with_accumulator() {
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
            let accumulator$$: unknown = 0;
            loop {
                let acc: unknown = accumulator$$;
                (accumulator$$ = match iterator$$.next() {
                    Option::Some? { 0: i } => {
                        (acc + i)
                    },
                    Option::None? => {
                        break accumulator$$
                    },
                })
            }
        };
        log?(sum);
    };
    "###);
}
