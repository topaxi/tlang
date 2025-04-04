mod common;

use pretty_assertions::{assert_eq, assert_matches};
use tlang_hir::hir::{self, DefKind, HirScope};

use self::common::{collect_paths, hir_from_str};

#[test]
fn test_fn_param_assigns_resolution_to_paths_referring_to_same_fn() {
    let mut hir = hir_from_str(
        r#"
            fn foo_a(a) {
                a + a
            }
            fn foo_b(a, b) {
                a + b
            }
        "#,
    );
    let paths = collect_paths(&mut hir);

    // path 0 is foo_a within the fn declaration

    // path 1 is the first `a` in the binary expression, it points to the a fn param (hir 2)
    assert_eq!(paths[1].res, hir::Res::Local(hir::HirId::new(3), 1));
    // path 2 is the second `a` in the binary expression, it points to the a fn param (hir 2)
    assert_eq!(paths[2].res, hir::Res::Local(hir::HirId::new(3), 1));

    // foo_a has one local slot, for a
    match hir.block.stmts[0].kind {
        hir::StmtKind::FunctionDeclaration(ref fn_decl) => {
            assert_eq!(fn_decl.locals(), 2);
            assert_eq!(fn_decl.upvars(), 0);
        }
        _ => unreachable!(),
    }

    // path 3 is foo_b within the fn declaration

    // path 4 is the first `a` in the binary expression, it points to the a fn param (hir 9)
    assert_eq!(paths[4].res, hir::Res::Local(hir::HirId::new(10), 1));
    // path 5 is the second `b` in the binary expression, it points to the b fn param (hir 10)
    assert_eq!(paths[5].res, hir::Res::Local(hir::HirId::new(11), 2));

    // foo_b has two local slots, for a and b
    match hir.block.stmts[1].kind {
        hir::StmtKind::FunctionDeclaration(ref fn_decl) => {
            assert_eq!(fn_decl.locals(), 3);
            assert_eq!(fn_decl.upvars(), 0);
        }
        _ => unreachable!(),
    }
}

#[test]
fn test_self_referal_reserves_local_slot() {
    let mut hir = hir_from_str(
        r#"
            fn foo(a) {
                foo(a)
            }
        "#,
    );
    let paths = collect_paths(&mut hir);

    // path 0 is foo within the fn declaration

    // path 1 is `foo` of the call expression, it points to the fn declaration (hir 2)
    assert_eq!(
        paths[1].res,
        hir::Res::Def(hir::DefKind::Fn, hir::HirId::new(1), 0)
    );
    // path 2 is the `a` in the call expression, it points to the a fn param (hir 3)
    assert_eq!(paths[2].res, hir::Res::Local(hir::HirId::new(3), 1));

    // foo has two local slots, for a and the fn itself
    match hir.block.stmts[0].kind {
        hir::StmtKind::FunctionDeclaration(ref fn_decl) => {
            assert_eq!(fn_decl.locals(), 2);
            assert_eq!(fn_decl.upvars(), 0);
        }
        _ => unreachable!(),
    }
}

#[test]
fn test_enum_variant_res() {
    let mut hir = hir_from_str(
        r#"
            enum Foo {
                Bar,
                Baz,
            }

            fn foo() {
                Foo::Baz
            }
        "#,
    );
    let paths = collect_paths(&mut hir);

    assert_eq!(paths[1].res, hir::Res::Upvar(1, 1));

    match hir.block.stmts[1].kind {
        hir::StmtKind::FunctionDeclaration(ref fn_decl) => {
            assert_eq!(fn_decl.locals(), 1); // one, fns have a slot for themselves
            assert_eq!(fn_decl.upvars(), 1);
        }
        _ => unreachable!(),
    }
}

#[test]
fn test_variadic_fn_res() {
    let mut hir = hir_from_str(
        r#"
            fn fac(n) { fac(n, 1) }
            fn fac(0, acc) { acc }
            fn fac(n, acc) { rec fac(n - 1, n * acc) }
        "#,
    );
    let paths = collect_paths(&mut hir);

    assert_matches!(paths[1].res, hir::Res::Upvar(1, 1));
    assert_matches!(paths[2].res, hir::Res::Local(_, 1));

    match hir.block.stmts[0].kind {
        hir::StmtKind::FunctionDeclaration(ref fn_decl) => {
            assert_eq!(fn_decl.locals(), 2);
            assert_eq!(fn_decl.upvars(), 1);
        }
        _ => unreachable!(),
    }

    // match [n/*1*/, acc/*2*/] in the second function declaration
    assert_matches!(paths[4].res, hir::Res::Local(_, 1));
    assert_matches!(paths[5].res, hir::Res::Local(_, 2));

    // [0, acc] => acc/*0*/
    assert_matches!(paths[6].res, hir::Res::Local(_, 0));
    // [n, acc] => rec factorial$$2((n - 1), (n * acc))
    assert_matches!(paths[7].res, hir::Res::Upvar(1, 0));
    assert_matches!(paths[8].res, hir::Res::Local(_, 0));
    assert_matches!(paths[9].res, hir::Res::Local(_, 0));
    assert_matches!(paths[10].res, hir::Res::Local(_, 1));

    match hir.block.stmts[1].kind {
        hir::StmtKind::FunctionDeclaration(ref fn_decl) => {
            assert_eq!(fn_decl.locals(), 3);
            assert_eq!(fn_decl.upvars(), 0);
        }
        _ => unreachable!(),
    }
}

#[test]
fn test_shadowing_creates_new_slots() {
    let mut hir = hir_from_str(
        r#"
            fn foo(a) {
                let a = a + 1;
                a + a
            }
        "#,
    );
    let paths = collect_paths(&mut hir);

    // path 0 is foo within the fn declaration

    // path 1 is `a` in the fn declaration, it points to the a fn param (hir 2)
    assert_eq!(paths[1].res, hir::Res::Local(hir::HirId::new(3), 1));
    // path 2 is the first `a` in the binary expression, it points to the shadowed a (hir 3)
    assert_eq!(paths[2].res, hir::Res::Local(hir::HirId::new(7), 2));
    assert_eq!(paths[3].res, hir::Res::Local(hir::HirId::new(7), 2));
}

#[test]
fn test_struct_res() {
    let mut hir = hir_from_str(
        r#"
            struct Foo {
                a: i32,
                b: i32,
            }

            fn Foo.new() {
                Foo { a: 1, b: 2 }
            }

            fn foo() {
                Foo.new()
            }
        "#,
    );

    let paths = collect_paths(&mut hir);

    assert_eq!(
        paths[0].res,
        hir::Res::Def(DefKind::Struct, hir::HirId::new(1), 0)
    );

    assert_eq!(
        paths[1].res,
        hir::Res::Def(DefKind::Struct, hir::HirId::new(1), 0)
    );

    assert_eq!(
        paths[5].res,
        hir::Res::Def(DefKind::Struct, hir::HirId::new(1), 0),
        "path {}",
        paths[5].join(""),
    );
}

#[test]
#[ignore]
fn test_simple_enum_res() {
    let mut hir = hir_from_str(
        r#"
            enum Foo {
                Bar,
                Baz,
            }

            fn Foo.qux(Foo::Bar) { "bar" }
            fn Foo.qux(Foo::Baz) { "baz" }

            fn foo() {
                Foo::Bar.qux()
            }
        "#,
    );

    let paths = collect_paths(&mut hir);

    assert_eq!(
        paths[0].res,
        hir::Res::Def(DefKind::Enum, hir::HirId::new(1), 0)
    );

    assert_eq!(
        paths[3].res,
        hir::Res::Def(DefKind::Variant, hir::HirId::new(1), 0),
        "path {}",
        paths[3].join("::")
    );
}

#[test]
fn test_enum_res() {
    let mut hir = hir_from_str(
        r#"
            enum Foo {
                Bar(u32),
                Baz(u32),
            }

            fn Foo.qux(Foo::Bar(x)) { x }
            fn Foo.qux(Foo::Baz(x)) { x }

            fn foo() {
                Foo::Bar(1).qux()
            }
        "#,
    );

    let paths = collect_paths(&mut hir);

    assert_eq!(
        paths[0].res,
        hir::Res::Def(DefKind::Enum, hir::HirId::new(1), 0)
    );

    // Path 1 is `foo` as pattern matched functions move their arguments into a match expression
    // within their body.

    assert_eq!(
        paths[2].res,
        hir::Res::Def(DefKind::Variant, hir::HirId::new(2), 1),
        "path {}",
        paths[2].join("::")
    );

    // Path 3 is `x`

    assert_eq!(
        paths[4].res,
        hir::Res::Def(DefKind::Variant, hir::HirId::new(4), 2),
        "path {}",
        paths[4].join("::")
    );
}
