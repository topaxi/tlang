mod common;

use pretty_assertions::assert_eq;
use tlang_hir::hir;

use self::common::collect_paths;

fn optimizer() -> tlang_hir_opt::HirOptimizer {
    tlang_hir_opt::HirOptimizer::new(vec![
        Box::new(tlang_hir_opt::symbol_resolution::SymbolResolution::default()),
        Box::new(tlang_hir_opt::slot_allocation::SlotAllocator::default()),
    ])
}

fn compile(source: &str) -> hir::Module {
    common::compile_and_optimize(source, &mut optimizer())
}

fn collect_slots(hir: &mut hir::Module) -> Vec<(String, hir::Slot)> {
    collect_paths(hir)
        .iter()
        .map(|path| (path.to_string(), path.res.slot()))
        .filter(|(_, slot)| *slot != hir::Slot::None)
        .collect::<Vec<_>>()
}

#[test]
fn test_fn_param_assigns_resolution_to_paths_referring_to_same_fn() {
    let mut hir = compile(
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
    assert_eq!(paths[1].res.slot(), hir::Slot::Local(1));
    // path 2 is the second `a` in the binary expression, it points to the a fn param (hir 2)
    assert_eq!(paths[2].res.slot(), hir::Slot::Local(1));

    // foo_a has one local slot, for a
    //match hir.block.stmts[0].kind {
    //    hir::StmtKind::FunctionDeclaration(ref fn_decl) => {
    //        assert_eq!(fn_decl.locals(), 2);
    //        assert_eq!(fn_decl.upvars(), 0);
    //    }
    //    _ => unreachable!(),
    //}

    // path 3 is foo_b within the fn declaration

    // path 4 is the first `a` in the binary expression, it points to the a fn param (hir 9)
    assert_eq!(paths[4].res.slot(), hir::Slot::Local(1));
    // path 5 is the second `b` in the binary expression, it points to the b fn param (hir 10)
    assert_eq!(paths[5].res.slot(), hir::Slot::Local(2));

    // foo_b has two local slots, for a and b
    //match hir.block.stmts[1].kind {
    //    hir::StmtKind::FunctionDeclaration(ref fn_decl) => {
    //        assert_eq!(fn_decl.locals(), 3);
    //        assert_eq!(fn_decl.upvars(), 0);
    //    }
    //    _ => unreachable!(),
    //}
}

#[test]
fn test_self_referal_reserves_local_slot() {
    let mut hir = compile(
        r#"
            fn foo(a) {
                foo(a)
            }
        "#,
    );
    let paths = collect_paths(&mut hir);

    // path 0 is the function declaration name `foo`

    // path 1 is the `a` in the call expression, it points to the a fn param (hir 2)
    assert_eq!(paths[1].res.slot(), hir::Slot::Local(1));

    // path 2 is `foo` of the call expression, it points to the fn declaration (hir 3)
    assert_eq!(paths[2].res.slot(), hir::Slot::Local(0));

    // foo has two local slots, for a and the fn itself
    //match hir.block.stmts[0].kind {
    //    hir::StmtKind::FunctionDeclaration(ref fn_decl) => {
    //        assert_eq!(fn_decl.locals(), 2);
    //        assert_eq!(fn_decl.upvars(), 0);
    //    }
    //    _ => unreachable!(),
    //}
}

#[test]
fn test_variadic_fn_res() {
    let mut hir = compile(
        r#"
            fn fac(n) { fac(n, 1) }
            fn fac(0, acc) { acc }
            fn fac(n, acc) { rec fac(n - 1, n * acc) }
        "#,
    );

    assert_eq!(
        collect_slots(&mut hir),
        vec![
            // fac/1 definition on line 1
            ("fac/1".to_string(), hir::Slot::Local(0)),
            ("n".to_string(), hir::Slot::Local(1)),
            ("fac/2".to_string(), hir::Slot::Upvar(1, 1)),
            // fac/2 definition on line 2
            ("fac/2".to_string(), hir::Slot::Local(1)),
            // match [n, acc]
            ("n".to_string(), hir::Slot::Local(1)),
            ("acc".to_string(), hir::Slot::Local(2)),
            // first match returns acc
            ("acc".to_string(), hir::Slot::Local(0)),
            // second match, calls fac(n - 1, n * acc)
            ("n".to_string(), hir::Slot::Local(0)),
            ("n".to_string(), hir::Slot::Local(0)),
            ("acc".to_string(), hir::Slot::Local(1)),
            ("fac/2".to_string(), hir::Slot::Upvar(0, 1)),
            // the dynamic dispatch definition
            ("fac".to_string(), hir::Slot::Local(2)),
        ]
    );

    //match hir.block.stmts[0].kind {
    //    hir::StmtKind::FunctionDeclaration(ref fn_decl) => {
    //        assert_eq!(fn_decl.locals(), 2);
    //        assert_eq!(fn_decl.upvars(), 1);
    //    }
    //    _ => unreachable!(),
    //}

    //match hir.block.stmts[1].kind {
    //    hir::StmtKind::FunctionDeclaration(ref fn_decl) => {
    //        assert_eq!(fn_decl.locals(), 3);
    //        assert_eq!(fn_decl.upvars(), 0);
    //    }
    //    _ => unreachable!(),
    //}
}

#[test]
fn test_variadic_fn_res_with_list_pat() {
    let mut hir = compile(
        r#"
            fn rs(str) { rs(str, "") }
            fn rs("", acc) { acc }
            fn rs([x, ...xs], acc) { rec rs(xs, x + acc) }
        "#,
    );

    assert_eq!(
        collect_slots(&mut hir),
        vec![
            // rs/1 definition on line 1
            ("rs/1".to_string(), hir::Slot::Local(0)),
            ("str".to_string(), hir::Slot::Local(1)),
            ("rs/2".to_string(), hir::Slot::Upvar(1, 1)),
            // rs/2 definition on line 2
            ("rs/2".to_string(), hir::Slot::Local(1)),
            // match [str, acc]
            ("str".to_string(), hir::Slot::Local(1)),
            ("acc".to_string(), hir::Slot::Local(2)),
            // first match returns acc
            ("acc".to_string(), hir::Slot::Local(0)),
            // second match, calls rs(xs, x + acc)
            ("xs".to_string(), hir::Slot::Local(1)),
            ("x".to_string(), hir::Slot::Local(0)),
            ("acc".to_string(), hir::Slot::Local(2)),
            ("rs/2".to_string(), hir::Slot::Upvar(0, 1)),
            // the dynamic dispatch definition
            ("rs".to_string(), hir::Slot::Local(2)),
        ]
    );

    //match hir.block.stmts[0].kind {
    //    hir::StmtKind::FunctionDeclaration(ref fn_decl) => {
    //        assert_eq!(fn_decl.locals(), 2);
    //        assert_eq!(fn_decl.upvars(), 1);
    //    }
    //    _ => unreachable!(),
    //}

    //match hir.block.stmts[1].kind {
    //    hir::StmtKind::FunctionDeclaration(ref fn_decl) => {
    //        assert_eq!(fn_decl.locals(), 3);
    //        assert_eq!(fn_decl.upvars(), 0);
    //    }
    //    _ => unreachable!(),
    //}
}

#[test]
fn test_shadowing_creates_new_slots() {
    let mut hir = compile(
        r#"
            fn foo(a) {
                let a = a + 1;
                a + a
            }
        "#,
    );

    assert_eq!(
        collect_slots(&mut hir),
        vec![
            ("foo".to_string(), hir::Slot::Local(0)),
            ("a".to_string(), hir::Slot::Local(1)),
            ("a".to_string(), hir::Slot::Local(2)),
            ("a".to_string(), hir::Slot::Local(2)),
        ]
    );
}

#[test]
#[ignore = "struct definitions do allocate slots (yet?)"]
fn test_struct_res() {
    let mut hir = compile(
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

    assert_eq!(
        collect_slots(&mut hir),
        vec![
            ("Foo".to_string(), hir::Slot::Local(0)), // Foo in fn Foo.new
            ("Foo".to_string(), hir::Slot::Upvar(0, 1)), // Foo creation Foo { .. }
            ("foo".to_string(), hir::Slot::Local(2)), // foo definition itself
            ("Foo".to_string(), hir::Slot::Upvar(0, 1)), // Foo.new acess
        ],
    );
}

#[test]
fn test_enum_variant_res() {
    let mut hir = compile(
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

    assert_eq!(
        collect_slots(&mut hir),
        vec![
            ("foo".to_string(), hir::Slot::Local(2)),
            ("Foo::Baz".to_string(), hir::Slot::Upvar(1, 1))
        ]
    );

    //match hir.block.stmts[1].kind {
    //    hir::StmtKind::FunctionDeclaration(ref fn_decl) => {
    //        assert_eq!(fn_decl.locals(), 1); // one, fns have a slot for themselves
    //        assert_eq!(fn_decl.upvars(), 1);
    //    }
    //    _ => unreachable!(),
    //}
}

#[test]
fn test_tagged_enum_res() {
    let mut hir = compile(
        r#"
            enum Foo {
                Bar(u32),
                Baz(u32),
            }

            fn Foo.qux(Foo::Bar(x)) { x }
            fn Foo.qux(Foo::Baz(x)) { x }

            fn bar() {
                Foo::Bar(1).qux()
            }
        "#,
    );

    assert_eq!(
        collect_slots(&mut hir),
        vec![
            ("foo".to_string(), hir::Slot::Local(1)), // the argument name of Foo.qux
            ("x".to_string(), hir::Slot::Local(0)),
            ("x".to_string(), hir::Slot::Local(0)),
            ("bar".to_string(), hir::Slot::Local(1)),
        ]
    );
}
