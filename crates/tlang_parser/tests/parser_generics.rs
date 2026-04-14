#[macro_use]
mod common;

#[test]
fn test_generic_function_declaration() {
    assert_parser_snapshot!(indoc::indoc! {"
        fn identity<T>(x: T) -> T { x }
    "});
}

#[test]
fn test_generic_function_multiple_type_params() {
    assert_parser_snapshot!(indoc::indoc! {"
        fn pair<T, U>(a: T, b: U) -> T {
            a
        }
    "});
}

#[test]
fn test_generic_struct_declaration() {
    assert_parser_snapshot!(indoc::indoc! {"
        struct Pair<T, U> {
            first: T,
            second: U,
        }
    "});
}

#[test]
fn test_generic_enum_declaration() {
    assert_parser_snapshot!(indoc::indoc! {"
        enum Result<T, E> {
            Ok(T),
            Err(E),
        }
    "});
}

#[test]
fn test_generic_protocol_declaration() {
    assert_parser_snapshot!(indoc::indoc! {"
        protocol Into<T> {
            fn into(self) -> T
        }
    "});
}

#[test]
fn test_protocol_associated_type_and_method_generics() {
    let ast = parse!(indoc::indoc! {"
        protocol Functor {
            type Wrapped<T, U>
            fn map<T, U>(self, f: fn(T) -> U) -> Wrapped<T, U>
        }
    "});

    let decl = match &ast.statements[0].kind {
        tlang_ast::node::StmtKind::ProtocolDeclaration(decl) => decl,
        other => panic!("expected ProtocolDeclaration, got {other:?}"),
    };

    assert_eq!(decl.associated_types.len(), 1);
    assert_eq!(decl.associated_types[0].name.as_str(), "Wrapped");
    assert_eq!(decl.associated_types[0].type_params.len(), 2);
    assert_eq!(decl.methods.len(), 1);
    assert_eq!(decl.methods[0].name.as_str(), "map");
    assert_eq!(decl.methods[0].type_params.len(), 2);
}

#[test]
fn test_impl_type_params_where_clause_and_associated_type_binding() {
    let ast = parse!(indoc::indoc! {"
        impl<T, U, I, F> Functor for I
        where
            I: Iterator<T> + Display,
            F: fn(T) -> U
        {
            type Wrapped<V> = MappedIterator<T, U, V>
            fn map(self, f) { self }
        }
    "});

    let decl = match &ast.statements[0].kind {
        tlang_ast::node::StmtKind::ImplBlock(decl) => decl,
        other => panic!("expected ImplBlock, got {other:?}"),
    };

    assert_eq!(decl.type_params.len(), 4);
    assert_eq!(decl.associated_types.len(), 1);
    assert_eq!(decl.associated_types[0].name.as_str(), "Wrapped");
    assert_eq!(decl.associated_types[0].type_params.len(), 1);

    let where_clause = decl
        .where_clause
        .as_ref()
        .expect("expected where clause to be parsed");
    assert_eq!(where_clause.predicates.len(), 2);
    assert_eq!(where_clause.predicates[0].name.as_str(), "I");
    assert_eq!(where_clause.predicates[0].bounds.len(), 2);
    assert_eq!(where_clause.predicates[1].name.as_str(), "F");
    assert_eq!(where_clause.predicates[1].bounds.len(), 1);
}

#[test]
fn test_protocol_phase1_syntax_parses() {
    assert_parses!(indoc::indoc! {"
        pub protocol Functor {
            type Wrapped<T, U>
            fn map<T, U>(self, f: fn(T) -> U) -> Wrapped<T, U>
        }

        impl<T, U, I> Functor for I
        where
            I: Iterator<T>
        {
            type Wrapped = MappedIterator<T, U>
            fn map(self, f) { self }
        }
    "});
}

#[test]
fn test_generic_function_single_bound() {
    assert_parser_snapshot!(indoc::indoc! {"
        fn sort<T: Ord>(list: List<T>) -> List<T> { list }
    "});
}

#[test]
fn test_generic_function_multiple_bounds() {
    assert_parser_snapshot!(indoc::indoc! {"
        fn serialize<T: Display + Serialize>(value: T) -> String { value }
    "});
}

#[test]
fn test_generic_function_mixed_bound_and_unbound() {
    let ast = parse!(indoc::indoc! {"
        fn transform<T: Ord, U>(input: T) -> U { input }
    "});

    let decl = match &ast.statements[0].kind {
        tlang_ast::node::StmtKind::FunctionDeclaration(decl) => decl,
        other => panic!("expected FunctionDeclaration, got {other:?}"),
    };

    assert_eq!(decl.type_params.len(), 2);
    // T has one bound: Ord
    assert_eq!(decl.type_params[0].name.as_str(), "T");
    assert_eq!(decl.type_params[0].bounds.len(), 1);
    // U has no bounds
    assert_eq!(decl.type_params[1].name.as_str(), "U");
    assert!(decl.type_params[1].bounds.is_empty());
}

#[test]
fn test_generic_struct_with_bounds() {
    assert_parser_snapshot!(indoc::indoc! {"
        struct SortedPair<T: Ord> {
            first: T,
            second: T,
        }
    "});
}
