/**
 * @file tlang grammar for tree-sitter
 * @license MIT
 * @see {@link https://github.com/topaxi/tlang}
 */

// @ts-check
/// <reference types="tree-sitter-cli/dsl" />

/**
 * Operator precedences, matching the tlang parser's internal precedences.
 * Dict/struct calls have precedence 2, the lowest non-assignment level,
 * so binary operators always take precedence over struct field consumption.
 */
const PREC = {
  ASSIGN: 1,
  STRUCT: 2,
  PIPELINE: 3,
  OR: 4,
  AND: 5,
  REL: 6,
  PLUS: 7,
  TIMES: 8,
  BITWISE: 9,
  EXP: 10,
  UNARY: 11,
  CALL: 12,
  MEMBER: 13,
};

module.exports = grammar({
  name: 'tlang',

  word: ($) => $.identifier,

  extras: ($) => [/\s/, $.line_comment, $.block_comment],

  supertypes: ($) => [$._statement, $._expression, $._pattern, $._literal],

  inline: ($) => [$._statement, $._declaration, $._literal],

  conflicts: ($) => [
    // { ... } can be a block expression or an object expression
    [$.block, $.object_expression],
    // fn identifier( could start either a function_declaration or function_expression
    [$.function_name, $.function_expression],
    // { identifier } could be block with expression or object with shorthand property
    [$._expression, $.property_name],
    // enum_pattern may or may not have tuple arguments
    [$.enum_pattern],
    // enum_pattern vs struct_pattern: Foo::Bar could be start of either
    [$.enum_pattern, $.struct_pattern],
    // Foo::Bar could be start of path_expression or type_path (for struct_expression)
    [$.path_expression, $.type_path],
    // identifier followed by string: could be expression or tagged_string tag
    [$._expression, $.tagged_string],
  ],

  rules: {
    // =========================================================================
    // Top Level
    // =========================================================================

    source_file: ($) => repeat($._statement),

    // =========================================================================
    // Statements
    // =========================================================================

    _statement: ($) =>
      choice($._declaration, $.return_statement, $.expression_statement),

    _declaration: ($) =>
      choice(
        $.function_declaration,
        $.variable_declaration,
        $.enum_declaration,
        $.struct_declaration,
        $.protocol_declaration,
        $.impl_block,
      ),

    expression_statement: ($) => seq($._expression, optional(';')),

    return_statement: ($) =>
      prec.right(seq('return', optional($._expression), optional(';'))),

    // =========================================================================
    // Variable Declaration
    // =========================================================================

    variable_declaration: ($) =>
      seq('let', field('pattern', $.pattern_assign), optional(';')),

    // =========================================================================
    // Function Declaration
    // =========================================================================

    function_declaration: ($) =>
      prec.dynamic(
        1,
        seq(
          'fn',
          field('name', $.function_name),
          field('parameters', $.parameter_list),
          optional(field('guard', $.guard_clause)),
          optional(seq('->', field('return_type', $.type_annotation))),
          field('body', $.block),
        ),
      ),

    function_name: ($) =>
      seq(
        repeat(seq(field('namespace', $.type_identifier), '::')),
        field('definition', choice($.identifier, $.type_identifier)),
        optional(field('arity', $.function_arity)),
        optional(seq('.', field('method', $.property_name))),
      ),

    function_arity: ($) => seq('/', $.number),

    parameter_list: ($) => seq('(', commaSep($.function_parameter), ')'),

    function_parameter: ($) =>
      seq($._pattern, optional(seq(':', field('type', $.type_annotation)))),

    guard_clause: ($) =>
      seq('if', field('condition', $._expression), optional(';')),

    // =========================================================================
    // Enum Declaration
    // =========================================================================

    enum_declaration: ($) =>
      seq(
        'enum',
        field('name', $.type_identifier),
        '{',
        commaSep($.enum_variant),
        '}',
      ),

    enum_variant: ($) =>
      seq(
        field('name', $.type_identifier),
        optional(
          choice(
            seq('(', commaSep($.type_annotation), ')'),
            seq('{', commaSep($.struct_field), '}'),
          ),
        ),
      ),

    // =========================================================================
    // Struct Declaration
    // =========================================================================

    struct_declaration: ($) =>
      seq(
        'struct',
        field('name', $.type_identifier),
        '{',
        commaSep($.struct_field),
        '}',
      ),

    struct_field: ($) =>
      seq(
        field('name', $.identifier),
        optional(seq(':', field('type', $.type_annotation))),
      ),

    // =========================================================================
    // Protocol Declaration
    // =========================================================================

    protocol_declaration: ($) =>
      seq(
        'protocol',
        field('name', $.type_identifier),
        '{',
        repeat($.protocol_method_signature),
        '}',
      ),

    protocol_method_signature: ($) =>
      seq('fn', field('name', $.identifier), $.parameter_list),

    // =========================================================================
    // Impl Block
    // =========================================================================

    impl_block: ($) =>
      seq(
        'impl',
        field('protocol', $.type_identifier),
        'for',
        field('type', $.type_identifier),
        '{',
        repeat($.impl_item),
        '}',
      ),

    impl_item: ($) => choice($.function_declaration, $.apply_statement),

    apply_statement: ($) =>
      seq('apply', commaSep1(field('method', $.identifier)), optional(';')),



    // =========================================================================
    // Type Annotation
    // =========================================================================

    type_annotation: ($) =>
      seq(
        choice($.type_identifier, $.identifier),
        repeat(seq('::', choice($.type_identifier, $.identifier))),
        optional(seq('<', commaSep($.type_annotation), '>')),
      ),

    // =========================================================================
    // Patterns
    // =========================================================================

    _pattern: ($) =>
      choice(
        $.wildcard_pattern,
        $.literal_pattern,
        $.enum_pattern,
        $.struct_pattern,
        $.list_pattern,
        $.object_pattern,
        alias('self', $.self_pattern),
        alias($.identifier, $.binding),
      ),

    wildcard_pattern: (_) => '_',

    literal_pattern: ($) =>
      seq(optional('-'), choice($.number, $.string, $.boolean_literal)),

    // Requires at least one "::" to distinguish from plain identifier.
    // Type names (uppercase) must be used for enum paths.
    enum_pattern: ($) =>
      seq(
        $.type_identifier,
        repeat(seq('::', choice($.type_identifier, $.identifier))),
        optional(seq('(', commaSep($._pattern), ')')),
      ),

    // Requires at least one "::" to distinguish from plain identifier
    struct_pattern: ($) =>
      seq(
        $.type_identifier,
        repeat(seq('::', choice($.type_identifier, $.identifier))),
        '{',
        commaSep($.struct_pattern_field),
        '}',
      ),

    struct_pattern_field: ($) =>
      choice(
        seq('...', $._pattern),
        seq(
          field('name', $.property_name),
          ':',
          field('pattern', $._pattern),
          optional(seq('=', field('default', $._expression))),
        ),
        seq(
          field('shorthand', $.property_name),
          optional(seq('=', field('default', $._expression))),
        ),
      ),

    list_pattern: ($) =>
      seq(
        '[',
        optional(
          choice(
            // Only a rest binding: [...rest]
            seq('...', field('rest', $._pattern), optional(',')),
            // Plain patterns with optional trailing rest binding: [x, y, ...rest]
            seq(
              $._pattern,
              repeat(seq(',', $._pattern)),
              optional(seq(',', '...', field('rest', $._pattern))),
              optional(','),
            ),
          ),
        ),
        ']',
      ),

    object_pattern: ($) => seq('{', commaSep($.pattern_property), '}'),

    pattern_property: ($) =>
      choice(
        seq('...', $._pattern),
        seq(
          field('name', $.property_name),
          ':',
          field('pattern', $._pattern),
          optional(seq('=', field('default', $._expression))),
        ),
        seq(
          field('shorthand', $.property_name),
          optional(seq('=', field('default', $._expression))),
        ),
      ),

    pattern_assign: ($) =>
      seq(
        field('pattern', $._pattern),
        optional(seq('=', field('default', $._expression))),
      ),

    // =========================================================================
    // Expressions
    // =========================================================================

    _expression: ($) =>
      choice(
        $._literal,
        $.identifier,
        alias('self', $.self),
        alias('_', $.wildcard),
        $.path_expression,
        $.list_expression,
        $.object_expression,
        $.block,
        $.unary_expression,
        $.binary_expression,
        $.assignment_expression,
        $.call_expression,
        $.struct_expression,
        $.member_expression,
        $.index_expression,
        $.parenthesized_expression,
        $.function_expression,
        $.if_expression,
        $.match_expression,
        $.loop_expression,
        $.for_expression,
        $.rec_expression,
        $.break_expression,
        $.continue_expression,
      ),

    _literal: ($) => choice($.number, $.string, $.tagged_string, $.boolean_literal),

    tagged_string: ($) =>
      prec.dynamic(
        1,
        seq(
          field('tag', alias($.identifier, $.tagged_string_tag)),
          field('content', $.string),
        ),
      ),

    boolean_literal: (_) => choice('true', 'false'),

    path_expression: ($) =>
      seq(
        choice($.type_identifier, $.identifier),
        repeat1(seq('::', choice($.type_identifier, $.identifier))),
      ),

    // A type path consists of uppercase-only identifiers separated by "::".
    // Used for struct/enum type instantiation to avoid ambiguity with blocks.
    type_path: ($) =>
      seq($.type_identifier, repeat(seq('::', $.type_identifier))),

    list_expression: ($) =>
      seq('[', commaSep(choice(seq('...', $._expression), $._expression)), ']'),

    object_expression: ($) => seq('{', commaSep($.property), '}'),

    property: ($) =>
      choice(
        seq(field('key', $.property_name), ':', field('value', $._expression)),
        seq(
          '[',
          field('key', $._expression),
          ']',
          ':',
          field('value', $._expression),
        ),
        seq('...', field('value', $._expression)),
        field('shorthand', $.property_name),
      ),

    parenthesized_expression: ($) => seq('(', $._expression, ')'),

    unary_expression: ($) =>
      prec.right(
        PREC.UNARY,
        seq(
          field('operator', choice('!', 'not', '+', '-')),
          field('operand', $._expression),
        ),
      ),

    binary_expression: ($) => {
      /** @type {Array<[number, RuleOrLiteral, 'left'|'right']>} */
      const table = [
        [PREC.EXP, '**', 'right'],
        [PREC.BITWISE, choice('&', '|', '^'), 'left'],
        [PREC.TIMES, choice('*', '/', '%'), 'left'],
        [PREC.PLUS, choice('+', '-'), 'left'],
        [PREC.REL, choice('==', '!=', '=~', '!~', '<', '<=', '>', '>='), 'left'],
        [PREC.AND, choice('&&', 'and'), 'left'],
        [PREC.OR, choice('||', 'or'), 'left'],
        [PREC.PIPELINE, '|>', 'left'],
      ];
      return choice(
        ...table.map(([prec_level, operator, assoc]) => {
          const fn = assoc === 'right' ? prec.right : prec.left;
          return fn(
            prec_level,
            seq(
              field('left', $._expression),
              field('operator', operator),
              field('right', $._expression),
            ),
          );
        }),
      );
    },

    assignment_expression: ($) =>
      prec.right(
        PREC.ASSIGN,
        seq(
          field(
            'left',
            choice($.identifier, $.member_expression, $.index_expression),
          ),
          field('operator', choice('=', '+=', '-=', '*=', '/=', '%=', '**=')),
          field('right', $._expression),
        ),
      ),

    call_expression: ($) =>
      prec.left(
        PREC.CALL,
        seq(
          field('callee', $._expression),
          field('arguments', $.argument_list),
        ),
      ),

    // Struct/enum instantiation: Foo { x: 1 } or Foo::Bar { x: 1 }
    // Type must start with uppercase to avoid confusion with block expressions
    // that follow lowercase variable names in if/loop/for conditions.
    // Uses low precedence (STRUCT=2) matching the tlang parser's dict-call precedence.
    struct_expression: ($) =>
      prec.left(
        PREC.STRUCT,
        seq(field('type', $.type_path), field('fields', $.object_expression)),
      ),

    argument_list: ($) =>
      seq('(', commaSep(choice(seq('...', $._expression), $._expression)), ')'),

    member_expression: ($) =>
      prec.left(
        PREC.MEMBER,
        seq(
          field('object', $._expression),
          '.',
          field('property', $.property_name),
        ),
      ),

    index_expression: ($) =>
      prec.left(
        PREC.MEMBER,
        seq(
          field('object', $._expression),
          '[',
          field('index', $._expression),
          ']',
        ),
      ),

    if_expression: ($) =>
      prec.right(
        seq(
          'if',
          field('condition', $._expression),
          optional(';'),
          field('consequence', $.block),
          optional(
            seq('else', field('alternative', choice($.if_expression, $.block))),
          ),
        ),
      ),

    match_expression: ($) =>
      seq(
        'match',
        field('subject', $._expression),
        optional(';'),
        '{',
        commaSep($.match_arm),
        '}',
      ),

    match_arm: ($) =>
      seq(
        field('pattern', $._pattern),
        optional(field('guard', $.guard_clause)),
        '=>',
        field('body', $._expression),
      ),

    loop_expression: ($) => seq('loop', field('body', $.block)),

    for_expression: ($) =>
      seq(
        'for',
        field('pattern', $._pattern),
        'in',
        field('iterable', $._expression),
        optional(';'),
        optional(
          seq('with', field('accumulator', $.pattern_assign), optional(';')),
        ),
        field('body', $.block),
        optional(seq('else', field('alternative', $.block))),
      ),

    rec_expression: ($) => seq('rec', field('expression', $._expression)),

    break_expression: ($) =>
      prec.right(seq('break', optional(field('value', $._expression)))),

    continue_expression: (_) => 'continue',

    function_expression: ($) =>
      seq(
        'fn',
        optional(field('name', $.identifier)),
        optional(field('parameters', $.parameter_list)),
        optional(seq('->', field('return_type', $.type_annotation))),
        field('body', $.block),
      ),

    block: ($) => seq('{', repeat($._statement), '}'),

    // =========================================================================
    // Misc
    // =========================================================================

    property_name: ($) => $.identifier,

    // =========================================================================
    // Tokens
    // =========================================================================

    number: (_) =>
      token(
        choice(
          // Floats (longest match first to take priority over integers)
          seq(
            /[0-9][0-9_]*/,
            '.',
            optional(/[0-9][0-9_]*/),
            optional(/[eE][+-]?[0-9][0-9_]*/),
          ),
          seq('.', /[0-9][0-9_]*/, optional(/[eE][+-]?[0-9][0-9_]*/)),
          seq(/[0-9][0-9_]*/, /[eE][+-]?[0-9][0-9_]*/),
          // Integers
          seq(/[0-9][0-9_]*/, optional('n')),
          seq('0x', /[0-9a-fA-F][0-9a-fA-F_]*/, optional('n')),
          seq('0b', /[01][01_]*/, optional('n')),
          seq('0o', /[0-7][0-7_]*/, optional('n')),
        ),
      ),

    string: (_) =>
      token(
        choice(seq('"', /([^"\\]|\\.)*/, '"'), seq("'", /([^'\\]|\\.)*/, "'")),
      ),

    // Identifiers starting with a lowercase letter, underscore, or dollar sign.
    // These are used for variable names, function names, and field names.
    identifier: (_) => /[a-z_$][a-zA-Z0-9_$]*/,

    // Identifiers starting with an uppercase letter (type names, enum variants,
    // struct names). Separated from identifier to disambiguate struct_expression
    // from block expressions following variables in if/for/loop conditions.
    type_identifier: (_) => /[A-Z][a-zA-Z0-9_$]*/,

    line_comment: (_) => token(seq('//', /.*/)),

    block_comment: (_) => token(seq('/*', /[^*]*\*+([^/*][^*]*\*+)*/, '/')),
  },
});

/**
 * Creates a rule that matches zero or more comma-separated occurrences of
 * the given rule, with optional trailing comma.
 *
 * @param {RuleOrLiteral} rule
 * @returns {ChoiceRule}
 */
function commaSep(rule) {
  return optional(seq(rule, repeat(seq(',', rule)), optional(',')));
}

/**
 * Creates a rule that matches one or more comma-separated occurrences of
 * the given rule, with optional trailing comma.
 *
 * @param {RuleOrLiteral} rule
 * @returns {SeqRule}
 */
function commaSep1(rule) {
  return seq(rule, repeat(seq(',', rule)), optional(','));
}
