; Highlights for tlang

; Keywords
[
  "fn"
  "let"
  "return"
  "if"
  "else"
  "match"
  "loop"
  "for"
  "in"
  "with"
  "rec"
  "break"
  "enum"
  "struct"
  "protocol"
  "impl"
  "apply"
  "implements"
  "matches"
  "as"
  "not"
  "and"
  "or"
] @keyword

(visibility_modifier) @keyword

(continue_expression) @keyword

; Declarations
(function_declaration
  name: (function_name
    definition: _ @function))

(function_expression
  name: (identifier) @function)

(enum_declaration
  name: (type_identifier) @type)

(struct_declaration
  name: (type_identifier) @type)

(protocol_declaration
  name: (type_identifier) @type)

(apply_statement
  method: (identifier) @function.method)

; Type identifiers
(type_identifier) @type

; Call expressions
(call_expression
  callee: (identifier) @function.call)
(call_expression
  callee: (member_expression
    property: (property_name) @function.method))
(call_expression
  callee: (path_expression
    (type_identifier) @type
    (identifier) @function.call))
(call_expression
  callee: (path_expression
    (identifier) @function.call))

; Struct expressions
(struct_expression
  type: _ @type)

; Operators
(binary_expression operator: _ @operator)
(unary_expression operator: _ @operator)
(assignment_expression operator: _ @operator)

["|>" "->"] @operator

; Special variables
(self) @variable.builtin
(wildcard) @variable.builtin
(wildcard_pattern) @variable.builtin
(self_pattern) @variable.builtin

; Patterns
(binding) @variable

; Literals
(number) @number
(string) @string
(tagged_string (tagged_string_tag) @string.special.symbol)
(tagged_string (tagged_string_start) @string)
(tagged_string (tagged_string_content) @string)
(tagged_string (tagged_string_end) @string)
(interpolation "{" @punctuation.special)
(interpolation "}" @punctuation.special)
(boolean_literal) @boolean

; Comments
(line_comment) @comment
(block_comment) @comment

; Punctuation
["(" ")" "[" "]" "{" "}"] @punctuation.bracket
["," "." ";" "::"] @punctuation.delimiter
["=>" ":"] @punctuation.delimiter
