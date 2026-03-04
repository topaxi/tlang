; Scopes and local variable tracking for tlang

; Scopes
(source_file) @local.scope
(block) @local.scope
(function_declaration body: (block) @local.scope)
(function_expression body: (block) @local.scope)
(match_arm body: _ @local.scope)
(for_expression body: (block) @local.scope)
(loop_expression body: (block) @local.scope)

; Definitions
(function_declaration
  name: (function_name
    definition: (identifier) @local.definition))

(function_declaration
  name: (function_name
    definition: (type_identifier) @local.definition))

(variable_declaration
  pattern: (pattern_assign
    pattern: (binding) @local.definition))

(function_parameter
  (binding) @local.definition)

(function_parameter
  (wildcard_pattern) @local.definition)

(pattern_assign pattern: (binding) @local.definition)

; References
(identifier) @local.reference
