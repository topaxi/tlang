; Tags for navigation (functions, types, etc.)

; Function definitions
(function_declaration
  name: (function_name
    definition: (identifier) @name) @definition.function)

(function_declaration
  name: (function_name
    definition: (type_identifier) @name) @definition.function)

; Method definitions
(function_declaration
  name: (function_name
    method: (property_name) @name) @definition.method)

; Enum declarations
(enum_declaration
  name: (type_identifier) @name) @definition.type

; Struct declarations
(struct_declaration
  name: (type_identifier) @name) @definition.type

; Calls
(call_expression
  callee: (identifier) @name) @reference.call

(call_expression
  callee: (member_expression
    property: (property_name) @name)) @reference.call
