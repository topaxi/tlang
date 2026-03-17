; Regex injection for re"..." tagged strings
((tagged_string
  tag: (tagged_string_tag) @_tag
  (tagged_string_content) @injection.content)
 (#eq? @_tag "re")
 (#set! injection.language "regex")
 (#set! injection.combined))

; HTML injection for html"..." tagged strings
((tagged_string
  tag: (tagged_string_tag) @_tag
  (tagged_string_content) @injection.content)
 (#eq? @_tag "html")
 (#set! injection.language "html")
 (#set! injection.combined))

; CSS injection for css"..." tagged strings
((tagged_string
  tag: (tagged_string_tag) @_tag
  (tagged_string_content) @injection.content)
 (#eq? @_tag "css")
 (#set! injection.language "css")
 (#set! injection.combined))

; SQL injection for sql"..." tagged strings
((tagged_string
  tag: (tagged_string_tag) @_tag
  (tagged_string_content) @injection.content)
 (#eq? @_tag "sql")
 (#set! injection.language "sql")
 (#set! injection.combined))

; JSON injection for json"..." tagged strings
((tagged_string
  tag: (tagged_string_tag) @_tag
  (tagged_string_content) @injection.content)
 (#eq? @_tag "json")
 (#set! injection.language "json")
 (#set! injection.combined))

; JavaScript injection for js"..." tagged strings
((tagged_string
  tag: (tagged_string_tag) @_tag
  (tagged_string_content) @injection.content)
 (#eq? @_tag "js")
 (#set! injection.language "javascript")
 (#set! injection.combined))
