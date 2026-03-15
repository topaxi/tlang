((tagged_string
  tag: (tagged_string_tag) @_tag
  (#eq? @_tag "re")
  content: (string) @injection.content)
 (#set! injection.language "regex")
 (#set! injection.include-children))
