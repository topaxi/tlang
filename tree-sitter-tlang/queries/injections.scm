((tagged_string
  tag: (tagged_string_tag) @_tag
  content: (string) @injection.content)
 (#eq? @_tag "re")
 (#set! injection.language "regex")
 (#offset! @injection.content 0 1 0 -1))
