# tree-sitter nu

[nushell](https://github.com/nushell/nushell) grammar for [tree-sitter](https://tree-sitter.github.io/tree-sitter/)

# issues
1. unquoted strings
_example_:
```nu
# this should parse as a pipeline with two elements
'nushell' | str contains n

# but it is parsed as two pipelines, with one ending
# at `contains`
'nushell' | str contains
n

# quoting the string solves it though
'nushell' | str contains 'n' # <- okay
```
