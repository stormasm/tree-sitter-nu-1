; keywords
; --------

[
 "def"
 "def-env"
 "export-env"
] @keyword.function

[
 "export"
 "hide"
 "hide-env"
] @keyword.control.export

[
 "let"
 "mut"
 "const"
 "alias"
 "let-env"
] @keyword.storage.type

[
 "source"
 "source-env"
 "use"
] @keyword.control.import

[
 "for"
 "loop"
 "while"
] @keyword.control.repeat

[
 "error"
] @keyword.control.exception

[
 "do"
] @keyword

[
 "if"
 "else"
 "try"
 "catch"
 "match"
] @keyword.control.conditional

[
 "break" 
"continue"
] @keyword.control

"return" @keyword.control.return

[
 "hide"
 "hide-env"
 ] @keyword

[
 "as"
 "in"
] @keyword.operator

[
  "err>" "out>"
  "e>" "o>"
  "err+out>" "out+err>"
  "o+e>" "e+o>"
] @special

; comments
; --------

(comment @comment)

; variables
; ---------
(val_variable ["$in" "$nu" "$env" "$nothing"] @variable.builtin )
(val_variable @variable.other)

; operators
; ---------
(expr_binary
  opr:  [
   "+"
   "-"
   "*"
   "/"
   "mod"
   "//"
   "**"
   "++"
   "=="
   "!="
   "<"
   ">"
   "<="
   ">="
   "=~"
   "~="
   "in"
   "not-in"
   "not"
   "and"
   "or"
   "xor"
   "bit-or"
   "bit-xor"
   "bit-and"
   "bit-shl"
   "bit-shr"
   "starts-with"
   "ends-with"
   "="
   "+="
   "-="
   "*="
   "/="
   "++="
] @operator )

; primitives and types
; --------------------
(flat_type) @type.builtin

(val_string) @string
(val_null) @constant.builtin
(val_int) @constant.numeric.integer
(val_float) @constant.numeric.float
(val_bool) @constant.builtin.boolean
(val_interpolated) @string

[
 escape_sequence
 inter_escape_sequence
] @constant.character.escape

; punctuation
; -----------
[
 "(" ")"
 "{" "}"
 "[" "]"
] @punctuation.bracket

[
 "@"
 "."
 "="
 "..."
 ":"
 ";"
 ","
 "=>"
 "?"
] @punctuation.delimiter

(parameter_pipes "|" @punctuation.delimiter)

(val_interpolated
  expr: ["(" ")"] @punctuation.special)

(list_type
  ["<" ">"] @punctuation.bracket)

"|" @punctuation.special
