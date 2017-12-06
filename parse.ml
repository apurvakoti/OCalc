(*Acknowledgements: The parse function was adapted from the A4 source
 * code, written by Michael Clarkson.*)

open Lexing

exception SyntaxError of string

(*[parse p s] parses string [s] using the parsing rules defined in [p].*)
let parse parser_start s =
  let lexbuf = from_string (String.lowercase_ascii s) in
  parser_start Lexer.token lexbuf

(*[parse_expr] calls [parse] using the rules defined in the Parser.mly file.*)
let parse_expr =
  parse Parser.parse_expression