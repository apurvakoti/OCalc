(*Acknowledgements: The parse function was adapted from the A4 source
 * code, written by Michael Clarkson.*)

open Lexing

exception SyntaxError of string

let parse parser_start s =
  let lexbuf = from_string (String.lowercase_ascii s) in
  parser_start Lexer.token lexbuf

let parse_expr =
  parse Parser.parse_expression