open Lexing

exception SyntaxError of string

let location_message lexbuf =
  let start = lexeme_start_p lexbuf in
  let finish = lexeme_end_p lexbuf in
  Printf.sprintf "line %d, characters %d-%d"
    start.pos_lnum
    (start.pos_cnum - start.pos_bol)
    (finish.pos_cnum - finish.pos_bol)

let syntax_error_message lexbuf  =
  Printf.sprintf
    "Syntax error, %s: %s"
    (location_message lexbuf)
    (lexeme lexbuf)

let parse_error lexbuf =
  raise (SyntaxError (syntax_error_message lexbuf))

let unexpected_error msg lexbuf =
  failwith ("Unexpected parsing exception: " ^ msg
            ^ "\noccurred at " ^ (location_message lexbuf))

let parse parser_start s =
  let lexbuf = from_string s in
  parser_start Lexer.token lexbuf

let parse_expr =
  parse Parser.parse_expression