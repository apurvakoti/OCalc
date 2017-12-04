
(**********************************)
(* ACKNOWLEDGMENTS: Much of the regular expression logic was adapted from
the A4 source code - written by Michael Clarkson*)
(**********************************)

{
open Lexing
open Parser

exception Error of string

let comment_depth = ref 0

(******************************************************************)
(* Helper functions for lexing strings *)
(******************************************************************)


let string_buffer = Buffer.create 256
let reset_string_buffer () = Buffer.reset string_buffer
let get_stored_string () = Buffer.contents string_buffer

let store_string_char c = Buffer.add_char string_buffer c
let store_string s = Buffer.add_string string_buffer s
let store_lexeme lexbuf = store_string (Lexing.lexeme lexbuf)

let store_escaped_char lexbuf c = store_string_char c

let hex_digit_value d = (* assert (d in '0'..'9' 'a'..'f' 'A'..'F') *)
  let d = Char.code d in
  if d >= 97 then d - 87 else
  if d >= 65 then d - 55 else
  d - 48

let hex_num_value lexbuf ~first ~last =
  let rec loop acc i = match i > last with
  | true -> acc
  | false ->
      let value = hex_digit_value (Lexing.lexeme_char lexbuf i) in
      loop (16 * acc + value) (i + 1)
  in
  loop 0 first

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  if (c < 0 || c > 255)
    then raise (Error "Encoding error")
    else Char.chr c

let char_for_octal_code lexbuf i =
  let c = 64 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           8 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
               (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  Char.chr c

let char_for_hexadecimal_code lexbuf i =
  let byte = hex_num_value lexbuf ~first:i ~last:(i+1) in
  Char.chr byte

}

(******************************************************************)
(* Lexer body *)
(******************************************************************)

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let word = letter+
let id = ('_' | letter) ('_' | letter | digit)*

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']

let lowercase = ['a'-'z']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let id = (lowercase | '_') identchar*

let whole_number =
  ['0'-'9'] ['0'-'9' '_']*

let decimal_number =
  ['0'-'9'] ['.'] ['0'-'9']*

let num_literal =
  whole_number | decimal_number
  

rule token = parse
  | blank+
        { token lexbuf }
  | ['\n']
        { new_line lexbuf; token lexbuf }
  | num_literal
        { CONST (Lexing.lexeme lexbuf) }
  | "x"
        { IDENT }
  | "+"
        { PLUS }
  | "-"
        { MINUS }
  | "*"
        { TIMES }
  | "/"
        { DIV }
  | "("
        { LPAREN }
  | ")"
        { RPAREN }
  | "," 
        { COMMA }
  | "^"
        { CARAT }
  | "pow"
        { POW }
  | "sqrt"
        { SQRT }
  | "log"
        { LOG }
  | "ln"
        { LN }
  | "sin"
        { SIN }
  | "cos"
        { COS }
  | "tan"
        { TAN }
  | "arcsin"
        { ARCSIN }
  | "arccos"
        { ARCCOS }
  | "arctan"
        { ARCTAN }
  | "pi"
        { PI }
  | "e"
        { NATEXP }
  | "phi"
        { PHI }
  | "y"
        { Y }
  | "="
        { EQUALS }
  | eof
        { EOF }
  | word
        { raise (Error (Lexing.lexeme lexbuf))}
  | _
        { raise (Error (Lexing.lexeme lexbuf))}
