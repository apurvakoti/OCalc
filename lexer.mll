
(**********************************)
(* ACKNOWLEDGMENTS: Much of the regular expression logic was adapted from
the A4 source code - written by Michael Clarkson.

Levenshtein algorithm adapted from the Wikipedia page description.
https://en.wikipedia.org/wiki/Levenshtein_distance*)
(**********************************)

{
open Lexing
open Parser
open Lazy

exception Error of string

exception Autocorrect of string*string

let comment_depth = ref 0

(*******AUTOCORRECT - LEVENSHTEIN AND DICTIONARY********)

(*[minimum a b c] is the minimum of a, b and c*)
let minimum a b c = 
  min a (min b c)

(*[levenshtein s t] calculates the minimum number of single-character
* transformations (could be an addition, switch or deletion) required to take
* string s to string t.
* Follows the Levenshtein distance algorithm.*)
let rec levenshtein s t =
  (*if l1 = 0 then l2 else
  if l2 = 0 then l1 else
  let cost = (if s1.[l1-1] = s2.[l2-1] then 0 else 1) in
  let minimum x y z = List.hd (List.sort (-) [x;y;z]) in
  minimum ((levenshtein s1 (l1-1) s2 l2)+1) ((levenshtein s1 l1 s2 (l2-1))+1)
          ((levenshtein s1 (l1-1) s2 (l2-1))+cost)

  let m = String.length s in
  let n = String.length t in*)
  

  let rec distance i j =
    match i, j with
    |i, 0 -> force (lazy i) (*i deletions needed to empty string*)
    |0, j -> force (lazy j)
    |i, j -> if s.[i-1] = t.[j-1] (*last characters equal*)
             then force (lazy (distance (i-1) (j-1))) (*no transformations needed - same as without last characters*)
             else let d1, d2, d3 = (distance i (j-1)), (distance (i-1) j), (distance (i-1) (j-1)) in
             1 + (min d1 (min d2 d3))
  in distance (String.length s) (String.length t)





(*[dictionary] contains all the supported functions*)
let dictionary = 
  ["pi"; "e"; "phi";
  "pow"; "sqrt"; 
  "sin"; "cos"; "tan";
  "arcsin"; "arccos"; "arctan";
  "ln"; "log"]

(*[autocorrect s lst] returns the string in [lst] whose
* Levenshtein distance to [s] is lowest, bounded above
* by 2. Returns None if no such string.*)
let autocorrect s l =
  let rec helper s l num word =
  match l with
  |[] -> word
  |h::t -> let lev = levenshtein s h in
               if (lev <= 2) && (lev <= num) then helper s t lev (Some h) 
               else helper s t num word
  in helper s l 2 None
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
        { let s = Lexing.lexeme lexbuf in
          match autocorrect s dictionary with
          |None -> raise (Error (Lexing.lexeme lexbuf))
          |Some x -> raise (Autocorrect ((Lexing.lexeme lexbuf), x)) }
              
  | _
        { raise (Error (Lexing.lexeme lexbuf))}
