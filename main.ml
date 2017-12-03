open Ast
open Eval
(*open Parse*)

let rec lst_to_string lst acc =
  match lst with
  |[] -> acc
  |(k, v)::t -> lst_to_string t (acc ^ "(" ^ (string_of_float k) ^ "," ^ (string_of_float v) ^ ")" ^ "; ")

let interp_expr s =
    let parsed = Parse.parse_expr s in
    let eval = Eval.eval_expr parsed (Eval.transform 1. 10.) in
    (fun r -> (match r with Const f -> string_of_float f | Mapping l -> lst_to_string l "")) eval


let rec main () =
  print_endline "Enter a function\n";
  print_string  "> ";
  match read_line () with
  (*| exception End_of_file -> ()*)
  | "quit" -> ()
  | e -> print_endline (interp_expr e); main ()

let _ = main ()



 
