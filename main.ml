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


let help_text = 
  "This program acts as a numeric calculator and as a graphing calculator. If what
  evaluates to a constant is entered, the calculated value is displayed; if a function is entered,
  the function is graphed within the specified range.\n\n
  SUPPORTED CONSTANTS:\n
  \"pi\" (3.14159...)\n
  \"e\" (2.71828...)\n\n
  SUPPORTED FUNCTIONS:\n
  In the following, \"expr\" refers to what can be another function, allowing for
  nested functions.\n
  - y = x
  - y = expr + expr
  - y = expr - expr
  - y = expr * expr
  - y = expr / expr
  - y = pow(expr, expr) or expr^expr
  - y = sin(expr), cos(expr), tan(expr)
  - y = arcsin(expr), arccos(expr) [arguments must be between -1.0 and 1.0]
  - y = arctan(expr)
  - y = ln(expr)
  - y = log(expr1, expr2) [i.e. log of expr2 to the base expr1]\n\n
  Make sure your inputs are syntactically valid according to the above guide. Type anything to continue."


let handle_quit s =
  match s with
  |"y" -> ()

let rec main () =
  print_endline "Enter a function, or enter \"help\" or \"quit\".\n";
  print_string  "> ";
  match String.lowercase_ascii (read_line ()) with
  (*| exception End_of_file -> ()*)
  | "quit" ->  print_endline "Are you sure? Y/N"; let rec handle_quit () = ( 
               match (String.lowercase_ascii (read_line ())) with 
               |"y" -> ()
               |"n" -> main ()
               | _ -> print_endline "I didn't get that."; handle_quit ())
               in handle_quit ()
  | "help" -> print_endline help_text; let inp = read_line () in main ()
  | e -> let interped = (try interp_expr e with | Parser.Error -> "Error" 
                                                | Lexer.Error s -> "Error: " ^ s)
         in print_endline interped; main ()

let _ = main ()



 
