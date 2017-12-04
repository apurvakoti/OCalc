open Ast
open Eval


let rec lst_to_string lst acc =
  match lst with
  |[] -> acc
  |(k, v)::t -> lst_to_string t (acc ^ "(" ^ (string_of_float k) ^ "," ^ (string_of_float v) ^ ")" ^ "; ")

let interp_expr s min max =
    let parsed = Parse.parse_expr s in
    let eval = Eval.eval_expr parsed (Eval.transform min max) in
    (fun r -> (match r with Const f -> string_of_float f | Mapping l -> lst_to_string l "")) eval

let is_num s =
  try float_of_string s; true with _ -> false

(*let rec ensure_number () =
  let inp = read_line () in
  if is_num inp then inp
  else print_endline "Not a number. Try again."; ensure_number ()*)


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
  - y = expr1 * expr2
  - y = expr / expr
  - y = pow(expr, expr) or expr^expr 
  - y = sin(expr), cos(expr), tan(expr)
  - y = arcsin(expr), arccos(expr) [arguments must be between -1.0 and 1.0]
  - y = arctan(expr)
  - y = ln(expr)
  - y = log(expr1, expr2) [i.e. log of expr2 to the base expr1]\n\n
  Make sure your inputs are syntactically valid with parentheses according to the above guide. Inputs are case-insensitive.
 \n Type anything to continue."




let rec main min max () =
  print_endline "Enter a function, or enter \"help\" or \"quit\".\n";
  print_string  "> ";
  match String.lowercase_ascii (read_line ()) with
  | "quit" ->  print_endline "Are you sure? Y/N"; let rec handle_quit () = ( 
               match (String.lowercase_ascii (read_line ())) with 
               |"y" -> ()
               |"n" -> main min max ()
               | _ -> print_endline "I didn't get that."; handle_quit ())
               in handle_quit ()
  | "help" -> (print_endline help_text; let inp = read_line () in main min max ())
  | "change-scale" -> let rec changescale () = 
                      print_endline "Enter min-bound:"; 
                      let min' = read_line () in
                      print_endline "Enter max-bound:";
                      let max' = read_line () in
                      if not ((is_num min') && (is_num max')) then (print_endline "One or more bounds are invalid. Try again."; changescale ())
                      else let minnum = float_of_string min' in
                      let maxnum = float_of_string max' in
                      if minnum >= maxnum then (print_endline "Min must be strictly less than max."; changescale ())
                      else main minnum maxnum ()
                      in changescale ()                                  
  | e -> let interped = (try interp_expr e min max with | Parser.Error -> "Syntax error. Type \"help\" for syntax guidance." 
                                                | Lexer.Error s -> "Error: " ^ s
                                                | Eval.EvalExp s -> "Evaluation error: " ^ s)
         in print_endline interped; main min max ()

let _ = main 1. 10. ()



 