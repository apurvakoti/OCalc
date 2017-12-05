open Ast
open Eval


let rec lst_to_string lst acc =
  match lst with
  |[] -> acc
  |(k, v)::t -> lst_to_string t (acc ^ "(" ^ (string_of_float k) ^ "," ^ (string_of_float v) ^ ")" ^ "; ")

let interp_expr s min max =
    let parsed = Parse.parse_expr s in
    let eval = Eval.eval_expr parsed (Eval.transform min max) in
    (fun r -> (match r with |Const f -> let st = string_of_float f in
                                        if st.[(String.length st) - 1] = '.'
                                        then st ^ "0"
                                        else st
                            |Mapping l -> lst_to_string l "")) eval

let is_num s =
  try ignore (float_of_string s); true with _ -> false

(*let rec ensure_number () =
  let inp = read_line () in
  if is_num inp then inp
  else print_endline "Not a number. Try again."; ensure_number ()*)


let help_text = 
  "This program acts as a numeric calculator and as a graphing calculator. If what
  evaluates to a constant is entered, the calculated value is displayed; if a function is entered,
  the function is graphed within the specified range.\n
  The default scale is x ϵ [1.0, 10.0].\n\n
  SUPPORTED COMMANDS: \n
  - \"help\"
  - \"quit\"
  - \"see-scale\"
  - \"change-scale\"\n\n

  SUPPORTED CONSTANTS:\n
  - \"pi\" (3.14159...)
  - \"e\" (2.71828...)
  - \"phi\" (1.61803...)\n\n
  SUPPORTED FUNCTIONS:\n
  In the following, \"expr\" refers to what can be another function, allowing for
  nested functions.\n
  - x
  - expr + expr
  - expr - expr
  - expr1 * expr2 [constx is shorthand for const*x]
  - expr / expr
  - pow(expr, expr) [or expr^expr]
  - sqrt(expr)
  - sin(expr), cos(expr), tan(expr)
  - arcsin(expr), arccos(expr) [arguments must be between -1.0 and 1.0]
  - arctan(expr)
  - ln(expr)
  - log(expr1, expr2) [i.e. log of expr2 to the base expr1]\n\n
  Make sure your inputs are syntactically valid with parentheses according to the above guide. Inputs are case-insensitive."




let rec main min max () =
  print_string  "\n> ";
  match String.lowercase_ascii (read_line ()) with
  | "quit" ->  print_endline "\nAre you sure? Y/N"; let rec handle_quit () = ( 
               match (String.lowercase_ascii (read_line ())) with 
               |"y" -> ()
               |"n" -> main min max ()
               | _ -> print_endline "I didn't get that."; handle_quit ())
               in handle_quit ()
  | "help" -> (print_endline help_text; main min max ())
  | "change-scale" -> let rec changescale () = 
                      print_endline "Enter min-bound:"; 
                      let min' = read_line () in
                      print_endline "Enter max-bound:";
                      let max' = read_line () in
                      if not ((is_num min') && (is_num max')) then (print_endline "One or more bounds are invalid. Try again."; changescale ())
                      else let minnum = float_of_string min' in
                      let maxnum = float_of_string max' in
                      if minnum >= maxnum then (print_endline "Min must be strictly less than max."; changescale ())
                      else print_endline "Scale set."; main minnum maxnum ()
                      in changescale ()
  | "see-scale" -> (print_endline ("x ϵ [" ^(string_of_float min)^", "^(string_of_float max)^"]."); 
                    main min max ())             
  | e -> let interped = try interp_expr e min max 
                        with| Parser.Error -> "Syntax error. Type \"help\" for syntax guidance." 
                            | Lexer.Error s -> "Interpretation error: \"" ^ s ^ "\" may not be defined."
                            | Eval.EvalExp s -> "Evaluation error: " ^ s
                            | End_of_file -> "" 
         in print_endline interped; main min max ()

let _ = print_endline "\n\n\n\nEnter a function, \"help\", \"quit\", \"see-scale\", or \"change-scale\".\n"; main 1. 10. ()



 
