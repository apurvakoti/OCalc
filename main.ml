open Ast
open Eval
open Grapher


(*Overriding [string_of_float]*)
let string_of_float f =
  let st = string_of_float f in
  if st.[(String.length st) - 1] = '.'
  then st ^ "0"
  else st

(*[lst_to_string] converts a float list [lst] to a string representation.*)
let rec lst_to_string lst acc =
  match lst with
  |[] -> acc
  |(k, v)::t -> lst_to_string t (acc ^ "(" ^ (string_of_float k) ^ "," ^ (string_of_float v) ^ ")" ^ "; ")

let interp_expr s minx maxx miny maxy =
    try (let parsed = Parse.parse_expr s in
    let eval = Eval.eval_expr parsed (Eval.transform minx maxx) in
    match eval with |Const f -> string_of_float f
                    |Mapping l -> (Grapher.set_up s minx maxx miny maxy l); "plotting")
    with | Parser.Error -> "Syntax error. Type \"help\" for syntax guidance."
    | Lexer.Error s -> "Interpretation error: \"" ^ s ^ "\" is not defined."
    | Lexer.Autocorrect (s, x) -> "Interpretation error: \"" ^ s ^ "\" is not defined. Did you mean " ^ x ^ "?"
    | Eval.EvalExp s -> "Evaluation error: " ^ s
    | e -> "Failure. Bounds may be too extreme."
    | End_of_file -> ""

(*[is_num s] is true [s] is a valid string representation of a number.*)
let is_num s =
  try ignore (float_of_string s); true with _ -> false

let help_text =
  "This program acts as a numeric calculator and as a graphing calculator. If what
  evaluates to a constant is entered, the calculated value is displayed; if a function is entered,
  the function is graphed within the specified range.\n
  The default scale is x ϵ [-10.0, 10.0], y ϵ [-10.0, 10.0],.\n\n
  SUPPORTED COMMANDS: \n
  - \"help\"
  - \"quit\"
  - \"see scale\"
  - \"change scale\"\n\n

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




let rec main minx maxx miny maxy () =
  print_string  "\n> ";
  match String.lowercase_ascii (read_line ()) |> String.trim with
  | "quit" ->  (*print_endline "\nAre you sure? Y/N"; let rec handle_quit () = (
               match (String.lowercase_ascii (read_line ())) with
               |"y" -> ()
               |"n" -> main min max ()
               | _ -> print_endline "I didn't get that."; handle_quit ())
               in handle_quit ()*) ()
  | "help" -> (print_endline help_text; main minx maxx miny maxy ())
  | "change scale" -> changescale ()
  | "see scale" -> (print_endline ("x ϵ [" ^(string_of_float minx)^", "^(string_of_float maxx)^"], " ^
                                   "y ϵ [" ^(string_of_float miny)^", "^(string_of_float maxy)^"]");
                    main minx maxx miny maxy ())
  | e -> let interped = interp_expr e minx maxx miny maxy in print_endline interped; main minx maxx miny maxy ()

  (*[changescale ()] is a helper "REPL" meant to process the scale change input sequence.*)
  and changescale () =
    print_endline "Enter min-bound x:";
    let min' = read_line () in
    print_endline "Enter max-bound x:";
    let max' = read_line () in
    print_endline "Enter min-bound y:";
    let min'' = read_line () in
    print_endline "Enter max-bound y:";
    let max'' = read_line () in
    if not ((is_num min') && (is_num max') && (is_num min'') && (is_num max'')) then (print_endline "One or more bounds are invalid. Try again."; changescale ())
    else let minnum = float_of_string min' in
    let maxnum = float_of_string max' in
    let minnum' = float_of_string min'' in
    let maxnum' = float_of_string max'' in
    if minnum >= maxnum || minnum' >= maxnum' then (print_endline "Min must be strictly less than max."; changescale ())
    else print_endline "Scale set."; main minnum maxnum minnum' maxnum' ()

let _ = print_endline "\n\n\n\n
________  ________  ________  ___       ________     
|\\   __  \\|\\   ____\\|\\   __  \\|\\  \\     |\\   ____\\    
\\ \\  \\|\\  \\ \\  \\___|\\ \\  \\|\\  \\ \\  \\    \\ \\  \\___|    
 \\ \\  \\\\\\  \\ \\  \\    \\ \\   __  \\ \\  \\    \\ \\  \\       
  \\ \\  \\\\\\  \\ \\  \\____\\ \\  \\ \\  \\ \\  \\____\\ \\  \\____  
   \\ \\_______\\ \\_______\\ \\__\\ \\__\\ \\_______\\ \\_______\\
    \\|_______|\\|_______|\\|__|\\|__|\\|_______|\\|_______|

\n\nEnter a function, \"help\", \"quit\", \"see scale\", or \"change scale\".\n"; main (-10.) 10. (-10.) 10. ()
