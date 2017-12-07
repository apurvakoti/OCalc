(*Acknowledgments: The method of using a function to transform the list of
 * string-string pairs into OUnit test adapted from A4 source code.*)

open OUnit2
open Ast
open Main

let st_func s = 
  Main.interp_expr s 1. 10. 1. 10.

let tests = [
  (*constants*)
  "", "";
  "1", "1.0";
  "10", "10.0";
  "20000", "20000.0";
  "pi", "3.14159265";
  "e", (string_of_float (exp 1.));
  "phi", "1.61803399";

  (*addition*)
  "1 + 2", "3.0";
  "1     +2", "3.0";
  "100+1+1+1", "103.0";
  "0 + 0", "0.0";
  (*subtraction*)
  "-1", "-1.0";
  "2--2", "4.0";
  "1-----1", "0.0";
  "-1-1", "-2.0";
  "1 + 1 - 1 + 1", "2.0";
  "1      -  1", "0.0";
  (*multiplication*)
  "0 * 0", "0.0";
  "1 * 1", "1.0";
  "100000 * 200", "20000000.0";
  (*division*)
  "100/0", "Evaluation error: Can't divide by zero.";
  "0/0", "Evaluation error: Can't divide by zero.";
  "100/100", "1.0";
  "52/20", "2.6";
  "1/2", "0.5";
  "e/e", "1.0";
  "pi/pi", "1.0";
  (*pow*)
  "1 ^ 2", "1.0";
  "pow(1, 2)", "1.0";
  "0^0", "1.0";
  "pow(2, pow(2, 3))", "256.0";
  "2^2^3", "64.0";
  "3^(pow(2, 2))", "81.0";
  "2^(-1)", "0.5";
  "1^1^1^1^1", "1.0";
  (*sqrt*)
  "sqrt(36)", "6.0";
  "sqrt(100)", "10.0";
  "sqrt(0)", "0.0";
  "sqrt(1)", "1.0";
  (*trig*)
  "pow(sin(1), 2) + pow(cos(1), 2)", "1.0";
  "tan(0)", "0.0";
  "sin(0)/cos(0)", "0.0";
  "arcsin(0)", "0.0";
  "arccos(1)", "0.0";
  "arctan(0)", "0.0";
  (*log*)
  "ln(e)", "1.0";
  "log(2, 8)", "3.0";
  "log(1, 10)", "Evaluation error: Can't divide by zero."

]

let make_interp_expr_test idx in_str out_str =
  "Test " ^ (string_of_int idx) ^ "("^ in_str ^", "^ out_str ^ "; output :" ^ (st_func in_str) >:: (fun _ -> assert_equal out_str (st_func in_str))

let _ = run_test_tt_main ("suite" >::: 
  List.mapi (fun idx (i, o) -> make_interp_expr_test idx i o) tests)
