(*Acknowledgments: The method of using a function to transform the list of
 * string-string pairs into OUnit test adapted from A4 source code.*)

open OUnit2
open Ast
open Main

   

let tests = [
  (*ints*)
  {|42|}, "42";
  {|-1|}, "-1";
  {|3110|}, "3110";
  {|0|}, "0";
  {|1001|}, "1001";
  (*strings*)
  {|"hello"|}, {|"hello"|};
  {|""|}, {|""|};
  {|"undefined"|}, {|"undefined"|};
  {|"x"|}, {|"x"|};
  {|true|}, {|true|};
  {|false|}, {|false|};
  {|undefined|}, {|undefined|};
  {|"xyzzy"|}, {|"xyzzy"|};
  {|4/0|}, {|Exception: "Division by zero"|};
  {|4 mod 0|}, {|Exception: "Division by zero"|};
  {|let x = 0 in y|}, {|Exception: "Unbound variable"|};
  {|throw 0|}, {|Exception: 0|};
  {|fun (x) -> 0|}, "<closure>";
  {|0 0|}, {|Exception: "Application: not a function"|};
  {|(fun (x) -> 0) 1 2|}, {|Exception: "Application: wrong number of arguments"|};
  {|ref 0|}, "<location>";
  {|1 := 0|}, {|Exception: "Assignment to non-location"|};
  {|{"x":1}|}, "<location>";
]

let make_interp_expr_test idx in_str out_str =
  "test" ^ (string_of_int idx) ^ in_str ^ out_str ^ "output :" ^ (interp_expr in_str) >:: (fun _ -> assert_equal out_str (interp_expr in_str))

let _ = run_test_tt_main ("suite" >::: 
  List.mapi (fun idx (i, o) -> make_interp_expr_test idx i o) tests)
