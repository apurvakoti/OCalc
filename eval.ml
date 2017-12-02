open Ast
open Ast_maker

type result =
  Const of float | Mapping of float*float list


(*HELPERS*)
(*[make_list_n v n] creates a list of size n
 * in which all the elements are v.*)
let rec make_list_n v n =
  if n = 0 then [] else (v::(make_list_n v (n-1)))

(*[both_consts e1 e2] returns true iff e1 and e2 are both
 * constants.*)
let both_consts e1 e2 =
  match e1 with
  |Const _ -> (match e2 with Const _ -> true |_ -> false)
  |_ -> false

(*[eval_as_consts f e1 e2] evaluates [e1] and [e2] as
 * constants using function [f]. Returns a result.*)
let eval_as_consts f e1 e2 =
  let fval = (match e1 with Const f -> f) in
  let sval = (match e2 with Const f -> f) in
  Const (f fval sval)

(*[eval_as_consts f e] is the single-argument
 * version of the above function. Returns a result.*)
 let eval_as_const f e =
  let fval = (match e1 with Const f -> f) in
  Const (f fval)

(********************)

let transform min max = 
  let rec helper min' max' acc lacc = 
    if acc = min' then lacc else
    helper min' max' (acc -. (0.05 *. (max' -. min'))) (acc::lacc)
  in helper min max max []

let rec eval_expr e scale =
  match e with
  |NIdent -> Mapping (List.map eval_iden scale)

and eval_iden x_val =
  (x_val, x_val)

and eval_const c_val =
  float_of_string c_val

and eval_bop_helper e1 e2 func scale = 
  let first = eval_expr e1 scale in
  let second = eval_expr e2 scale in
  if both_consts first second then 
  eval_as_consts func first second
  else
  let firstlst = 
    (match first with
    | Const f -> make_list_n f (List.length scale)
    | Mapping l -> List.map (fun (_, b) -> b) l) in
  let secondlst =
    (match second with
    | Const f -> make_list_n f (List.length scale)
    | Mapping l -> List.map (fun (_, b) -> b) l) in
  let sum = List.map2 func firstlst secondlst in
  Mapping (List.map2 (fun a b -> (a, b)) scale sum)

and eval_add e1 e2 scale =
  eval_bop_helper e1 e2 (+.) scale

and eval_minus e1 e2 scale =
  eval_bop_helper e1 e2 (-.) scale

and eval_mult e1 e2 scale = 
  eval_bop_helper e1 e2 (fun a b -> a *. b) scale

and eval_div e1 e2 scale =

  
  