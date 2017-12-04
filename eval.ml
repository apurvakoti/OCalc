open Ast
open Ast_maker

type result =
  Const of float | Mapping of (float*float) list


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

(*[is_const e] returns true iff e is a
 * constant.*)
let is_const e =
  match e with
  |Const _ -> true
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
  let fval = (match e with Const f -> f) in
  if fval = nan then failwith "Not a number. Check your inputs" else
  Const (f fval)

(********************)

let transform min max = 
  let rec helper min' max' offset acc lacc = 
    if acc <= min' then lacc else
    helper min' max' offset (acc -. offset) (acc::lacc)
  in helper min max (0.005 *. (max -. min)) max []

(********THIS IS THE MAIN FUNCTION PLEASE DON'T GET LOST IN THE CODE*****)
let rec eval_expr e scale =
  match e with
  |NIdent -> eval_iden scale
  |NConst s -> eval_const s
  |NAdd (e1, e2) -> eval_add e1 e2 scale
  |NMinus (e1, e2) -> eval_minus e1 e2 scale
  |NMult (e1, e2) -> eval_mult e1 e2 scale
  |NDiv (e1, e2) -> eval_div e1 e2 scale
  |NPow (e1, e2) -> eval_pow e1 e2 scale
  |NSin e -> eval_sin e scale
  |NCos e -> eval_cos e scale
  |NTan e -> eval_tan e scale
  |NArcsin e -> eval_arcsin e scale
  |NArccos e -> eval_arccos e scale
  |NArctan e -> eval_arctan e scale
  |_ -> failwith "Unimplemented"

(***********************************************************)

and eval_iden scale =
  Mapping (List.map (fun x -> (x, x)) scale) 

and eval_const c_val =
  Const (float_of_string c_val)

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
  let combined = List.map2 func firstlst secondlst in
  Mapping (List.map2 (fun a b -> (a, b)) scale combined)

and eval_uop_helper e func scale =
  let first = eval_expr e scale in
  if is_const first then 
  eval_as_const func first
  else
  let firstlst = 
    (match first with
    | Const _ -> failwith "wasn't supposed to be a constant"
    | Mapping l -> List.map (fun (_, b) -> b) l) in
  let operated = List.map func firstlst in
  (*the below check is only in UOP because NaN appears only with the
   * unary operators arcsin, arccos etc*)
  if List.mem nan operated then failwith "Not a number. Check your inputs" else
  Mapping (List.map2 (fun a b -> (a, b)) scale operated)


and eval_add e1 e2 scale =
  eval_bop_helper e1 e2 (+.) scale

and eval_minus e1 e2 scale =
  eval_bop_helper e1 e2 (-.) scale

and eval_mult e1 e2 scale = 
  eval_bop_helper e1 e2 ( *. ) scale

and eval_div e1 e2 scale = 
  eval_bop_helper e1 e2 (/.) scale

and eval_pow e1 e2 scale =
  eval_bop_helper e1 e2 ( ** ) scale

and eval_sin e scale =
  eval_uop_helper e sin scale

and eval_cos e scale = 
  eval_uop_helper e cos scale

and eval_tan e scale =
  eval_uop_helper e tan scale

and eval_arcsin e scale =
  eval_uop_helper e asin scale

and eval_arccos e scale =
  eval_uop_helper e acos scale

and eval_arctan e scale =
  eval_uop_helper e atan scale

  
  