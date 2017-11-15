open Ast


(******************
Ast_maker is the module that creates AST nodes using the data from the parser and the
types in Ast. *)

(* [make_let_defn ()] creates the identity function y = x. *)
val make_identity : unit -> expr

(*[make_const s] creates the function y = s, where s is the string
 * representing a constant.*)
val make_const: string -> expr

(* [make_add e1 e2] represents y = e1 + e2 *)
val make_add : expr -> expr -> expr

(*[make_mult e1 e2] represents y = (e1)(e2)*)
val make_mult : expr -> expr -> expr

(* [make_expo e1 e2] represents y = (e1)^(e2) *)
val make_expo : expr -> expr -> expr

(* [make_sin e] represents y = sin(e) *)
val make_sin : expr -> expr

(* [make_cos e] represents y = cos(e) *)
val make_cos : expr -> expr

(* [make_tan e] represents y = tan(e) *)
val make_tan : expr -> expr

(* [make_log e] represents y = log(e) *)
val make_log : expr -> expr

