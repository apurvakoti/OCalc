open Ast

(*[EvalExp] is the exception raised during evaluations of invalid
 * mathematical forms*)
exception EvalExp of string

(*type [result] is a mapping of x-values to y-values resulting
 *from the evaluation of an expression. Can be either a mapping
 *within the scale specified, or a single constant value.*)
type result = Const of float | Mapping of (float*float) list

(*[eval_expr e lst] evaluates expression [e] for all x-values
 * in [lst] and returns the corresponding [result].*)
val eval_expr : Ast.expr -> float list -> result


(*[transform min max] returns a list of x-values between
 * [min] and [max]. Change in x is 0.005 * (max - min).*)
val transform : float -> float -> float list


