open Ast

(*type [result] is a mapping of x-values to y-values resulting
 *from the evaluation of an expression. Can be either a mapping
 *within the scale specified, or a single constant value.*)
type result

(*[eval_expr e lst] evaluates expression [e] for all x-values
 * in [lst] and returns the corresponding [result].*)
val eval_expr : expr -> float list -> result


(*[transform min max] returns a list of x-values between
 * [min] and [max]. Change in x is 0.05 * (max - min).*)
val transform : float -> float -> float list

(*[string_of_result r] is a string representing result [r].*)
val string_of_result : result -> string


