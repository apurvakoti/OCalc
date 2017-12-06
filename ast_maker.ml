open Ast

let make_identity () = 
  NIdent

let make_const s =
  NConst s

let make_add e1 e2 =
  NAdd (e1, e2)

let make_minus e1 e2 =
  NMinus (e1, e2)

let make_mult e1 e2 =
  NMult (e1, e2)

let make_div e1 e2 =
  NDiv (e1, e2)

let make_pow e1 e2 =
  NPow (e1, e2)

let make_sin e =
  NSin e

let make_cos e =
  NCos e

let make_tan e =
  NTan e

let make_arcsin e =
  NArcsin e

let make_arccos e =
  NArccos e

let make_arctan e =
  NArctan e

let make_ln e =
  NLn e