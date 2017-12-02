type expr = 
  |NIdent
  |NConst of string
  |NAdd of expr*expr
  |NMinus of expr*expr
  |NMult of expr*expr
  |NDiv of expr*expr
  |NPow of expr*expr
  |NSin of expr
  |NCos of expr
  |NTan of expr
  |NArcsin of expr
  |NArccos of expr
  |NArctan of expr
  |NLog of expr