(*TODO: ADD ACKNOWLEDGMENTS*)

%{
  open Ast
  open Ast_maker
%}

%token <float> CONST
%token IDENT
(*simple binary operators*)
%token PLUS MINUS DIV TIMES
(*exponentiation*)
%token POW LOG
(*trig funcs*)
%token SIN COS TAN ARCSIN ARCCOS ARCTAN
(*elementary constants*)
%token PI NATEXP