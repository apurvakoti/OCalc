(*TODO: ADD ACKNOWLEDGMENTS*)

%{
  open Ast
  open Ast_maker
%}

(*DECLARING TOKENS*)
%token <string> CONST
%token IDENT
(*simple binary operators*)
%token PLUS MINUS DIV TIMES
(*exponentiation*)
%token POW LOG
(*trig funcs*)
%token SIN COS TAN ARCSIN ARCCOS ARCTAN
(*elementary constants*)
%token PI NATEXP

(*I assume this defines association for the below operators - could be wrong*)
%left PLUS MINUS TIMES DIV


%start <Ast.expr> parse_expression

%%

parse_expression:
  | e = expr; EOF 
        { e }


expr:
  | c = CONST 
        { make_const c }
  | IDENT
        { make_identity () }
  | e1 = expr; PLUS; e2 = expr
        { make_add e1 e2 }
  | MINUS; e1 = expr (*HANDLE UNARY NEGATION like in A4*)
        { make_minus (make_const "0") e1 } (*basically turn -e into 0-e*)
  | e1 = expr; MINUS; e2 = expr
        { make_minus e1 e2 }
  | e1 = expr; TIMES; e2 = expr
        { make_times e1 e2 }
  | e1 = expr; DIV; e2 = expr
        { make_div e1 e2 }
  | POW; e1 = expr; e2 = expr
        { make_pow e1 e2 }
  | 