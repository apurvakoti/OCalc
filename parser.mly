%{
  open Ast
  open Ast_maker
%}


%token <string> CONST
%token IDENT

%token PLUS MINUS DIV TIMES

%token POW LOG

%token SIN COS TAN ARCSIN ARCCOS ARCTAN

%token PI NATEXP

%token EOF


%token LPAREN RPAREN COMMA CARAT

%left PLUS MINUS TIMES DIV POW


%start <Ast.expr> parse_expression


%%

parse_expression:
  | e = expr; EOF
        { e }


expr:
  | c = CONST 
        { make_const c }
  | LPAREN; e = expr; RPAREN 
        { e }
  | IDENT
        { make_identity () }
  | e1 = expr; PLUS; e2 = expr
        { make_add e1 e2 }
  | MINUS; e1 = expr 
        { make_minus (make_const "0") e1 } 
  | e1 = expr; MINUS; e2 = expr
        { make_minus e1 e2 }
  | e1 = expr; TIMES; e2 = expr
        { make_mult e1 e2 }
  | e1 = expr; DIV; e2 = expr
        { make_div e1 e2 }
  | POW; LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN
        { make_pow e1 e2 }
  | e1 = expr; CARAT; e2 = expr
        { make_pow e1 e2 }
  | SIN; LPAREN; e = expr; RPAREN
        { make_sin e }
  | COS; LPAREN; e = expr; RPAREN
        { make_cos e }
  | TAN; LPAREN; e = expr; RPAREN
        { make_tan e }
  | ARCSIN; LPAREN; e = expr; RPAREN
        { make_arcsin e }
  | ARCCOS; LPAREN; e = expr; RPAREN
        { make_arccos e }
  | ARCTAN; LPAREN; e = expr; RPAREN
        { make_arctan e }
  | LOG; LPAREN; e = expr; RPAREN
        { make_log e }
  | LPAREN; e = expr; LPAREN
        { e }
  | PI
        { make_pi () }
  | NATEXP 
        { make_e () }
  ;